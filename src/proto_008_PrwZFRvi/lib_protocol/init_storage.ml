(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Logging

(* Invoice a contract at given address with a given amount. Returns updated
   context and a  balance update receipt (singleton list). The address must be
   a valid base58 hash and its contract must be allocated, otherwise this is
   no-op and returns empty receipts list. *)
let invoice_contract :
    Raw_context.t ->
    address:string ->
    amount:Tez_repr.t ->
    (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t =
 fun ctxt ~address ~amount ->
  match Contract_repr.of_b58check address with
  | Ok recipient -> (
      Contract_storage.allocated ctxt recipient
      >>=? function
      | false ->
          return (ctxt, [])
      | true ->
          Contract_storage.credit ctxt recipient amount
          >>=? fun ctxt ->
          return (ctxt, Receipt_repr.[(Migration recipient, Credited amount)])
      )
  | Error _ ->
      return (ctxt, [])

type migrate_stats = {
  contract_delegated : int ref;
  contract_frozen_deposits : int ref;
  contract_frozen_fees : int ref;
  contract_frozen_rewards : int ref;
}

let migrate_baker :
    migrate_stats ->
    Raw_context.t ->
    Signature.Public_key_hash.t ->
    Raw_context.t tzresult Lwt.t =
 fun stats ctxt baker_pkh ->
  Contract_storage.get_public_key
    ctxt
    (Contract_repr.implicit_contract baker_pkh)
  >>=? fun pk ->
  let storage = Baker_script_repr.storage ~threshold:1 ~owner_keys:[pk] in
  Baker_storage.fresh_baker_from_current_nonce ctxt
  >>=? fun (ctxt, baker_hash) ->
  let from = baker_pkh in
  let to_ = baker_hash in
  let from_contract = Contract_repr.implicit_contract from in
  let to_contract = Contract_repr.baker_contract to_ in
  Storage.Baker.Registered.add ctxt baker_hash
  >>= fun ctxt ->
  Storage.Contract.Storage.init
    ctxt
    to_contract
    (Script_repr.lazy_expr storage)
  >>=? fun (ctxt, storage_size) ->
  let total_size = Z.of_int storage_size in
  Storage.Contract.Paid_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Used_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Global_counter.get ctxt
  >>=? fun counter ->
  Storage.Contract.Counter.init ctxt to_contract counter
  >>=? fun ctxt ->
  Storage.Baker.Consensus_key.init ctxt baker_hash pk
  >>=? fun ctxt ->
  let pkh = Signature.Public_key.hash pk in
  Storage.Baker.Consensus_key_rev.init ctxt pkh baker_hash
  >>=? fun ctxt ->
  Storage.Delegates_006.del ctxt baker_pkh
  >>= fun ctxt ->
  (* Migrate storages with original baker pkh as a key *)
  Storage.Contract.Balance.get ctxt from_contract
  >>=? fun balance ->
  Storage.Contract.Balance.init ctxt to_contract balance
  >>=? fun ctxt ->
  Storage.Contract.Balance.remove ctxt from_contract
  >>= fun ctxt ->
  (* Roll.Delegate_roll_list -> Roll.Baker_roll_list *)
  Storage.Roll.Delegate_roll_list_006.get_option ctxt from
  >>=? (function
         | None ->
             return ctxt
         | Some roll ->
             Storage.Roll.Delegate_roll_list_006.delete ctxt from
             >>=? fun ctxt -> Storage.Roll.Baker_roll_list.init ctxt to_ roll)
  >>=? fun ctxt ->
  (* Roll.Delegate_change -> Roll.Baker_change *)
  Storage.Roll.Delegate_change_006.get_option ctxt from
  >>=? (function
         | None ->
             return ctxt
         | Some change ->
             Storage.Roll.Delegate_change_006.delete ctxt from
             >>=? fun ctxt -> Storage.Roll.Baker_change.init ctxt to_ change)
  >>=? fun ctxt ->
  (* Contract.Delegated -> Baker.Delegators *)
  Storage.Contract.Delegated_006.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun contract acc ->
      incr stats.contract_delegated ;
      Lwt.return acc
      >>=? fun ctxt ->
      ( match Contract_repr.is_implicit contract with
      | None ->
          return_some contract
      | Some pkh ->
          (* if it's self-delegation, remove the association *)
          if Signature.Public_key_hash.equal pkh from then return_none
          else return_some contract )
      >>=? function
      | Some contract ->
          Storage.Baker.Delegators.add (ctxt, to_) contract
          >>= fun ctxt -> return ctxt
      | None ->
          return ctxt)
  >>=? fun ctxt ->
  Storage.Contract.Delegated_006.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_deposits -> Baker.Frozen_deposits *)
  Storage.Contract.Frozen_deposits_006.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle deposit acc ->
      incr stats.contract_frozen_deposits ;
      Lwt.return acc
      >>=? fun ctxt ->
      Storage.Baker.Frozen_deposits.init (ctxt, to_) cycle deposit)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_deposits_006.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_fees -> Baker.Frozen_fees *)
  Storage.Contract.Frozen_fees_006.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle fee acc ->
      incr stats.contract_frozen_fees ;
      Lwt.return acc
      >>=? fun ctxt -> Storage.Baker.Frozen_fees.init (ctxt, to_) cycle fee)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_fees_006.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_rewards -> Baker.Frozen_rewards *)
  Storage.Contract.Frozen_rewards_006.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle rewards acc ->
      incr stats.contract_frozen_rewards ;
      Lwt.return acc
      >>=? fun ctxt ->
      Storage.Baker.Frozen_rewards.init (ctxt, to_) cycle rewards)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_rewards_006.clear (ctxt, from_contract)
  >>= fun ctxt -> return ctxt

(* This is the genesis protocol: initialise the state *)
let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block ~level ~timestamp ~fitness ctxt
  >>=? fun (previous_protocol, ctxt, prev_blocks_per_voting_period) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments
      >>=? fun ctxt ->
      Roll_storage.init ctxt
      >>=? fun ctxt ->
      Seed_storage.init ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt
      >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
        param.bootstrap_bakers
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Baker_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0
      >>=? fun ctxt -> Vote_storage.update_listings ctxt
  | Delphi_007 ->
      (* 1. Voting period migration *)
      Storage.Vote.Current_period_kind_007.delete ctxt
      >>=? fun ctxt ->
      let level_position = (Level_storage.current ctxt).level_position in
      let voting_period_index =
        Int32.(div (succ level_position) prev_blocks_per_voting_period)
      in
      let start_position = level_position in
      Storage.Vote.Current_period.init
        ctxt
        {index = voting_period_index; kind = Proposal; start_position}
      >>=? fun ctxt ->
      Storage.Vote.Pred_period_kind.init ctxt Promotion_vote
      >>=? fun ctxt ->
      Storage.Sapling.Next.init ctxt
      >>=? fun ctxt ->
      (* 2. Baker accounts migration *)
      let nonce =
        Operation_hash.hash_bytes
          [ Bytes.of_string
              "Things will not calm down, Daniel Jackson. They will, in fact, \
               calm up." ]
      in
      let ctxt = Raw_context.init_origination_nonce ctxt nonce in
      let delegates = ref 0 in
      let migrate_stats =
        {
          contract_delegated = ref 0;
          contract_frozen_deposits = ref 0;
          contract_frozen_fees = ref 0;
          contract_frozen_rewards = ref 0;
        }
      in
      Storage.Delegates_006.fold ctxt ~init:(ok ctxt) ~f:(fun baker_pkh acc ->
          incr delegates ;
          Lwt.return acc
          >>=? fun ctxt -> migrate_baker migrate_stats ctxt baker_pkh)
      >>=? fun ctxt ->
      lwt_log_notice
        "migrated %d delegates (delegated=%d frozen_deposits=%d \
         frozen_fees=%d frozen_rewards=%d)"
        !delegates
        !(migrate_stats.contract_delegated)
        !(migrate_stats.contract_frozen_deposits)
        !(migrate_stats.contract_frozen_fees)
        !(migrate_stats.contract_frozen_rewards)
      >>= fun () ->
      Storage.Delegates_006.clear ctxt
      >>= fun ctxt ->
      (* Take a snapshot of the consensus keys *)
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      lwt_log_notice
        "snapshot consensus keys in cycle %a"
        Cycle_repr.pp
        current_cycle
      >>= fun () ->
      Storage.Baker.Consensus_key.snapshot ctxt current_cycle
      >>=? fun ctxt ->
      (* Migrate storages with references to original baker pkhs *)
      (* Contract.Delegate *)
      let contract_delegates = ref 0 in
      Storage.Contract.Delegate_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun contract pkh acc ->
          incr contract_delegates ;
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get_option ctxt pkh
          >>=? function
          | None ->
              (* NOTE There's one contract
                 "tz1gNQpjio7F5HsR9bs37vBYZXfbWpZ34tpL" that's not registered
                 as baker, but because of a bug in protocol 001 it has two
                 originated contracts delegated to it:
                  - KT1LLKuQLmxciFUif4yt6UyXFqEE3235emHE
                  - KT19MYkajshb4DnpDyNC37Qm7DCbPh5GP5Sg
                 We drop the delegations here.
              *)
              Storage.Contract.Delegate_006.delete ctxt contract
          | Some baker_hash -> (
              let migrate_delegate ctxt =
                (* Because the path has not changed, this will override 006
                   value *)
                Storage.Contract.Delegate.set ctxt contract baker_hash
              in
              match Contract_repr.is_implicit contract with
              | Some contract_pkh -> (
                  (* Check this the contract is a baker *)
                  Storage.Baker.Consensus_key_rev.get_option ctxt contract_pkh
                  >>=? function
                  | None ->
                      migrate_delegate ctxt
                  | Some _ ->
                      (* Baker contracts are no longer self-delegated, so we
                         don't migrate this relation, instead we delete it *)
                      Storage.Contract.Delegate_006.delete ctxt contract )
              | None ->
                  migrate_delegate ctxt ))
      >>=? fun ctxt ->
      lwt_log_notice "moving %d contract delegates" !contract_delegates
      >>= fun () ->
      (* Active_delegates_with_rolls -> Baker.Active_with_rolls *)
      Storage.Active_delegates_with_rolls_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Baker.Active_with_rolls.add ctxt baker
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      Storage.Active_delegates_with_rolls_006.clear ctxt
      >>= fun ctxt ->
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      let preserved = Constants_storage.preserved_cycles ctxt in
      (* Delegates_with_frozen_balance -> Baker.With_frozen_balance *)
      let rec migrate_frozen_balance ctxt cycle =
        lwt_log_notice
          "migrating bakers with frozen balance for cycle %a"
          Cycle_repr.pp
          cycle
        >>= fun () ->
        Storage.Delegates_with_frozen_balance_006.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun pkh acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            Storage.Baker.Consensus_key_rev.get ctxt pkh
            >>=? fun baker ->
            Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker
            >>= fun ctxt -> return ctxt)
        >>=? fun ctxt ->
        Storage.Delegates_with_frozen_balance_006.clear (ctxt, cycle)
        >>= fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            if Cycle_repr.(current_cycle = add prev_cycle (preserved + 1)) then
              return ctxt
            else migrate_frozen_balance ctxt prev_cycle
      in
      migrate_frozen_balance ctxt current_cycle
      >>=? fun ctxt ->
      let contract_inactive_delegate = ref 0 in
      (* Contract.Inactive_delegate -> Baker.Inactive *)
      Storage.Contract.Inactive_delegate_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun inactive_baker acc ->
          incr contract_inactive_delegate ;
          Lwt.return acc
          >>=? fun ctxt ->
          match Contract_repr.is_implicit inactive_baker with
          | None ->
              let message =
                Format.asprintf
                  "inactive baker `Contract.Inactive_delegate_006` \"%a\" is \
                   not implicit account"
                  Contract_repr.pp
                  inactive_baker
              in
              failwith message
          | Some baker ->
              Storage.Baker.Consensus_key_rev.get ctxt baker
              >>=? fun migrated_baker ->
              Storage.Baker.Inactive.add ctxt migrated_baker
              >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      lwt_log_notice
        "migrated %d contract inactive delegates"
        !contract_inactive_delegate
      >>= fun () ->
      Storage.Contract.Inactive_delegate_006.clear ctxt
      >>= fun ctxt ->
      (* Contract.Delegate_desactivation -> Baker.Inactive *)
      Storage.Contract.Delegate_desactivation_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun baker cycle acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          match Contract_repr.is_implicit baker with
          | None ->
              let message =
                Format.asprintf
                  "inactive baker `Contract.Delegate_desactivation_006` \
                   \"%a\" is not implicit account"
                  Contract_repr.pp
                  baker
              in
              failwith message
          | Some baker ->
              Storage.Baker.Consensus_key_rev.get ctxt baker
              >>=? fun migrated_baker ->
              Storage.Baker.Deactivation.init ctxt migrated_baker cycle)
      >>=? fun ctxt ->
      Storage.Contract.Delegate_desactivation_006.clear ctxt
      >>= fun ctxt ->
      (* Roll.Owner *)
      let roll_owners = ref 0 in
      Storage.Roll.Owner_006.fold ctxt ~init:(ok ctxt) ~f:(fun roll pk acc ->
          incr roll_owners ;
          Lwt.return acc
          >>=? fun ctxt ->
          let pkh = Signature.Public_key.hash pk in
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          (* because the path has not changed, this will override 006 value *)
          Storage.Roll.Owner.set ctxt roll baker >>=? fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d roll owners" !roll_owners
      >>= fun () ->
      let roll_owner_shapshot = ref 0 in
      Storage.Roll.Owner_006.Snapshot.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun key pk acc ->
          incr roll_owner_shapshot ;
          ( if Compare.Int.(!roll_owner_shapshot mod 500_000 = 0) then
            lwt_log_notice
              "migrating %d roll owner snapshot"
              !roll_owner_shapshot
          else Lwt.return () )
          >>= fun () ->
          Lwt.return acc
          >>=? fun ctxt ->
          let pkh = Signature.Public_key.hash pk in
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          (* because the path has not changed, this will override 006 value *)
          Storage.Roll.Owner.Snapshot.set ctxt key baker
          >>=? fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d roll owner snapshot" !roll_owner_shapshot
      >>= fun () ->
      (* Vote.Listings *)
      let vote_listings = ref 0 in
      Storage.Vote.Listings_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh listings acc ->
          incr vote_listings ;
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Listings_006.delete ctxt pkh
          >>=? fun ctxt -> Storage.Vote.Listings.init ctxt baker listings)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d vote listings" !vote_listings
      >>= fun () ->
      (* Vote.Proposals *)
      let vote_proposals = ref 0 in
      Storage.Vote.Proposals_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun (proposal, pkh) acc ->
          incr vote_proposals ;
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Proposals_006.del ctxt (proposal, pkh)
          >>= fun ctxt ->
          Storage.Vote.Proposals.add ctxt (proposal, baker)
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d vote proposals" !vote_proposals
      >>= fun () ->
      (* Vote.Proposals_count *)
      let vote_proposals_count = ref 0 in
      Storage.Vote.Proposals_count_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh count acc ->
          incr vote_proposals_count ;
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Proposals_count_006.delete ctxt pkh
          >>=? fun ctxt -> Storage.Vote.Proposals_count.init ctxt baker count)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d vote proposals count" !vote_proposals_count
      >>= fun () ->
      (* Vote.Ballots *)
      let vote_ballots = ref 0 in
      Storage.Vote.Ballots_006.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh ballot acc ->
          incr vote_ballots ;
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Ballots_006.delete ctxt pkh
          >>=? fun ctxt -> Storage.Vote.Ballots.init ctxt baker ballot)
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d vote ballots" !vote_ballots
      >>= fun () ->
      (* Storage.Seed.Nonce *)
      let storage_seed_nonce = ref 0 in
      let rec migrate_cycle_nonce ctxt cycle =
        Storage.Cycle_006.Nonce.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun level nonce acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            ( match nonce with
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                incr storage_seed_nonce ;
                Storage.Baker.Consensus_key_rev.get ctxt delegate
                >>=? fun baker ->
                return
                  (Storage.Cycle.Unrevealed {nonce_hash; baker; rewards; fees})
            | Revealed s ->
                return (Storage.Cycle.Revealed s) )
            >>=? fun nonce ->
            (* because the path has not changed, this will override 006 value *)
            Storage.Cycle.Nonce.set (ctxt, cycle) level nonce
            >>=? fun ctxt -> return ctxt)
        >>=? fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            migrate_cycle_nonce ctxt prev_cycle
      in
      migrate_cycle_nonce ctxt current_cycle
      >>=? fun ctxt ->
      lwt_log_notice "migrated %d unrevealed nonce" !storage_seed_nonce
      >>= fun () ->
      (* 3. balance update receipts for baker migration *)
      let baker_consensus_keys = ref 0 in
      Storage.Baker.Consensus_key_rev.fold
        ctxt
        ~init:(ok [])
        ~f:(fun pkh baker_hash acc ->
          incr baker_consensus_keys ;
          Lwt.return acc
          >>=? fun updates ->
          let original_contract = Contract_repr.implicit_contract pkh in
          let new_contract = Contract_repr.baker_contract baker_hash in
          Storage.Contract.Balance.get ctxt new_contract
          >>=? fun moved_balance ->
          return
          @@ Receipt_repr.(Migration original_contract, Debited moved_balance)
             :: Receipt_repr.(Migration new_contract, Credited moved_balance)
             :: updates)
      >>=? fun baker_balance_updates ->
      lwt_log_notice "migrated %d baker consensus keys" !baker_consensus_keys
      >>= fun () ->
      (* 4. protocol upgrade invoice *)
      Storage.Baker.Consensus_key_rev.get_option
        ctxt
        (Signature.Public_key_hash.of_b58check_exn
           "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
      >>=? (function
             | None ->
                 return (ctxt, [])
             | Some migrated_bootstrap1 ->
                 let amount = Tez_repr.of_mutez_exn 662_607_015L in
                 invoice_contract
                   ctxt
                   ~address:
                     (* NOTE: change this address (sandbox 'bootstrap1') *)
                     (Baker_hash.to_b58check migrated_bootstrap1)
                   ~amount)
      >>=? fun (ctxt, invoice_balance_updates) ->
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [[prepare]] function below *)
      Storage.Pending_migration_balance_updates.init
        ctxt
        (invoice_balance_updates @ baker_balance_updates)

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  Storage.Pending_migration_balance_updates.get_option ctxt
  >>=? function
  | Some balance_updates ->
      Storage.Pending_migration_balance_updates.remove ctxt
      >>= fun ctxt ->
      (* When applying balance updates in a migration, we must attach receipts.
         The balance updates returned from here will be applied in the first
         block of the new protocol. *)
      return (ctxt, balance_updates)
  | None ->
      return (ctxt, [])
