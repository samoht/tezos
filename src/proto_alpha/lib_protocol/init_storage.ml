(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0
      >>=? fun ctxt -> Vote_storage.update_listings ctxt
  | Delphi_007 ->
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
      let amount = Tez_repr.of_mutez_exn 662_607_015L in
      invoice_contract
        ctxt
        ~address:
          (* NOTE: change this address (sandbox 'bootstrap1') *)
          "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
        ~amount
      >>=? fun (ctxt, balance_updates) ->
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [[prepare]] function below *)
      Storage.Pending_migration_balance_updates.init ctxt balance_updates

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
