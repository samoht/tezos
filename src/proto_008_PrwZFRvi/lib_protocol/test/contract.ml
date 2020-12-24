(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Test_tez

let check_empty_contract_counter ~loc b contract =
  Context.Contract.counter (B b) contract
  >>= function
  | Error
      (Environment.Ecoproto_error
         (Contract_storage.Empty_implicit_contract _pkh)
      :: _) ->
      return_unit
  | _ ->
      failwith "Unexpected result at %s" loc

(** Check the state of an unregistered contract *)
let test_unregistered_implicit_contract () =
  Context.init 1
  >>=? fun (b, _, _bakers) ->
  let unregistered_pkh = (Account.new_account ()).pkh in
  let unregistered_contract = Contract.implicit_contract unregistered_pkh in
  Context.Contract.info (B b) unregistered_contract
  >>=? fun {balance; delegate; counter; script} ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ delegate
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ counter
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ script
  >>=? fun () ->
  check_empty_contract_counter ~loc:__LOC__ b unregistered_contract

(** Check the state of a contract after it's been allocated, emptied and
    re-allocated. *)
let test_emptied_implicit_contract () =
  Context.init 1
  >>=? fun (b, contracts, _bakers) ->
  let bootstrap = List.hd contracts in
  let new_contract = Contract.implicit_contract (Account.new_account ()).pkh in
  (* Send some tezzies to a new contract to allocate it first *)
  Incremental.begin_construction b
  >>=? fun i ->
  Op.transaction (I i) bootstrap new_contract Tez.one
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Incremental.finalize_block i
  >>=? fun b ->
  Context.Contract.info (I i) new_contract
  >>=? fun {balance; delegate; counter; script} ->
  Context.Contract.global_counter (B b)
  >>=? fun global_counter ->
  Assert.equal_tez ~loc:__LOC__ Tez.one balance
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ delegate
  >>=? fun () ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "counter is initialized to the global counter value"
    Format.(pp_print_option Z.pp_print)
    counter
    (Some global_counter)
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ script
  >>=? fun () ->
  Context.Contract.counter (B b) new_contract
  >>=? fun counter ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "counter is initialized to the global counter value"
    Z.pp_print
    counter
    global_counter
  (* Empty the new contract *)
  >>=? fun () ->
  Incremental.begin_construction b
  >>=? fun i ->
  Op.transaction (I i) ~fee:Tez.zero new_contract bootstrap Tez.one
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Incremental.finalize_block i
  >>=? fun b ->
  Context.Contract.info (B b) new_contract
  >>=? fun {balance; delegate; counter; script} ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ delegate
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ counter
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ script
  >>=? fun () ->
  check_empty_contract_counter ~loc:__LOC__ b new_contract
  >>=? fun () ->
  (* Send tezzies to the contract again to re-allocate it *)
  Incremental.begin_construction b
  >>=? fun i ->
  Op.transaction (I i) bootstrap new_contract Tez.one
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Incremental.finalize_block i
  >>=? fun b ->
  Context.Contract.info (B b) new_contract
  >>=? fun {balance; delegate; counter; script} ->
  Context.Contract.global_counter (B b)
  >>=? fun global_counter ->
  Assert.equal_tez ~loc:__LOC__ Tez.one balance
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ delegate
  >>=? fun () ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "counter is initialized to the global counter value"
    Format.(pp_print_option Z.pp_print)
    counter
    (Some global_counter)
  >>=? fun () ->
  Assert.is_none ~loc:__LOC__ script
  >>=? fun () ->
  Context.Contract.counter (B b) new_contract
  >>=? fun counter ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "counter is initialized to the global counter value"
    Z.pp_print
    counter
    global_counter

let tests =
  [ Test.tztest
      "unregistered implicit contract"
      `Quick
      test_unregistered_implicit_contract;
    Test.tztest
      "emptied implicit contract info"
      `Quick
      test_emptied_implicit_contract ]
