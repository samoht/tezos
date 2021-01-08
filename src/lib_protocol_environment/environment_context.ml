(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Error_monad

module type CONTEXT = Environment_context_intf.S

module Context = struct
  type key = string list

  type value = Bytes.t

  type 'ctxt ops = (module CONTEXT with type t = 'ctxt)

  type _ kind = ..

  type t = Context : {kind : 'a kind; ctxt : 'a; ops : 'a ops} -> t

  let mem (Context {ops = (module Ops); ctxt; _}) key = Ops.mem ctxt key

  let add (Context {ops = (module Ops) as ops; ctxt; kind}) key value =
    Ops.add ctxt key value
    >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  let mem_tree (Context {ops = (module Ops); ctxt; _}) key =
    Ops.mem_tree ctxt key

  let find (Context {ops = (module Ops); ctxt; _}) key = Ops.find ctxt key

  let copy (Context {ops = (module Ops) as ops; ctxt; kind}) ~from ~to_ =
    Ops.copy ctxt ~from ~to_
    >>= function
    | Some ctxt ->
        Lwt.return_some (Context {ops; ctxt; kind})
    | None ->
        Lwt.return_none

  let remove (Context {ops = (module Ops) as ops; ctxt; kind}) key =
    Ops.remove ctxt key >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  type key_or_dir = [`Key of key | `Dir of key]

  let current_protocol_key = ["protocol"]

  let fold (Context {ops = (module Ops); ctxt; _}) key ~init ~f =
    Ops.fold ctxt key ~init ~f

  let set_protocol (Context ({ops = (module Ops); ctxt; _} as c)) protocol_hash
      =
    Ops.add ctxt current_protocol_key (Protocol_hash.to_bytes protocol_hash)
    >>= fun ctxt -> Lwt.return (Context {c with ctxt})

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}
