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

module type CONTEXT = sig
  type t

  type key = string list

  type value = Bytes.t

  val mem : t -> key -> bool Lwt.t

  val dir_mem : t -> key -> bool Lwt.t

  val get : t -> key -> value option Lwt.t

  val set : t -> key -> value -> t Lwt.t

  val copy : t -> from:key -> to_:key -> t option Lwt.t

  val remove_rec : t -> key -> t Lwt.t

  type key_or_dir = [`Key of key | `Dir of key]

  val fold :
    t -> key -> init:'a -> f:(key_or_dir -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  val set_protocol : t -> Protocol_hash.t -> t Lwt.t

  val fork_test_chain :
    t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t

  type cursor

  val get_cursor : t -> key -> cursor Lwt.t

  val set_cursor : t -> key -> cursor -> t Lwt.t

  val fold_rec :
    ?depth:int ->
    t ->
    key ->
    init:'a ->
    f:(key -> cursor -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  module Cursor : sig
    val empty : t -> cursor

    val get : cursor -> key -> value option Lwt.t

    val set : cursor -> key -> value -> cursor Lwt.t

    val get_cursor : cursor -> key -> cursor Lwt.t

    val set_cursor : cursor -> key -> cursor -> cursor Lwt.t
  end
end

module Context : sig
  type ('ctxt, 'cursor) ops =
    (module CONTEXT with type t = 'ctxt and type cursor = 'cursor)

  type _ kind = ..

  type ('a, 'b) witness

  val witness : unit -> ('a, 'b) witness

  type t =
    | Context : {
        kind : 'a kind;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        wit : ('a, 'b) witness;
      }
        -> t

  include CONTEXT with type t := t
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
