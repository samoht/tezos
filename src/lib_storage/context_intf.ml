(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
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

module type S = sig
  (** The type for database views. *)
  type t

  (** The type for database keys. *)
  type key = string list

  (** The type for database values. *)
  type value = bytes

  (** The type for database trees. *)
  type tree

  (** {2 Getters} *)

  (** [mem t k] is true iff [k] is bound to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [find t k] is [Some v] if [k] is bound to [v] in [t] and [None]
     otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [find_tree] is like {!find} but for trees. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** {2 Setters} *)

  (** [add t k v] is the database view where [k] is bound to [v] and
     is similar to [t] for other keys. *)
  val add : t -> key -> value -> t Lwt.t

  (** [add_tree] is like {!add} but for trees. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** [remove c k] removes any values and trees bound to [k] in [c]. *)
  val remove : t -> key -> t Lwt.t

  (** [fold ?depth t root ~init ~value ~treee] Recursive fold over [t]
      trees and values. The [value] and [tree] callbacks are called
      with keys relative to [root]. [value] is never called with an
      empty key, e.g. folding over the value is a no-op.

      If [depth] is set (by default it is not), then [value] and [tree]
      are only called with relative keys of size [depth].  *)
  val fold :
    ?depth:int ->
    t ->
    key ->
    init:'a ->
    value:(key -> value -> 'a -> 'a Lwt.t) ->
    tree:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  module Tree : sig
    val empty : t -> tree

    val is_empty : tree -> bool

    val mem : tree -> key -> bool Lwt.t

    val mem_tree : tree -> key -> bool Lwt.t

    val find : tree -> key -> value option Lwt.t

    val get : tree -> key -> value Lwt.t

    val add : tree -> key -> value -> tree Lwt.t

    val remove : tree -> key -> tree Lwt.t

    val find_tree : tree -> key -> tree option Lwt.t

    val get_tree : tree -> key -> tree Lwt.t

    val add_tree : tree -> key -> tree -> tree Lwt.t

    val fold :
      ?depth:int ->
      tree ->
      key ->
      init:'a ->
      value:(key -> value -> 'a -> 'a Lwt.t) ->
      tree:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t
  end
end
