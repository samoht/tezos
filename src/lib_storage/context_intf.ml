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

  (** [mem t k] is true iff [k] is bound to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [find t k] is [Some v] if [k] is bound to [v] in [t] and [None]
     otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [get t k] is similar to {!find} but raise [Not_found] if [k] is
     unbound in [t]. *)
  val get : t -> key -> value Lwt.t

  (** [add t k v] is the database view where [k] is bound to [v] and
     is similar to [t] for other keys. *)
  val add : t -> key -> value -> t Lwt.t

  (** [remove_rec c k] removes any values and trees bound to [k] in [c]. *)
  val remove_rec : t -> key -> t Lwt.t

  (** {2 Trees} *)

  (** The type for database trees. *)
  type tree

  (** [mem_tree t k] is true iff [k] is bound to a tree in [t]. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [find_tree c k] is [Some v] if [v] is the tree bound to [k] in
     [t]. If [k] is unbound it is [None]. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** [get_tree t k] is similar to {!find} but raise [Not_found] if
     [k] is unbound. *)
  val get_tree : t -> key -> tree Lwt.t

  (** [add_tree t k v] binds the tree [v] to the key [k] in [t]. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** Recursive fold *)
  val fold :
    ?depth:int ->
    t ->
    key ->
    init:'a ->
    value:(key -> value -> 'a -> 'a Lwt.t) ->
    tree:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  module Tree : sig
    val equal : tree -> tree -> bool

    val hash : tree -> Context_hash.t

    val empty : tree

    val mem : tree -> key -> bool Lwt.t

    val mem_tree : tree -> key -> bool Lwt.t

    val find : tree -> key -> value option Lwt.t

    val get : tree -> key -> value Lwt.t

    val add : tree -> key -> value -> tree Lwt.t

    val remove_rec : tree -> key -> tree Lwt.t

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
