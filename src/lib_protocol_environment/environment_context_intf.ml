(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Tarides <contact@tarides.com>                          *)
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

(* Same as [lib_storage/context_intf.ml]. *)

module type VIEW = sig
  (** The type for context views. *)
  type t

  (** The type for context keys. *)
  type key := string list

  (** The type for database values. *)
  type value := Bytes.t

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

  (** {2 Fold} *)

  (** [fold ?depth t root ~init ~value ~tree] Recursive fold over [t]
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
end

module type S = sig
  include VIEW

  module Tree : sig
    (** [empty t] is the empty tree. [t] is only needed to please the
        typing gods. *)
    val empty : t -> tree

    (** [is_empty t] is true iff [t] is [empty _]. *)
    val is_empty : tree -> bool

    include
      VIEW
        with type t := tree
         and type tree := tree
         and type key := key
         and type value := value
  end
end
