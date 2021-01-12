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

(** All context manipulation functions. This signature is included
    as-is for direct context accesses, and used in {!Storage_functors}
    to provide restricted views to the context. *)

module type VIEW = sig
  (* Same as [Environment_context.VIEW] but with extra getters and
     setters functions. *)

  (** The type for context handler. *)
  type t

  (** The type for context trees. *)
  type tree

  (** The type for context keys. *)
  type key = string list

  (** The type for context values. *)
  type value = bytes

  (** {2 Getters} *)

  (** [mem t k] is true iff [k] is bound to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [get t k] is [v] if [Ok k] is bound to the value [v] in [t] and
      {!Storage_Error Missing_key} otherwise. *)
  val get : t -> key -> value tzresult Lwt.t

  (** [get_tree] is like {!get} but for trees. *)
  val get_tree : t -> key -> tree tzresult Lwt.t

  (** [find t k] is [v] if [Some k] is bound to the value [v] in [t] and
      [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [find_tree] is like {!find} but for trees. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** {2 Setters} *)

  (** [init t k v] is [Ok c] if:

      - [k] is unbound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Existing_key} if [k] is already bound in [t]. *)
  val init : t -> key -> value -> t tzresult Lwt.t

  (** [init_tree] is like {!init} but for trees. *)
  val init_tree : t -> key -> tree -> t tzresult Lwt.t

  (** [update t k v] is [Ok c] if:

      - [k] is bound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Missing_key} if [k] is not already bound in [t]. *)
  val update : t -> key -> value -> t tzresult Lwt.t

  (** [update] is like {!update} but for trees. *)
  val update_tree : t -> key -> tree -> t tzresult Lwt.t

  (** [add t k v] is [c] such that:

      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise. *)
  val add : t -> key -> value -> t Lwt.t

  (** [add_tree] is like {!add} but for trees. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** [remove t k v] is [c] such that:

      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.

      [remove] will remove any values and trees bound to [k] in [t]. *)
  val remove : t -> key -> t Lwt.t

  (** [remove t k v] is [Ok c] if:

      - [k] is bound in [t] to a value;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing : t -> key -> t tzresult Lwt.t

  (** [remove t k v] is [Ok c] if:

      - [k] is bound in [t] to a tree;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing_tree : t -> key -> t tzresult Lwt.t

  (** [add_or_remove t k v] is:

      - [add t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove : t -> key -> value option -> t Lwt.t

  (** [add_or_remove_tree t k v] is:

      - [add_tree t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove_tree : t -> key -> tree option -> t Lwt.t

  (** {2 Folds} *)

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
end

module type T = sig
  (** The type for root contexts. *)
  type root

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

  (** Internally used in {!Storage_functors} to escape from a view. *)
  val project : t -> root

  (** Internally used in {!Storage_functors} to retrieve a full key
      from partial key relative a view. *)
  val absolute_key : t -> key -> key

  (** Raised if block gas quota is exhausted during gas
     consumption. *)
  type error += Block_quota_exceeded

  (** Raised if operation gas quota is exhausted during gas
     consumption. *)
  type error += Operation_quota_exceeded

  (** Internally used in {!Storage_functors} to consume gas from
     within a view. May raise {!Block_quota_exceeded} or
     {!Operation_quota_exceeded}. *)
  val consume_gas : t -> Gas_limit_repr.cost -> t tzresult

  (** Check if consume_gas will fail *)
  val check_enough_gas : t -> Gas_limit_repr.cost -> unit tzresult

  val description : t Storage_description.t
end
