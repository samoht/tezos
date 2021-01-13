(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

(** {2 Generic interface} *)

module type S = sig
  (** A (key x value) store for a given block. *)
  type t

  (** The type for context keys. *)
  type key = string list

  (** The type for context values. *)
  type value = bytes

  (** [mem t k] is an Lwt promise that resolves to true iff [k] is bound
   to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [find t k] is an Lwt promise that resolves to [v] if [Some k] is
    bound to the value [v] in [t] and [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [add t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is bound to [v] in [c];
    - and [c] is similar to [t] otherwise. *)
  val add : t -> key -> value -> t Lwt.t

  (** [remove t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is unbound in [c];
    - and [c] is similar to [t] otherwise. *)
  val remove : t -> key -> t Lwt.t

  (** [copy] returns None if the [from] key is not bound *)
  val copy : t -> from:key -> to_:key -> t option Lwt.t

  type key_or_dir = [`Key of key | `Dir of key]

  (** [fold] iterates over elements under a path (not recursive). Iteration order
    is nondeterministic. *)
  val fold :
    t -> key -> init:'a -> f:(key_or_dir -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end
