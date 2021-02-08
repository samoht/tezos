(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

open Tezos_storage_encoding.Context

module type DB =
  Irmin.S
    with type key = Path.t
     and type contents = Contents.t
     and type branch = Branch.t
     and type hash = Hash.t
     and type step = Path.step
     and type metadata = Metadata.t
     and type Key.step = Path.step

module Make_tree (Store : DB) = struct
  include Store.Tree

  let empty _ = Store.Tree.empty

  let equal = Irmin.Type.(unstage (equal Store.tree_t))

  let is_empty t = equal Store.Tree.empty t

  let hash t = Hash.to_context_hash (Store.Tree.hash t)

  let add t k v = Store.Tree.add t k v

  let kind t =
    match Store.Tree.destruct t with `Contents _ -> `Value | `Node _ -> `Tree

  let to_value t =
    match Store.Tree.destruct t with
    | `Contents (c, _) ->
        Lwt.return (Some c)
    | `Node _ ->
        Lwt.return_none

  let of_value _ v = Store.Tree.add Store.Tree.empty [] v

  let fold ?depth t k ~init ~f =
    find_tree t k
    >>= function
    | None ->
        Lwt.return init
    | Some t ->
        Store.Tree.fold
          ?depth
          ~force:`And_clear
          ~uniq:`False
          ~node:(fun k v acc -> f k (Store.Tree.of_node v) acc)
          ~contents:(fun k v acc ->
            if k = [] then Lwt.return acc
            else f k (Store.Tree.of_contents v) acc)
          t
          init

  type raw = [`Value of bytes | `Tree of raw TzString.Map.t]

  type concrete = Store.Tree.concrete

  let rec raw_of_concrete : type a. (raw -> a) -> concrete -> a =
   fun k -> function
    | `Tree l ->
        raw_of_node (fun l -> k (`Tree (TzString.Map.of_seq l))) l
    | `Contents (v, _) ->
        k (`Value v)

  and raw_of_node :
      type a. ((string * raw) Seq.t -> a) -> (string * concrete) list -> a =
   fun k -> function
    | [] ->
        k Seq.empty
    | (n, v) :: t ->
        raw_of_concrete
          (fun v ->
            raw_of_node (fun t -> k (fun () -> Seq.Cons ((n, v), t))) t)
          v

  let to_raw t = Store.Tree.to_concrete t >|= raw_of_concrete (fun t -> t)

  let rec concrete_of_raw : type a. (concrete -> a) -> raw -> a =
   fun k -> function
    | `Tree l ->
        concrete_of_node (fun l -> k (`Tree l)) (TzString.Map.to_seq l)
    | `Value v ->
        k (`Contents (v, ()))

  and concrete_of_node :
      type a. ((string * concrete) list -> a) -> (string * raw) Seq.t -> a =
   fun k seq ->
    match seq () with
    | Nil ->
        k []
    | Cons ((n, v), t) ->
        concrete_of_raw
          (fun v -> concrete_of_node (fun t -> k ((n, v) :: t)) t)
          v

  let of_raw = concrete_of_raw Store.Tree.of_concrete

  let raw_encoding : raw Data_encoding.t =
    let open Data_encoding in
    mu "Tree.raw" (fun encoding ->
        let map_encoding =
          conv
            TzString.Map.bindings
            (fun bindings -> TzString.Map.of_seq (List.to_seq bindings))
            (list (tup2 string encoding))
        in
        union
          [ case
              ~title:"tree"
              (Tag 0)
              map_encoding
              (function `Tree t -> Some t | `Value _ -> None)
              (fun t -> `Tree t);
            case
              ~title:"value"
              (Tag 1)
              bytes
              (function `Value v -> Some v | `Tree _ -> None)
              (fun v -> `Value v) ])
end
