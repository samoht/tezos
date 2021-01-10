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

module StringMap = TzString.Map

type t = Dir of t StringMap.t | Key of Bytes.t

module M = struct
  type key = string list

  type value = Bytes.t

  type nonrec t = t

  let pp_key =
    Format.(
      pp_print_list
        ~pp_sep:(fun ppf () -> pp_print_string ppf " / ")
        pp_print_string)

  let empty = Dir StringMap.empty

  type tree = t

  let rec find_tree m k =
    match (k, m) with
    | ([], m) ->
        Some m
    | (n :: k, Dir m) -> (
      match StringMap.find_opt n m with
      | Some res ->
          find_tree res k
      | None ->
          None )
    | (_ :: _, Key _) ->
        None

  let rec raw_add m k v =
    match (k, m, v) with
    | ([], (Key _ as m), Some v) ->
        if m = v then None else Some v
    | ([], (Dir _ as m), Some v) ->
        if m == v then None else Some v
    | ([], (Key _ | Dir _), None) ->
        Some empty
    | (n :: k, Dir m, _) -> (
      match
        raw_add (Option.value ~default:empty (StringMap.find_opt n m)) k v
      with
      | None ->
          None
      | Some rm when rm = empty ->
          Some (Dir (StringMap.remove n m))
      | Some rm ->
          Some (Dir (StringMap.add n rm m)) )
    | (_ :: _, Key _, None) ->
        None
    | (_ :: _, Key _, Some _) ->
        Format.kasprintf
          Stdlib.failwith
          "Mem_context.set: cannot set value below key %a, because there's a \
           Key value here. A value can only be nested below a Dir, not a Key"
          pp_key
          k

  let mem m k =
    match find_tree m k with
    | Some (Key _) ->
        Lwt.return_true
    | Some (Dir _) | None ->
        Lwt.return_false

  let mem_tree m k =
    match find_tree m k with
    | Some (Dir _) ->
        Lwt.return_true
    | Some (Key _) | None ->
        Lwt.return_false

  let find m k =
    match find_tree m k with
    | Some (Key v) ->
        Lwt.return_some v
    | Some (Dir _) | None ->
        Lwt.return_none

  let add_tree m k v =
    match raw_add m k (Some v) with
    | None ->
        Lwt.return m
    | Some m ->
        Lwt.return m

  let add m k v = add_tree m k (Key v)

  let remove m k =
    match raw_add m k None with None -> Lwt.return m | Some m -> Lwt.return m

  (* TODO(samoht): add tests *)
  let fold ?depth m k ~init ~value ~tree =
    match find_tree m k with
    | None | Some (Key _) ->
        Lwt.return init
    | Some (Dir m as tr) ->
        ( match depth with
        | None ->
            tree [] tr init
        | Some depth ->
            if depth = 0 then tree [] tr init else Lwt.return init )
        >>= fun init ->
        let rec aux d path acc m =
          match depth with
          | Some depth when d > depth ->
              acc
          | _ ->
              StringMap.fold
                (fun n m acc ->
                  let path = n :: path in
                  let k = List.rev path in
                  let act =
                    match depth with None -> true | Some depth -> d = depth
                  in
                  acc
                  >>= fun acc ->
                  match m with
                  | Key v ->
                      if act then value k v acc else Lwt.return acc
                  | Dir r ->
                      (if act then tree k m acc else Lwt.return acc)
                      >>= fun acc -> aux (d + 1) path (Lwt.return acc) r)
                m
                acc
        in
        aux 1 [] (Lwt.return init) m

  let find_tree m k = Lwt.return (find_tree m k)

  module Tree = struct
    let empty _ = empty

    let is_empty = function Key _ -> false | Dir d -> StringMap.is_empty d

    let add = add

    let mem = mem

    let find = find

    let remove = remove

    let add_tree = add_tree

    let mem_tree = mem_tree

    let find_tree = find_tree

    let fold = fold
  end

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    mu "memory_context" (fun encoding ->
        let map_encoding =
          conv
            (fun map -> List.of_seq (StringMap.to_seq map))
            (fun bindings -> StringMap.of_seq (List.to_seq bindings))
            (list (tup2 string encoding))
        in
        union
          [ case
              ~title:"directory"
              (Tag 0)
              map_encoding
              (function Dir map -> Some map | Key _ -> None)
              (fun map -> Dir map);
            case
              ~title:"value"
              (Tag 1)
              bytes
              (function Key v -> Some v | Dir _ -> None)
              (fun v -> Key v) ])
end

open Tezos_protocol_environment

type _ Context.kind += Memory : t Context.kind

let ops = (module M : CONTEXT with type t = 'ctxt and type tree = 'tree)

let wit = Context.witness ()

let empty =
  let ctxt = M.empty in
  Context.Context {ops; ctxt; kind = Memory; wit}

let project : Context.t -> t =
 fun (Context.Context {ctxt; kind; _} : Context.t) ->
  match kind with Memory -> ctxt | _ -> assert false

let inject : t -> Context.t =
 fun ctxt -> Context.Context {ops; ctxt; kind = Memory; wit}

let encoding : Context.t Data_encoding.t =
  let open Data_encoding in
  conv project inject M.encoding
