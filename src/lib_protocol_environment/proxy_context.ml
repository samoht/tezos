(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
module Local = Memory_context.M

module M = struct
  type key = Local.key

  type value = Local.value

  type tree = Memory_context.t = Dir of tree StringMap.t | Key of value

  module type ProxyDelegate = sig
    val proxy_dir_mem : key -> bool tzresult Lwt.t

    val proxy_get : key -> tree option tzresult Lwt.t

    val proxy_mem : key -> bool tzresult Lwt.t
  end

  type proxy_delegate = (module ProxyDelegate)

  (* When the option is [None], this instance of [M] should behave
     like [Memory_context]. *)
  type t = {proxy : proxy_delegate option; tree : tree}

  let rec tree_size = function
    | Key _ ->
        1
    | Dir t ->
        StringMap.fold (fun _ t' i -> tree_size t' + i) t 0

  let empty = Dir StringMap.empty
end

module C = struct
  type key = M.key

  type value = M.value

  type t = M.t

  type proxy = key * M.proxy_delegate

  type tree = {proxy : proxy option; tree : M.tree}

  (** Generic pretty printing functions *)
  let pp_key ppf key =
    Format.pp_print_list
      ~pp_sep:(fun ppf _ -> Format.fprintf ppf "; ")
      Format.pp_print_string
      ppf
      key

  module L = struct
    module S = Internal_event.Simple

    let section = ["proxy"; "context"]

    let proxy_context_missing =
      S.declare_1
        ~section
        ~name:"proxy_context_missing"
        ~msg:"delegating to proxy cache, because data misses for: {key}"
        ~pp1:pp_key
        ("key", Data_encoding.(Variable.list string))

    let delegation_error =
      S.declare_2
        ~section
        ~name:"delegation_error"
        ~msg:
          "{function} returned an error, ignoring it but this is bad: {trace}"
        ~pp2:pp_print_error
        ("function", Data_encoding.string)
        ("trace", Error_monad.trace_encoding)
  end

  let raw_find ~(proxy : proxy option) t k =
    Local.find_tree t k
    >>= function
    | None -> (
        L.(S.emit proxy_context_missing) k
        >>= fun () ->
        match proxy with
        | None ->
            Lwt.return_none
        | Some (root, proxy) -> (
            let (module ProxyDelegation) = proxy in
            ProxyDelegation.proxy_get (root @ k)
            >>= function
            | Error err ->
                L.(S.emit delegation_error ("get", err))
                >>= fun () -> Lwt.return_none
            | Ok x ->
                Lwt.return x ) )
    | Some _ as v ->
        Lwt.return v

  let raw_mem ~(proxy : proxy option) t k =
    Local.find_tree t k
    >>= function
    | Some (Key _) ->
        Lwt.return_true
    | Some (Dir _) ->
        Lwt.return_false
    | None -> (
      match proxy with
      | None ->
          Lwt.return_false
      | Some (root, proxy) -> (
          let (module ProxyDelegation) = proxy in
          ProxyDelegation.proxy_mem (root @ k)
          >>= function
          | Error err ->
              L.(S.emit delegation_error ("mem", err))
              >>= fun () -> Lwt.return_false
          | Ok x ->
              Lwt.return x ) )

  let root_proxy proxy = Option.map (fun p -> ([], p)) proxy

  let mem {M.proxy; tree; _} k = raw_mem ~proxy:(root_proxy proxy) tree k

  let mem_tree m k =
    Local.find_tree m.M.tree k
    >>= function
    | Some (Key _) ->
        Lwt.return_false
    | Some (Dir _) ->
        Lwt.return_true
    | None -> (
      match m.proxy with
      | None ->
          Lwt.return_false
      | Some proxy -> (
          let (module ProxyDelegation) = proxy in
          ProxyDelegation.proxy_dir_mem k
          >>= function
          | Error err ->
              L.(S.emit delegation_error ("dir_mem", err))
              >>= fun () -> Lwt.return_false
          | Ok x ->
              Lwt.return x ) )

  let find {M.proxy; tree; _} k =
    let proxy = root_proxy proxy in
    raw_find ~proxy tree k >|= function Some (Key v) -> Some v | _ -> None

  let find_tree {M.proxy; tree; _} k =
    let proxy = root_proxy proxy in
    raw_find ~proxy tree k
    >|= Option.map (fun tree ->
            let proxy = Option.map (fun (x, y) -> (x @ k, y)) proxy in
            {proxy; tree})

  let add_tree m k v =
    Local.add_tree m.M.tree k v.tree
    >|= fun tree -> if m.tree == tree then m else {m with tree}

  let add m k v =
    Local.add m.M.tree k v
    >|= fun tree -> if m.tree == tree then m else {m with tree}

  let remove m k =
    Local.remove m.M.tree k
    >|= fun tree -> if m.tree == tree then m else {m with tree}

  (* TODO(samoht): add tests *)
  let raw_fold ?depth ~proxy t k ~init ~value ~tree =
    let tree_proxy path =
      Option.map (fun (root, p) -> (root @ k @ path, p)) proxy
    in
    raw_find ~proxy t k
    >>= function
    | None | Some (Key _) ->
        Lwt.return init
    | Some (Dir m as tr) ->
        ( match depth with
        | None ->
            let tr = {proxy = tree_proxy []; tree = tr} in
            tree [] tr init
        | Some depth ->
            if depth = 0 then
              let tr = {proxy = tree_proxy []; tree = tr} in
              tree [] tr init
            else Lwt.return init )
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
                  | M.Key v ->
                      if act then value k v acc else Lwt.return acc
                  | Dir r ->
                      let m = {proxy = tree_proxy k; tree = m} in
                      (if act then tree k m acc else Lwt.return acc)
                      >>= fun acc -> aux (d + 1) path (Lwt.return acc) r)
                m
                acc
        in
        aux 1 k (Lwt.return init) m

  let fold ?depth {M.proxy; tree} =
    raw_fold ?depth ~proxy:(root_proxy proxy) tree

  module Tree = struct
    let empty _ = {proxy = None; tree = M.empty}

    let is_empty t =
      match t.tree with Key _ -> false | Dir t -> StringMap.is_empty t

    let add t k v = Local.Tree.add t.tree k v >|= fun tree -> {t with tree}

    let mem {proxy; tree} k = raw_mem ~proxy tree k

    let find {proxy; tree} k =
      raw_find ~proxy tree k >|= function Some (Key v) -> Some v | _ -> None

    let remove t k =
      Local.Tree.remove t.tree k
      >|= fun tree -> if tree == t.tree then t else {t with tree}

    let add_tree t k v =
      Local.Tree.add_tree t.tree k v.tree >|= fun tree -> {t with tree}

    let mem_tree {proxy; tree} k = raw_mem ~proxy tree k

    let find_tree {proxy; tree} k =
      raw_find ~proxy tree k
      >|= Option.map (fun tree ->
              let proxy = Option.map (fun (root, p) -> (root @ k, p)) proxy in
              {proxy; tree})

    let fold ?depth {proxy; tree} = raw_fold ?depth ~proxy tree
  end
end

open Tezos_protocol_environment

type _ Context.kind += Proxy : M.t Context.kind

let ops = (module C : CONTEXT with type t = 'ctxt and type tree = 'tree)

let wit = Context.witness ()

let empty proxy =
  let ctxt = M.{proxy; tree = empty} in
  Context.Context {ops; ctxt; kind = Proxy; wit}

let set_delegate : M.proxy_delegate -> Context.t -> Context.t =
 fun proxy (Context.Context t) ->
  match t.kind with
  | Proxy ->
      let ctxt = {t.ctxt with proxy = Some proxy} in
      Context.Context {t with ctxt}
  | _ ->
      assert false
