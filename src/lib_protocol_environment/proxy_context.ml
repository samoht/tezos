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

module StringMap = Map.Make (String)
module Local = Memory_context.M

module M = struct
  type key = Local.key

  type value = Local.value

  type tree = Memory_context.t =
    | Dir of tree Map.Make(String).t
    | Key of value

  module type ProxyDelegate = sig
    val proxy_dir_mem : key -> bool tzresult Lwt.t

    val proxy_get : key -> tree option tzresult Lwt.t

    val proxy_mem : key -> bool tzresult Lwt.t
  end

  type proxy_delegate = (module ProxyDelegate)

  (* When the option is [None], this instance of [M] should behave
     like [Memory_context]. *)
  type t = {proxy : proxy_delegate option; local : tree}

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

  type tree = {proxy : M.proxy_delegate option; root : key; tree : M.tree}

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

  let raw_find ~root ~(proxy : M.proxy_delegate option) t k =
    Local.find_tree t k
    >>= function
    | None -> (
        L.(S.emit proxy_context_missing) k
        >>= fun () ->
        match proxy with
        | None ->
            Lwt.return_none
        | Some proxy -> (
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

  let raw_mem ~root ~(proxy : M.proxy_delegate option) t k =
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
      | Some proxy -> (
          let (module ProxyDelegation) = proxy in
          ProxyDelegation.proxy_mem (root @ k)
          >>= function
          | Error err ->
              L.(S.emit delegation_error ("mem", err))
              >>= fun () -> Lwt.return_false
          | Ok x ->
              Lwt.return x ) )

  let mem {M.proxy; local; _} k = raw_mem ~root:[] ~proxy local k

  let mem_tree m k =
    Local.find_tree m.M.local k
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

  let find {M.proxy; local; _} k =
    raw_find ~root:[] ~proxy local k
    >|= function Some (Key v) -> Some v | _ -> None

  let get m k =
    find m k >>= function None -> Lwt.fail Not_found | Some v -> Lwt.return v

  let find_tree {M.proxy; local; _} k =
    raw_find ~root:[] ~proxy local k
    >|= Option.map (fun tree -> {proxy; root = k; tree})

  let get_tree t k =
    find_tree t k
    >>= function None -> Lwt.fail Not_found | Some v -> Lwt.return v

  let add_tree m k v =
    Local.add_tree m.M.local k v.tree
    >|= fun local -> if m.local == local then m else {m with local}

  let add m k v =
    Local.add m.M.local k v
    >|= fun local -> if m.local == local then m else {m with local}

  let remove_rec m k =
    Local.remove_rec m.M.local k
    >|= fun local -> if m.local == local then m else {m with local}

  (* TODO(samoht): add tests *)
  let raw_fold ?depth ~root ~proxy t k ~init ~value ~tree =
    let tree_root x = k @ x in
    raw_find ~root ~proxy t k
    >>= function
    | None | Some (Key _) ->
        Lwt.return init
    | Some (Dir m) ->
        let rec aux d path acc m =
          match depth with
          | Some depth when d > depth ->
              acc
          | _ ->
              StringMap.fold
                (fun n m acc ->
                  let path = n :: path in
                  let k = List.rev path in
                  acc
                  >>= fun acc ->
                  match m with
                  | M.Key v ->
                      value k v acc
                  | Dir r ->
                      tree k {root = tree_root k; proxy; tree = m} acc
                      >>= fun acc -> aux (d + 1) path (Lwt.return acc) r)
                m
                acc
        in
        aux 0 k (Lwt.return init) m

  let fold ?depth {M.proxy; local} = raw_fold ?depth ~root:[] ~proxy local

  module Tree = struct
    let empty _ = {proxy = None; root = []; tree = M.empty}

    let is_empty t =
      match t.tree with Key _ -> false | Dir t -> StringMap.is_empty t

    let add t k v = Local.Tree.add t.tree k v >|= fun tree -> {t with tree}

    let mem {root; proxy; tree} k = raw_mem ~root ~proxy tree k

    let find {root; proxy; tree} k =
      raw_find ~root ~proxy tree k
      >|= function Some (Key v) -> Some v | _ -> None

    let get t k =
      find t k
      >>= function None -> Lwt.fail Not_found | Some v -> Lwt.return v

    let remove_rec t k =
      Local.Tree.remove_rec t.tree k
      >|= fun tree -> if tree == t.tree then t else {t with tree}

    let add_tree t k v =
      Local.Tree.add_tree t.tree k v.tree >|= fun tree -> {t with tree}

    let mem_tree {root; proxy; tree} k = raw_mem ~root ~proxy tree k

    let find_tree {root; proxy; tree} k =
      raw_find ~root ~proxy tree k
      >|= Option.map (fun tree -> {root = root @ k; proxy; tree})

    let get_tree t k =
      find_tree t k
      >>= function None -> Lwt.fail Not_found | Some v -> Lwt.return v

    let fold ?depth {root; proxy; tree} = raw_fold ?depth ~root ~proxy tree
  end

  let current_protocol_key = ["protocol"]

  let set_protocol v key =
    add v current_protocol_key (Protocol_hash.to_bytes key)

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

open Tezos_protocol_environment

type _ Context.kind += Proxy : M.t Context.kind

let ops = (module C : CONTEXT with type t = 'ctxt and type tree = 'tree)

let wit = Context.witness ()

let empty proxy =
  let ctxt = M.{proxy; local = empty} in
  Context.Context {ops; ctxt; kind = Proxy; wit}

let set_delegate : M.proxy_delegate -> Context.t -> Context.t =
 fun proxy (Context.Context t) ->
  match t.kind with
  | Proxy ->
      let ctxt = {t.ctxt with proxy = Some proxy} in
      Context.Context {t with ctxt}
  | _ ->
      assert false
