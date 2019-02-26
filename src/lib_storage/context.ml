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

(** Tezos - Versioned (key x value) store (over Irmin) *)

module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Hash = struct
  module H = Context_hash

  (* XXX(samoht): probably not very efficient... *)
  type t = H.t

  let pp = H.pp

  let of_string x =
    match H.of_b58check x with Ok x -> Ok x | Error _ -> assert false

  let t =
    Irmin.Type.map ~cli:(pp, of_string)
      Irmin.Type.(string_of (`Fixed H.size))
      (fun x -> H.of_string_exn x)
      (fun x -> H.to_string x)

  let digest_size = H.size

  let hash = H.hash

  let digest s = H.hash_string [s]
end

module Node = struct
  module M = Irmin.Private.Node.Make (Hash) (Path) (Metadata)

  module V1 = struct
    module Hash = Irmin.Hash.V1 (Hash)

    type kind = [`Node | `Contents of Metadata.t]

    type entry = {key: string Lazy.t; kind: kind; name: M.step; node: Hash.t}

    let compare_entries a b = compare (Lazy.force a.key) (Lazy.force b.key)

    (* Irmin 1.4 uses int64 to store string lengths *)
    let step_t =
      let pre_digest = Irmin.Type.(pre_digest (string_of `Int64)) in
      Irmin.Type.like M.step_t ~pre_digest

    let metadata_t =
      let some = "\255\000\000\000\000\000\000\000" in
      let none = "\000\000\000\000\000\000\000\000" in
      Irmin.Type.(map (string_of (`Fixed 8)))
        (fun s ->
          match s.[0] with
          | '\255' -> None
          | '\000' -> Some ()
          | _ -> assert false )
        (function Some _ -> some | None -> none)

    (* Irmin 1.4 uses int64 to store list lengths *)
    let entry_t : entry Irmin.Type.t =
      let open Irmin.Type in
      record "Tree.entry" (fun kind name node ->
          let kind = match kind with None -> `Node | Some m -> `Contents m in
          let key =
            match kind with
            | `Node -> lazy (name ^ "/")
            | `Contents _ -> lazy name
          in
          {key; kind; name; node} )
      |+ field "kind" metadata_t (function
           | {kind= `Node; _} -> None
           | {kind= `Contents m; _} -> Some m )
      |+ field "name" step_t (fun {name; _} -> name)
      |+ field "node" Hash.t (fun {node; _} -> node)
      |> sealr

    let entries_t : entry list Irmin.Type.t =
      Irmin.Type.(list ~len:`Int64 entry_t)

    let import_entry (s, v) =
      match v with
      | `Node h -> {key= lazy (s ^ "/"); name= s; kind= `Node; node= h}
      | `Contents (h, m) -> {key= lazy s; name= s; kind= `Contents m; node= h}

    let import t = List.map import_entry (M.list t)

    (* store the entries before hashing to be compatible with Tezos v1 *)
    let pre_digest entries =
      let entries = List.fast_sort compare_entries entries in
      Irmin.Type.pre_digest entries_t entries
  end

  include M

  let pre_digest_v1 x = V1.pre_digest (V1.import x)

  let t = Irmin.Type.(like t ~pre_digest:pre_digest_v1)
end

module Commit = struct
  module M = Irmin.Private.Commit.Make (Hash)
  module V1 = Irmin.Private.Commit.V1 (M)
  include M

  let pre_digest_v1 t = Irmin.Type.pre_digest V1.t (V1.import t)

  let t = Irmin.Type.like t ~pre_digest:pre_digest_v1
end

module Contents = struct
  type t = MBytes.t

  let pre_digest_v1 x =
    let ty = Irmin.Type.(pair (string_of `Int64) unit) in
    Irmin.Type.(pre_digest ty) (MBytes.to_string x, ())

  let t =
    Irmin.Type.(map ~pre_digest:pre_digest_v1 string)
      MBytes.of_string MBytes.to_string

  let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
end

module Store =
  Irmin_pack.Make_ext (Irmin.Metadata.None) (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)
    (Node)
    (Commit)

type index =
  {path: string; repo: Store.Repo.t; patch_context: context -> context Lwt.t}

and context = {index: index; parents: Store.commit list; tree: Store.tree}

type t = context

(*-- Version Access and Update -----------------------------------------------*)

let current_protocol_key = ["protocol"]

let current_test_chain_key = ["test_chain"]

let current_data_key = ["data"]

let exists index key =
  Store.Commit.of_hash index.repo key
  >|= function None -> false | Some _ -> true

let checkout index key =
  Store.Commit.of_hash index.repo key
  >>= function
  | None -> Lwt.return_none
  | Some commit ->
      let tree = Store.Commit.tree commit in
      let ctxt = {index; tree; parents= [commit]} in
      Lwt.return_some ctxt

let checkout_exn index key =
  checkout index key
  >>= function None -> Lwt.fail Not_found | Some p -> Lwt.return p

let raw_commit ~time ?(message = "") context =
  let info =
    Irmin.Info.v ~date:(Time.to_seconds time) ~author:"Tezos" message
  in
  let parents = List.map Store.Commit.hash context.parents in
  Store.Commit.v context.index.repo ~info ~parents context.tree

module P = Store.Private

let hash ~time ?(message = "") context =
  let info =
    Irmin.Info.v ~date:(Time.to_seconds time) ~author:"Tezos" message
  in
  let parents = List.map (fun c -> Store.Commit.hash c) context.parents in
  let node = Store.Tree.hash context.tree in
  let commit = P.Commit.Val.v ~parents ~node ~info in
  let x = P.Commit.Key.digest commit in
  (* FIXME: this doesn't have to be lwt *)
  Lwt.return x

let commit ~time ?message context =
  raw_commit ~time ?message context
  >>= fun commit ->
  let h = Store.Commit.hash commit in
  Lwt.return h

(*-- Generic Store Primitives ------------------------------------------------*)

let data_key key = "data" :: key

let undata_key = function "data" :: key -> key | _ -> assert false

type key = string list

type value = MBytes.t

let mem ctxt key =
  Store.Tree.mem ctxt.tree (data_key key) >>= fun v -> Lwt.return v

let dir_mem ctxt key =
  Store.Tree.mem_tree ctxt.tree (data_key key) >>= fun v -> Lwt.return v

let raw_get ctxt key = Store.Tree.find ctxt.tree key

let get t key = raw_get t (data_key key)

let raw_set ctxt key data =
  Store.Tree.add ctxt.tree key data >>= fun tree -> Lwt.return {ctxt with tree}

let set t key data = raw_set t (data_key key) data

let raw_del ctxt key =
  Store.Tree.remove ctxt.tree key >>= fun tree -> Lwt.return {ctxt with tree}

let del t key = raw_del t (data_key key)

let remove_rec ctxt key =
  Store.Tree.remove ctxt.tree (data_key key)
  >>= fun tree -> Lwt.return {ctxt with tree}

let copy ctxt ~from ~to_ =
  Store.Tree.find_tree ctxt.tree (data_key from)
  >>= function
  | None -> Lwt.return_none
  | Some sub_tree ->
      Store.Tree.add_tree ctxt.tree (data_key to_) sub_tree
      >>= fun tree -> Lwt.return_some {ctxt with tree}

let fold ctxt key ~init ~f =
  Store.Tree.list ctxt.tree (data_key key)
  >>= fun keys ->
  Lwt_list.fold_left_s
    (fun acc (name, kind) ->
      let key =
        match kind with
        | `Contents -> `Key (key @ [name])
        | `Node -> `Dir (key @ [name])
      in
      f key acc )
    init keys

(*-- Predefined Fields -------------------------------------------------------*)

let get_protocol v =
  raw_get v current_protocol_key
  >>= function
  | None ->
      Store.Private.Repo.batch v.index.repo (fun _ _ _ -> Lwt.return ())
      >>= fun () -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)

let set_protocol v key =
  raw_set v current_protocol_key (Protocol_hash.to_bytes key)

let get_test_chain v =
  raw_get v current_test_chain_key
  >>= function
  | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_chain)")
  | Some data -> (
    match Data_encoding.Binary.of_bytes Test_chain_status.encoding data with
    | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_chain)")
    | Some r -> Lwt.return r )

let set_test_chain v id =
  let data = Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding id in
  raw_set v current_test_chain_key data

let del_test_chain v = raw_del v current_test_chain_key

let fork_test_chain v ~protocol ~expiration =
  set_test_chain v (Forking {protocol; expiration})

(*-- Initialisation ----------------------------------------------------------*)

let init ?patch_context ?mapsize:_ ?readonly:_ root =
  Store.Repo.v (Irmin_pack.config root)
  >>= fun repo ->
  Lwt.return
    { path= root
    ; repo
    ; patch_context=
        ( match patch_context with
        | None -> fun ctxt -> Lwt.return ctxt
        | Some patch_context -> patch_context ) }

let get_branch chain_id = Format.asprintf "%a" Chain_id.pp chain_id

let commit_genesis index ~chain_id ~time ~protocol =
  let tree = Store.Tree.empty in
  let ctxt = {index; tree; parents= []} in
  index.patch_context ctxt
  >>= fun ctxt ->
  set_protocol ctxt protocol
  >>= fun ctxt ->
  set_test_chain ctxt Not_running
  >>= fun ctxt ->
  raw_commit ~time ~message:"Genesis" ctxt
  >>= fun commit ->
  Store.Branch.set index.repo (get_branch chain_id) commit
  >>= fun () -> Lwt.return (Store.Commit.hash commit)

let compute_testchain_genesis forked_block =
  let genesis = Block_hash.hash_bytes [Block_hash.to_bytes forked_block] in
  let chain_id = Chain_id.of_block_hash genesis in
  (chain_id, genesis)

let commit_test_chain_genesis index forked_block time ctxt =
  let chain_id, genesis = compute_testchain_genesis forked_block in
  let branch = get_branch chain_id in
  let message = Format.asprintf "Forking testchain: %s." branch in
  raw_commit ~time ~message ctxt
  >>= fun commit ->
  Store.Branch.set index.repo branch commit
  >>= fun () -> return (chain_id, genesis, Store.Commit.hash commit)

let reset_test_chain ctxt forked_block timestamp =
  get_test_chain ctxt
  >>= function
  | Not_running -> Lwt.return ctxt
  | Running {expiration} ->
      if Time.(expiration <= timestamp) then set_test_chain ctxt Not_running
      else Lwt.return ctxt
  | Forking {protocol; expiration} ->
      let chain_id, genesis = compute_testchain_genesis forked_block in
      set_test_chain ctxt (Running {chain_id; genesis; protocol; expiration})

let clear_test_chain index chain_id =
  (* TODO remove commits... ??? *)
  let branch = get_branch chain_id in
  Store.Branch.remove index.repo branch

let set_head index chain_id commit =
  let branch = get_branch chain_id in
  Store.Private.Branch.set
    (Store.Private.Repo.branch_t index.repo)
    branch commit

let set_master index commit =
  Store.Commit.of_hash index.repo commit
  >>= function
  | None -> assert false
  | Some commit -> Store.Branch.set index.repo Store.Branch.master commit

(* Context dumping *)

module Pruned_block = struct
  type t =
    { block_header: Block_header.t
    ; operations: (int * Operation.t list) list
    ; operation_hashes: (int * Operation_hash.t list) list }

  let encoding =
    let open Data_encoding in
    conv
      (fun {block_header; operations; operation_hashes} ->
        (block_header, operations, operation_hashes) )
      (fun (block_header, operations, operation_hashes) ->
        {block_header; operations; operation_hashes} )
      (obj3
         (req "block_header" (dynamic_size Block_header.encoding))
         (req "operations"
            (list (tup2 int31 (list (dynamic_size Operation.encoding)))))
         (req "operation_hashes"
            (list (tup2 int31 (list Operation_hash.encoding)))))

  let to_bytes = Data_encoding.Binary.to_bytes_exn encoding

  let of_bytes = Data_encoding.Binary.of_bytes encoding
end

module Block_data = struct
  type t = {block_header: Block_header.t; operations: Operation.t list list}

  let encoding =
    let open Data_encoding in
    conv
      (fun {block_header; operations} -> (block_header, operations))
      (fun (block_header, operations) -> {block_header; operations})
      (obj2
         (req "block_header" (dynamic_size Block_header.encoding))
         (req "operations" (list (list (dynamic_size Operation.encoding)))))

  let to_bytes = Data_encoding.Binary.to_bytes_exn encoding

  let of_bytes = Data_encoding.Binary.of_bytes encoding

  let empty =
    { block_header=
        Block_header.
          { protocol_data= MBytes.empty
          ; shell=
              { level= 0l
              ; proto_level= 0
              ; predecessor= Block_hash.zero
              ; timestamp= Time.epoch
              ; validation_passes= 0
              ; operations_hash= Operation_list_list_hash.zero
              ; fitness= []
              ; context= Context_hash.zero } }
    ; operations= [[]] }
end

module Protocol_data = struct
  type info = {author: string; message: string; timestamp: Time.t}

  let info_encoding =
    let open Data_encoding in
    conv
      (fun {author; message; timestamp} -> (author, message, timestamp))
      (fun (author, message, timestamp) -> {author; message; timestamp})
      (obj3 (req "author" string) (req "message" string)
         (req "timestamp" Time.encoding))

  type data =
    { info: info
    ; protocol_hash: Protocol_hash.t
    ; test_chain_status: Test_chain_status.t
    ; data_key: Context_hash.t
    ; parents: Context_hash.t list }

  let data_encoding =
    let open Data_encoding in
    conv
      (fun {info; protocol_hash; test_chain_status; data_key; parents} ->
        (info, protocol_hash, test_chain_status, data_key, parents) )
      (fun (info, protocol_hash, test_chain_status, data_key, parents) ->
        {info; protocol_hash; test_chain_status; data_key; parents} )
      (obj5 (req "info" info_encoding)
         (req "protocol_hash" Protocol_hash.encoding)
         (req "test_chain_status" Test_chain_status.encoding)
         (req "data_key" Context_hash.encoding)
         (req "parents" (list Context_hash.encoding)))

  type t = Int32.t * data

  let encoding =
    let open Data_encoding in
    tup2 int32 data_encoding

  let empty =
    let info = {author= ""; message= ""; timestamp= Time.now ()} in
    let data =
      { info
      ; protocol_hash= Protocol_hash.zero
      ; test_chain_status= Test_chain_status.Not_running
      ; data_key= Context_hash.zero
      ; parents= [Context_hash.zero] }
    in
    (0l, data)

  let to_bytes = Data_encoding.Binary.to_bytes_exn encoding

  let of_bytes = Data_encoding.Binary.of_bytes encoding
end

module Dumpable_context = struct
  type nonrec index = index

  type nonrec context = context

  type tree = Store.tree

  type hash = Store.hash

  type step = string

  type key = step list

  type commit_info = Irmin.Info.t

  type batch =
    | Batch of
        [`Read | `Write] Store.Private.Contents.t
        * [`Read | `Write] Store.Private.Node.t

  let b x y = Batch (x, y)

  let batch index f =
    Store.Private.Repo.batch index.repo (fun x y _ -> f (b x y))

  let pp_tree = Irmin.Type.pp Store.tree_t

  let hash_export h = Context_hash.to_bytes h

  let hash_import mb = Context_hash.of_bytes mb >>? fun h -> Ok h

  let hash_equal h1 h2 = Context_hash.(h1 = h2)

  let context_parents ctxt =
    match ctxt with
    | {parents= [commit]; _} ->
        let parents = Store.Commit.parents commit in
        let parents = List.sort Context_hash.compare parents in
        parents
    | _ -> assert false

  let context_info = function
    | {parents= [c]; _} -> Store.Commit.info c
    | _ -> assert false

  let context_info_export i = Irmin.Info.(date i, author i, message i)

  let context_info_import (date, author, message) =
    Irmin.Info.v ~date ~author message

  let get_context idx bh = checkout idx bh.Block_header.shell.context

  let set_context ~info ~parents ctxt bh =
    (* let parents = List.sort Context_hash.compare parents in *)
    Store.Commit.v ctxt.index.repo ~info ~parents ctxt.tree
    >>= fun c ->
    let h = Store.Commit.hash c in
    if Context_hash.equal bh.Block_header.shell.context h then
      Lwt.return_some bh
    else assert false

  (*      Lwt.return_none *)

  let context_tree ctxt = ctxt.tree

  let tree_hash ctxt = Store.Tree.hash ctxt

  let sub_tree tree key = Store.Tree.find_tree tree key

  let tree_list tree = Store.Tree.list tree []

  let tree_content tree = Store.Tree.find tree []

  let make_context index = {index; tree= Store.Tree.empty; parents= []}

  let update_context context tree = {context with tree}

  let add_tree_hash index tree key hash =
    Store.Tree.of_hash index.repo hash
    >>= function
    | None -> Lwt.return_none
    | Some sub_tree ->
        Store.Tree.add_tree tree key (sub_tree :> tree) >>= Lwt.return_some

  let add_blob_hash index tree key hash =
    Store.Contents.of_hash index.repo hash
    >>= function
    | None -> Lwt.return_none
    | Some sub_tree ->
        Store.Tree.add_tree tree key (`Contents (sub_tree, ()))
        >>= Lwt.return_some

  let add_mbytes (Batch (t, _)) tree key bytes =
    Store.Private.Contents.add t bytes
    >>= fun hash -> Store.Tree.add tree key bytes >|= fun tree -> (hash, tree)

  let add_dir index (Batch (x, y)) tree key l =
    let rec fold_list sub_tree = function
      | [] -> Lwt.return_some sub_tree
      | (step, ty, hash) :: tl -> (
        match ty with
        | `Blob -> (
            add_blob_hash index sub_tree [step] hash
            >>= function
            | None -> Lwt.return_none | Some sub_tree -> fold_list sub_tree tl
            )
        | `Node -> (
            add_tree_hash index sub_tree [step] hash
            >>= function
            | None -> Lwt.return_none | Some sub_tree -> fold_list sub_tree tl
            ) )
    in
    fold_list Store.Tree.empty l
    >>= function
    | None -> Lwt.return_none
    | Some sub_tree ->
        Store.export_tree index.repo x y sub_tree
        >>= fun hash ->
        Store.Tree.add_tree tree key sub_tree
        >>= fun tree -> Lwt.return_some (hash, tree)

  module Commit_hash = Context_hash
  module Block_header = Block_header
  module Block_data = Block_data
  module Pruned_block = Pruned_block
  module Protocol_data = Protocol_data
end

(* Protocol data *)

let data_node_hash context =
  Store.Tree.get_tree context.tree current_data_key >|= Store.Tree.hash

let get_transition_block_headers pruned_blocks =
  let rec aux hs x bs =
    match bs with
    | [] -> x :: hs
    | b :: bs ->
        let xl = x.Pruned_block.block_header.shell.proto_level in
        let bl = b.Pruned_block.block_header.shell.proto_level in
        if not (xl = bl) then aux (x :: hs) b bs else aux hs b bs
  in
  match pruned_blocks with [] -> assert false | x :: xs -> aux [] x xs

let get_protocol_data_from_pruned_block index pruned_block =
  checkout_exn index
    pruned_block.Pruned_block.block_header.Block_header.shell.context
  >>= fun context ->
  let level = pruned_block.block_header.shell.level in
  let irmin_info = Dumpable_context.context_info context in
  let date = Irmin.Info.date irmin_info in
  let author = Irmin.Info.author irmin_info in
  let message = Irmin.Info.message irmin_info in
  let info =
    {Protocol_data.timestamp= Time.of_seconds date; author; message}
  in
  let parents = Dumpable_context.context_parents context in
  get_protocol context
  >>= fun protocol_hash ->
  get_test_chain context
  >>= fun test_chain_status ->
  data_node_hash context
  >>= fun data_key ->
  Lwt.return
    ( level
    , {Protocol_data.parents; protocol_hash; test_chain_status; data_key; info}
    )

let load_protocol_data index pruned_blocks =
  let transition_pruned_blocks = get_transition_block_headers pruned_blocks in
  Lwt_list.map_s
    (get_protocol_data_from_pruned_block index)
    transition_pruned_blocks

(* Mock some Store types, so we can build our own Merkle tree. *)

(* XXX(samoht): WAT? *)
module Mock : sig
  val node : Store.Repo.t -> P.Node.key -> Store.node

  val commit : Store.repo -> Store.hash -> P.Commit.value -> Store.commit
end = struct
  [@@@ocaml.warning "-37"]

  type commit = {r: Store.Repo.t; h: Context_hash.t; v: P.Commit.value}

  type empty

  type u =
    | Map : empty -> u
    | Key : Store.Repo.t * Store.hash -> u
    | Both : empty * empty * empty -> u

  and node = {mutable v: u}

  let node repo key =
    let t : u = Key (repo, key) in
    let node : node = {v= t} in
    (Obj.magic node : Store.node)

  let commit r h v =
    let c : commit = {r; h; v} in
    (Obj.magic c : Store.commit)
end

(* ??? *)
let validate_context_hash_consistency_and_commit ~data_hash:_
    ~expected_context_hash:_ ~timestamp:_ ~test_chain:_ ~protocol_hash:_
    ~message:_ ~author:_ ~parents:_ ~index:_ =
  Lwt.return true

(*
  let protocol_value = Protocol_hash.to_bytes protocol_hash in
  let test_chain_value =
    Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding test_chain
  in
  let tree = Store.Tree.empty in
  Store.Tree.add tree current_protocol_key protocol_value
  >>= fun tree ->
  Store.Tree.add tree current_test_chain_key test_chain_value
  >>= fun tree ->
  let info = Irmin.Info.v ~date:(Time.to_seconds timestamp) ~author message in
  let o_tree = match tree with `Node n -> n | _ -> assert false in
  let new_map = Hack.Map (Hack.StepMap.add "data" (`Node data_tree) map) in
  let node = Hack.hash_node new_map in
  let commit = P.Commit.Val.v ~parents ~node ~info in
  let computed_context_hash = P.Commit.Key.digest P.Commit.Val.t commit in
  if Context_hash.equal expected_context_hash computed_context_hash then
    let mock_parents =
      List.map (fun h -> Mock.commit index.repo h commit) parents
    in
    let ctxt = {index; tree= GitStore.Tree.empty; parents= mock_parents} in
    set_test_chain ctxt test_chain
    >>= fun ctxt ->
    set_protocol ctxt protocol_hash
    >>= fun ctxt ->
    let data_t = `Node (Mock.node index.repo data_hash) in
    GitStore.Tree.add_tree ctxt.tree current_data_key data_t
    >>= fun new_tree ->
    GitStore.Commit.v ctxt.index.repo ~info ~parents:ctxt.parents new_tree
    >>= fun commit ->
    let ctxt_h = GitStore.Commit.hash commit in
    Lwt.return (Context_hash.equal ctxt_h expected_context_hash)
  else Lwt.return_false
*)

(* Context dumper *)

module Context_dumper = Context_dump.Make (Dumpable_context)
include Context_dumper

(* provides functions dump_contexts and restore_contexts *)

type error += Cannot_create_file of string

let () =
  register_error_kind `Permanent ~id:"context_dump.write.cannot_open"
    ~title:"Cannot open file for context dump" ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf ppf "@[Error while opening file for context dumping: %s@]"
        uerr )
    Data_encoding.(obj1 (req "context_dump_cannot_open" string))
    (function Cannot_create_file e -> Some e | _ -> None)
    (fun e -> Cannot_create_file e)

type error += Cannot_open_file of string

let () =
  register_error_kind `Permanent ~id:"context_dump.read.cannot_open"
    ~title:"Cannot open file for context restoring" ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf ppf
        "@[Error while opening file for context restoring: %s@]" uerr )
    Data_encoding.(obj1 (req "context_restore_cannot_open" string))
    (function Cannot_open_file e -> Some e | _ -> None)
    (fun e -> Cannot_open_file e)

type error += Suspicious_file of int

let () =
  register_error_kind `Permanent ~id:"context_dump.read.suspicious"
    ~title:"Suspicious file: data after end" ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf ppf
        "@[Remaining bytes in file after context restoring: %d@]" uerr )
    Data_encoding.(obj1 (req "context_restore_suspicious" int31))
    (function Suspicious_file e -> Some e | _ -> None)
    (fun e -> Suspicious_file e)

let dump_contexts idx block_headers ~filename =
  let file_init () =
    Lwt_unix.openfile filename Lwt_unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o666
    >>= return
  in
  Lwt.catch file_init (function
    | Unix.Unix_error (e, _, _) ->
        fail @@ Cannot_create_file (Unix.error_message e)
    | exc ->
        let msg =
          Printf.sprintf "unknown error: %s" (Printexc.to_string exc)
        in
        fail (Cannot_create_file msg) )
  >>=? fun fd -> dump_contexts_fd idx block_headers ~fd

let restore_contexts idx ~filename =
  let file_init () =
    Lwt_unix.openfile filename Lwt_unix.[O_RDONLY] 0o600 >>= return
  in
  Lwt.catch file_init (function
    | Unix.Unix_error (e, _, _) ->
        fail @@ Cannot_open_file (Unix.error_message e)
    | exc ->
        let msg =
          Printf.sprintf "unknown error: %s" (Printexc.to_string exc)
        in
        fail (Cannot_open_file msg) )
  >>=? fun fd ->
  Lwt.finalize
    (fun () ->
      restore_contexts_fd idx ~fd
      >>=? fun result ->
      Lwt_unix.lseek fd 0 Lwt_unix.SEEK_CUR
      >>= fun current ->
      Lwt_unix.fstat fd
      >>= fun stats ->
      let total = stats.Lwt_unix.st_size in
      if current = total then return result
      else fail @@ Suspicious_file (total - current) )
    (fun () -> Lwt_unix.close fd)

let close _ = Lwt.return ()

let gc _ ~roots:_ = Lwt.return ()
