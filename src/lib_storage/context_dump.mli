(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

module type Dump_interface = sig
  type index

  type context

  type tree

  val pp_tree : tree Fmt.t

  type hash

  type step = string

  type key = step list

  type commit_info

  type batch

  val batch : index -> (batch -> 'a Lwt.t) -> 'a Lwt.t

  module Block_header : sig
    type t

    val to_bytes : t -> MBytes.t

    val of_bytes : MBytes.t -> t option

    val equal : t -> t -> bool
  end

  module Pruned_block : sig
    type t

    val to_bytes : t -> MBytes.t

    val of_bytes : MBytes.t -> t option
  end

  module Block_data : sig
    type t

    val to_bytes : t -> MBytes.t

    val of_bytes : MBytes.t -> t option
  end

  module Protocol_data : sig
    type t

    val to_bytes : t -> MBytes.t

    val of_bytes : MBytes.t -> t option
  end

  module Commit_hash : sig
    type t

    val to_bytes : t -> MBytes.t

    val of_bytes : MBytes.t -> t tzresult
  end

  (* hash manipulation *)
  val hash_export : hash -> string

  val hash_import : string -> hash tzresult

  val hash_equal : hash -> hash -> bool

  (* commit manipulation (for parents) *)
  val context_parents : context -> Commit_hash.t list

  (* Commit info *)
  val context_info : context -> commit_info

  val context_info_export : commit_info -> Int64.t * string * string

  val context_info_import : Int64.t * string * string -> commit_info

  (* block header manipulation *)
  val get_context : index -> Block_header.t -> context option Lwt.t

  val set_context :
       info:commit_info
    -> parents:Commit_hash.t list
    -> context
    -> Block_header.t
    -> Block_header.t option Lwt.t

  (* for dumping *)
  val context_tree : context -> tree

  val tree_hash : tree -> hash

  val sub_tree : tree -> key -> tree option Lwt.t

  val tree_list : tree -> (step * [`Contents | `Node]) list Lwt.t

  val tree_content : tree -> MBytes.t option Lwt.t

  (* for restoring *)
  val make_context : index -> context

  val update_context : context -> tree -> context

  val add_mbytes : batch -> tree -> key -> MBytes.t -> (hash * tree) Lwt.t

  val add_dir :
       index
    -> batch
    -> tree
    -> key
    -> (step * [`Blob | `Node] * hash) list
    -> (hash * tree) option Lwt.t
end

module type S = sig
  type index

  type context

  type block_header

  type block_data

  type pruned_block

  type protocol_data

  val dump_contexts_fd :
       index
    -> (block_header * block_data * pruned_block list * protocol_data list)
       list
    -> fd:Lwt_unix.file_descr
    -> unit tzresult Lwt.t

  val restore_contexts_fd :
       index
    -> fd:Lwt_unix.file_descr
    -> (block_header * block_data * pruned_block list * protocol_data list)
      list
      tzresult
      Lwt.t
end

module Make (I : Dump_interface) :
  S
  with type index := I.index
   and type context := I.context
   and type block_header := I.Block_header.t
   and type block_data := I.Block_data.t
   and type pruned_block := I.Pruned_block.t
   and type protocol_data := I.Protocol_data.t
