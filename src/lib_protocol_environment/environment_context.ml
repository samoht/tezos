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

open Error_monad

module type CONTEXT = Environment_context_intf.S

module type VIEW = Environment_context_intf.VIEW

module Witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option
end = struct
  type (_, _) eq = Refl : ('a, 'a) eq

  type _ equality = ..

  module type Inst = sig
    type t

    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a

      type _ equality += Eq : t equality
    end in
    (module Inst)

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

module Context = struct
  type key = string list

  type value = Bytes.t

  type ('ctxt, 'tree) ops =
    (module CONTEXT with type t = 'ctxt and type tree = 'tree)

  type _ kind = ..

  type ('a, 'b) witness = 'a Witness.t * 'b Witness.t

  let witness () = (Witness.make (), Witness.make ())

  let equiv (a, b) (c, d) = (Witness.eq a c, Witness.eq b d)

  type t =
    | Context : {
        kind : 'a kind;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        wit : ('a, 'b) witness;
      }
        -> t

  let mem (Context {ops = (module Ops); ctxt; _}) key = Ops.mem ctxt key

  let add (Context ({ops = (module Ops); ctxt; _} as c)) key value =
    Ops.add ctxt key value >|= fun ctxt -> Context {c with ctxt}

  let find (Context {ops = (module Ops); ctxt; _}) key = Ops.find ctxt key

  let remove (Context ({ops = (module Ops); ctxt; _} as c)) key =
    Ops.remove ctxt key >|= fun ctxt -> Context {c with ctxt}

  (* trees *)
  type tree =
    | Tree : {ops : ('a, 'b) ops; tree : 'b; wit : ('a, 'b) witness} -> tree

  let mem_tree (Context {ops = (module Ops); ctxt; _}) key =
    Ops.mem_tree ctxt key

  let add_tree (Context ({ops = (module Ops); ctxt; _} as c)) key
      (Tree {tree; wit; _}) =
    match equiv c.wit wit with
    | (Some Refl, Some Refl) ->
        Ops.add_tree ctxt key tree >|= fun ctxt -> Context {c with ctxt}
    | _ ->
        assert false

  let find_tree (Context {ops = (module Ops) as ops; ctxt; wit; _}) key =
    Ops.find_tree ctxt key >|= Option.map (fun tree -> Tree {ops; tree; wit})

  let fold ?depth (Context {ops = (module Ops) as ops; ctxt; wit; _}) key ~init
      ~value ~tree =
    Ops.fold ?depth ctxt key ~init ~value ~tree:(fun k v acc ->
        let v = Tree {ops; tree = v; wit} in
        tree k v acc)

  (* Tree *)
  module Tree = struct
    let empty (Context {ops = (module Ops) as ops; wit; ctxt; _}) =
      Tree {ops; wit; tree = Ops.Tree.empty ctxt}

    let is_empty (Tree {ops = (module Ops); tree; _}) = Ops.Tree.is_empty tree

    let mem (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.mem tree key

    let add (Tree ({ops = (module Ops); tree; _} as c)) key value =
      Ops.Tree.add tree key value >|= fun tree -> Tree {c with tree}

    let find (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.find tree key

    let mem_tree (Tree {ops = (module Ops); tree; _}) key =
      Ops.Tree.mem_tree tree key

    let add_tree (Tree ({ops = (module Ops); tree; wit; _} as c)) key
        (Tree {tree = tree2; wit = wit2; _}) =
      match equiv wit wit2 with
      | (Some Refl, Some Refl) ->
          Ops.Tree.add_tree tree key tree2 >|= fun tree -> Tree {c with tree}
      | _ ->
          assert false

    let find_tree (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.find_tree tree key
      >|= Option.map (fun tree -> Tree {c with tree})

    let remove (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.remove tree key >|= fun tree -> Tree {c with tree}

    let fold ?depth (Tree {ops = (module Ops) as ops; tree = t; wit; _}) key
        ~init ~value ~tree =
      Ops.Tree.fold ?depth t key ~init ~value ~tree:(fun k v acc ->
          let v = Tree {ops; tree = v; wit} in
          tree k v acc)
  end

  (* misc *)

  let current_protocol_key = ["protocol"]

  let set_protocol (Context ({ops = (module Ops); ctxt; _} as c)) protocol_hash
      =
    Ops.add ctxt current_protocol_key (Protocol_hash.to_bytes protocol_hash)
    >>= fun ctxt -> Lwt.return (Context {c with ctxt})

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}
