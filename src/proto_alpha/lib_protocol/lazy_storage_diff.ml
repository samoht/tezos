(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module type OPS = sig
  type alloc

  type updates

  val title : string

  val alloc_encoding : alloc Data_encoding.t

  val updates_encoding : updates Data_encoding.t

  val bytes_size_for_empty : Z.t

  val alloc : Raw_context.t -> id:Z.t -> alloc -> Raw_context.t tzresult Lwt.t

  val apply_updates :
    Raw_context.t -> id:Z.t -> updates -> (Raw_context.t * Z.t) tzresult Lwt.t

  module Total_bytes : sig
    val init : Raw_context.t -> Z.t -> Z.t -> Raw_context.t tzresult Lwt.t

    val get : Raw_context.t -> Z.t -> Z.t tzresult Lwt.t

    val set : Raw_context.t -> Z.t -> Z.t -> Raw_context.t tzresult Lwt.t
  end

  val copy :
    Raw_context.t -> from:Z.t -> to_:Z.t -> Raw_context.t tzresult Lwt.t

  val remove_rec : Raw_context.t -> Z.t -> Raw_context.t Lwt.t
end

module Big_map = struct
  include Lazy_storage_kind.Big_map

  let bytes_size_for_big_map_key = 65

  let bytes_size_for_empty =
    let bytes_size_for_big_map = 33 in
    Z.of_int bytes_size_for_big_map

  let alloc ctxt ~id {key_type; value_type} =
    (* Annotations are erased to allow sharing on [Copy]. The types from the
       contract code are used, these ones are only used to make sure they are
       compatible during transmissions between contracts, and only need to be
       compatible, annotations nonwhistanding. *)
    let key_type =
      Micheline.strip_locations
        (Script_repr.strip_annotations (Micheline.root key_type))
    in
    let value_type =
      Micheline.strip_locations
        (Script_repr.strip_annotations (Micheline.root value_type))
    in
    Storage.Big_map.Key_type.init ctxt id key_type
    >>=? fun ctxt -> Storage.Big_map.Value_type.init ctxt id value_type

  let apply_update ctxt ~id
      { key = _key_is_shown_only_on_the_receipt_in_print_big_map_diff;
        key_hash;
        value } =
    match value with
    | None ->
        Storage.Big_map.Contents.remove (ctxt, id) key_hash
        >|=? fun (ctxt, freed, existed) ->
        let freed =
          if existed then freed + bytes_size_for_big_map_key else freed
        in
        (ctxt, Z.of_int ~-freed)
    | Some v ->
        Storage.Big_map.Contents.init_set (ctxt, id) key_hash v
        >|=? fun (ctxt, size_diff, existed) ->
        let size_diff =
          if existed then size_diff else size_diff + bytes_size_for_big_map_key
        in
        (ctxt, Z.of_int size_diff)

  let apply_updates ctxt ~id updates =
    fold_left_s
      (fun (ctxt, size) update ->
        apply_update ctxt ~id update
        >|=? fun (ctxt, added_size) -> (ctxt, Z.add size added_size))
      (ctxt, Z.zero)
      updates

  include Storage.Big_map
end

type ('alloc, 'updates) ops =
  (module OPS with type alloc = 'alloc and type updates = 'updates)

let get_ops : type a u. (a, u) Lazy_storage_kind.t -> (a, u) ops = function
  | Big_map ->
      (module Big_map)

type 'alloc init = Existing | Copy of {src : Z.t} | Alloc of 'alloc

type ('alloc, 'updates) diff =
  | Remove
  | Update of {init : 'alloc init; updates : 'updates}

let diff_encoding : type a u. (a, u) ops -> (a, u) diff Data_encoding.t =
 fun (module OPS) ->
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"update"
        (obj2
           (req "action" (constant "update"))
           (req "updates" OPS.updates_encoding))
        (function
          | Update {init = Existing; updates} -> Some ((), updates) | _ -> None)
        (fun ((), updates) -> Update {init = Existing; updates});
      case
        (Tag 1)
        ~title:"remove"
        (obj1 (req "action" (constant "remove")))
        (function Remove -> Some () | _ -> None)
        (fun () -> Remove);
      case
        (Tag 2)
        ~title:"copy"
        (obj3
           (req "action" (constant "copy"))
           (req "source" z)
           (req "updates" OPS.updates_encoding))
        (function
          | Update {init = Copy {src}; updates} ->
              Some ((), src, updates)
          | _ ->
              None)
        (fun ((), src, updates) -> Update {init = Copy {src}; updates});
      case
        (Tag 3)
        ~title:"alloc"
        (merge_objs
           (obj2
              (req "action" (constant "alloc"))
              (req "updates" OPS.updates_encoding))
           OPS.alloc_encoding)
        (function
          | Update {init = Alloc alloc; updates} ->
              Some (((), updates), alloc)
          | _ ->
              None)
        (fun (((), updates), alloc) -> Update {init = Alloc alloc; updates}) ]

let apply_updates :
    type a u.
    Raw_context.t ->
    (a, u) ops ->
    id:Z.t ->
    u ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt (module OPS) ~id updates ->
  OPS.apply_updates ctxt ~id updates
  >>=? fun (ctxt, updates_size) ->
  if Z.(equal updates_size zero) then return (ctxt, updates_size)
  else
    OPS.Total_bytes.get ctxt id
    >>=? fun size ->
    OPS.Total_bytes.set ctxt id (Z.add size updates_size)
    >|=? fun ctxt -> (ctxt, updates_size)

let apply_init :
    type a u.
    Raw_context.t ->
    (a, u) ops ->
    id:Z.t ->
    a init ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt (module OPS) ~id init ->
  match init with
  | Existing ->
      return (ctxt, Z.zero)
  | Copy {src} ->
      OPS.copy ctxt ~from:src ~to_:id
      >>=? fun ctxt ->
      OPS.Total_bytes.get ctxt src
      >>=? fun copy_size ->
      return (ctxt, Z.add copy_size OPS.bytes_size_for_empty)
  | Alloc alloc ->
      OPS.Total_bytes.init ctxt id Z.zero
      >>=? fun ctxt ->
      OPS.alloc ctxt id alloc
      >>=? fun ctxt -> return (ctxt, OPS.bytes_size_for_empty)

let apply_diff :
    type a u.
    Raw_context.t ->
    (a, u) ops ->
    id:Z.t ->
    (a, u) diff ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt ((module OPS) as ops) ~id diff ->
  match diff with
  | Remove ->
      OPS.Total_bytes.get ctxt id
      >>=? fun size ->
      OPS.remove_rec ctxt id
      >>= fun ctxt -> return (ctxt, Z.neg (Z.add size OPS.bytes_size_for_empty))
  | Update {init; updates} ->
      apply_init ctxt ops ~id init
      >>=? fun (ctxt, init_size) ->
      apply_updates ctxt ops ~id updates
      >>=? fun (ctxt, updates_size) ->
      return (ctxt, Z.add init_size updates_size)

type diffs_item =
  | E : ('a, 'u) Lazy_storage_kind.t * Z.t * ('a, 'u) diff -> diffs_item

let make :
    type a u. (a, u) Lazy_storage_kind.t -> Z.t -> (a, u) diff -> diffs_item =
 fun k id diff -> E (k, id, diff)

let item_encoding =
  let open Data_encoding in
  union
  @@ List.map
       (fun (tag, Lazy_storage_kind.E k) ->
         let ops = get_ops k in
         let (module OPS) = ops in
         let title = OPS.title in
         case
           (Tag tag)
           ~title
           (obj3
              (req "kind" (constant title))
              (req "id" z)
              (req "diff" (diff_encoding ops)))
           (fun (E (kind, id, diff)) ->
             match Lazy_storage_kind.eq k kind with
             | Some Eq ->
                 Some ((), id, diff)
             | None ->
                 None)
           (fun ((), id, diff) -> E (k, id, diff)))
       Lazy_storage_kind.all

type diffs = diffs_item list

let encoding =
  let open Data_encoding in
  def "lazy_storage_diff" @@ list item_encoding

let apply ctxt diffs =
  fold_left_s
    (fun (ctxt, total_size) (E (k, id, diff)) ->
      let ops = get_ops k in
      apply_diff ctxt ops id diff
      >|=? fun (ctxt, added_size) ->
      ( ctxt,
        if Compare.Z.(id < Z.zero) then total_size
        else Z.add total_size added_size ))
    (ctxt, Z.zero)
    diffs
