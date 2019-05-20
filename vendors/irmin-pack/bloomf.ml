(* inspired from
   https://github.com/travisbrady/flajolet/blob/master/lib/bloom.ml
   and
   https://github.com/sergezloto/ocaml-bloom-filter/blob/master/bloomf.ml*)

module type HASH = sig
  type t

  val hash : t -> int
end

module type S = sig
  type elt

  type t

  val create : ?error_rate:float -> int -> t

  val add : t -> elt -> unit

  val mem : t -> elt -> bool

  val clear : t -> unit
end

module Bitarray = struct
  type t = char array (* Note: byte array available from 4.02.0 *)

  let zero = '\x00'

  let create nbits =
    (* Adjust to next byte boundary. This way no bits wasted! *)
    let nbytes = (nbits + 7) / 8 in
    Array.make nbytes zero

  let reset b = Array.fill b 0 (Array.length b) zero

  let index i = (i asr 3, i land 7)

  (* that is,  (i/8, i%8) *)

  let tbit byte bit =
    let byte = int_of_char byte in
    byte land (1 lsl bit) <> 0

  (* test bit in byte *)

  let sbit byte bit =
    let byte = int_of_char byte in
    char_of_int (byte lor (1 lsl bit))

  (* set bit in byte *)

  let get b i =
    let offset, bit = index i in
    let byte = b.(offset) in
    tbit byte bit

  let set (b : t) i =
    let offset, bit = index i in
    let byte = b.(offset) in
    (* Check whether it was set *)
    if tbit byte bit then (* Was set, no need to update value *)
      ()
    else
      let byte = sbit byte bit in
      b.(offset) <- byte
end

module Make (H : HASH) = struct
  type elt = H.t

  let log2 = log 2.0

  let log2sq = log2 ** 2.0

  type t = { m : int; k : int; b : Bitarray.t }

  let v m k = { m; k; b = Bitarray.create m }

  let base_hashes data =
    Array.map (fun x -> Hashtbl.seeded_hash x (H.hash data)) [| 0; 1; 2; 3 |]

  let location t h i =
    let loc =
      (h.(i mod 2) + (i * h.(2 + ((i + (i mod 2)) mod 4 / 2)))) mod t.m
    in
    abs loc

  let estimate_parameters n p =
    let nf = float_of_int n in
    let m = ceil (-1.0 *. nf *. log p /. log2sq) in
    let k = ceil (log2 *. m /. nf) in
    (m, k)

  let create ?(error_rate = 0.01) n_items =
    let m, k = estimate_parameters n_items error_rate in
    v (int_of_float m) (int_of_float k)

  let add t data =
    let h = base_hashes data in
    for i = 0 to t.k - 1 do
      let loc = location t h i in
      (*Bitarray.set t.b loc true*)
      Bitarray.set t.b loc
    done

  let mem t data =
    let h = base_hashes data in
    let rec aux res i =
      if i = t.k then res
      else if not res then res
      else
        let loc = location t h i in
        let res = Bitarray.get t.b loc in
        aux res (i + 1)
    in
    aux true 0

  let clear bf = Bitarray.reset bf.b
end
