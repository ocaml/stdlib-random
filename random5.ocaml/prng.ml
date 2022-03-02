(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Florian Angeletti, projet Cambium, Inria                 *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let m = 0xd1342543de82ef95L
type state = (Int64.t, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

module Indices : sig
  type t = private int
  val  a : t
  val  s : t
  val x0 : t
  val x1 : t
  val (.!{}) : state -> t -> Int64.t
  val (.!{}<-) : state -> t -> Int64.t -> unit
end = struct
  type t = int
  let a = 0
  let s = 1
  let x0 = 2
  let x1 = 3
  let (.!{}) = Bigarray.Array1.unsafe_get
  let (.!{}<-) = Bigarray.Array1.unsafe_set
end
open Indices

open struct
  let ( + ) = Int64.add
  let ( * ) = Int64.mul
  let ( lxor ) = Int64.logxor
  let ( lsl ) = Int64.shift_left
  let ( lsr ) = Int64.shift_right_logical
  let ( lor ) = Int64.logor
end

let[@inline always] rotl x k = x lsl k lor x lsr (64-k)

let next st =
  (* Combining operation *)
  let z = st.!{s} + st.!{x0} in
  (* Mixing function *)
  let z = (z lxor (z lsr 32)) * 0xdaba0b6eb09322e3L in
  let z = (z lxor (z lsr 32)) * 0xdaba0b6eb09322e3L in
  let z = (z lxor (z lsr 32)) in
  (* LGC update *)
  st.!{s} <- st.!{s} * m + st.!{a};
  (* XBG update *)
  let q0 = st.!{x0} and q1 = st.!{x1} in
  let q1 = q0 lxor q1 in
  let q0 = rotl q0 24 in
  let q0 = q0 lxor q1 lxor (q1 lsl 16) in
  let q1 = rotl q1 37 in
  st.!{x0} <- q0; st.!{x1} <- q1;
  z
