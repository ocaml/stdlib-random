(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* "Linear feedback shift register" pseudo-random number generator. *)
(* References: Robert Sedgewick, "Algorithms", Addison-Wesley *)

(* The PRNG is a linear feedback shift register.
   It is seeded by a MD5-based PRNG.
*)

external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  type t = { st : int array; mutable idx : int }

  let new_state () = { st = Array.make 55 0; idx = 0 }
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx


  let full_init s seed =
    let combine accu x = Digest.string (accu ^ Int.to_string x) in
    let extract d =
      (Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16))
      lxor (Char.code d.[3] lsl 22)
    in
    let seed = if Array.length seed = 0 then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- s.st.(j) lxor extract !accu;
    done;
    s.idx <- 0


  let make seed =
    let result = new_state () in
    full_init result seed;
    result


  let make_self_init () = make (random_seed ())

  let copy s =
    let result = new_state () in
    assign result s;
    result


  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let newval = (s.st.((s.idx + 24) mod 55) + s.st.(s.idx)) land 0x3FFFFFFF in
    s.st.(s.idx) <- newval;
    newval


  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

  let rec int63aux s n =
    let max_int_32 = (1 lsl 30) + 0x3FFFFFFF in (* 0x7FFFFFFF *)
    let b1 = bits s in
    let b2 = bits s in
    let (r, max_int) =
      if n <= max_int_32 then
        (* 31 random bits on both 64-bit OCaml and JavaScript.
           Use upper 15 bits of b1 and 16 bits of b2. *)
        let bpos =
          (((b2 land 0x3FFFC000) lsl 1) lor (b1 lsr 15))
        in
          (bpos, max_int_32)
      else
        let b3 = bits s in
        (* 62 random bits on 64-bit OCaml; unreachable on JavaScript.
           Use upper 20 bits of b1 and 21 bits of b2 and b3. *)
        let bpos =
          ((((b3 land 0x3FFFFE00) lsl 12) lor (b2 lsr 9)) lsl 20)
            lor (b1 lsr 10)
        in
          (bpos, max_int)
    in
    let v = r mod n in
    if r - v > max_int - n + 1 then int63aux s n else v

  let full_int s bound =
    if bound <= 0 then
      invalid_arg "Random.full_int"
    else if bound > 0x3FFFFFFF then
      int63aux s bound
    else
      intaux s bound


  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound


  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound


  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))

  (* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0
    and r0 = float_of_int (bits s)
    and r1 = float_of_int (bits s)
    and r2 = float_of_int (bits s)
    in ((r0 /. scale +. r1) /. scale +. r2) /. scale



  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)

  let bits32 s =
    let b1 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    let b2 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    Int32.(logor b1 (shift_left b2 16))

  let bits64 s =
    let b1 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b2 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b3 = Int64.(shift_right_logical (of_int (bits s)) 8) in  (* 22 bits *)
    Int64.(logor b1 (logor (shift_left b2 21) (shift_left b3 42)))

  let nativebits =
    if Nativeint.size = 32
    then fun s -> Nativeint.of_int32 (bits32 s)
    else fun s -> Int64.to_nativeint (bits64 s)

end

(* This is the state you get with [init 27182818] on a 32-bit machine. *)
let default = {
  State.st = [|
      509760043; 399328820; 99941072; 112282318; 611886020; 516451399;
      626288598; 337482183; 748548471; 808894867; 657927153; 386437385;
      42355480; 977713532; 311548488; 13857891; 307938721; 93724463;
      1041159001; 444711218; 1040610926; 233671814; 664494626; 1071756703;
      188709089; 420289414; 969883075; 513442196; 275039308; 918830973;
      598627151; 134083417; 823987070; 619204222; 81893604; 871834315;
      398384680; 475117924; 520153386; 324637501; 38588599; 435158812;
      168033706; 585877294; 328347186; 293179100; 671391820; 846150845;
      283985689; 502873302; 718642511; 938465128; 962756406; 107944131;
      192910970;
    |];
  State.idx = 0;
}

let bits () = State.bits default
let int bound = State.int default bound
let full_int bound = State.full_int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default
let bits32 () = State.bits32 default
let bits64 () = State.bits64 default
let nativebits () = State.nativebits default

let full_init seed = State.full_init default seed
let init seed = State.full_init default [| seed |]
let self_init () = full_init (random_seed())

(* Manipulating the current state. *)

let get_state () = State.copy default
let set_state s = State.assign default s

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000
init 27182818; chisquare int 100000 100
init 27182818; chisquare int 100000 5000
init 27182818; chisquare int 1000000 1000
init 27182818; chisquare int 100000 1024
init 299792643; chisquare int 100000 1024
init 14142136; chisquare int 100000 1024
init 27182818; init_diff 1024; chisquare diff 100000 1024
init 27182818; init_diff 100; chisquare diff 100000 100
init 27182818; init_diff2 1024; chisquare diff2 100000 1024
init 27182818; init_diff2 100; chisquare diff2 100000 100
init 14142136; init_diff2 100; chisquare diff2 100000 100
init 299792643; init_diff2 100; chisquare diff2 100000 100
- : float * float * float = (936.754446796632465, 1032., 1063.24555320336754)
# - : float * float * float = (80., 91.3699999999953434, 120.)
# - : float * float * float = (4858.57864376269026, 4982., 5141.42135623730974)
# - : float * float * float =
(936.754446796632465, 1017.99399999994785, 1063.24555320336754)
# - : float * float * float = (960., 984.565759999997681, 1088.)
# - : float * float * float = (960., 1003.40735999999742, 1088.)
# - : float * float * float = (960., 1035.23328000000038, 1088.)
# - : float * float * float = (960., 1026.79551999999967, 1088.)
# - : float * float * float = (80., 110.194000000003143, 120.)
# - : float * float * float = (960., 1067.98080000000482, 1088.)
# - : float * float * float = (80., 107.292000000001281, 120.)
# - : float * float * float = (80., 85.1180000000022119, 120.)
# - : float * float * float = (80., 86.614000000001397, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Stdlib.float v.(i0) *. Stdlib.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1


let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Stdlib.float r
  and n = Stdlib.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)


(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0
let init_diff r = st := int r
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2


let st1 = ref 0
and st2 = ref 0


(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r


********************)
