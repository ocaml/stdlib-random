(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pseudo-random number generator
   This is a lagged-Fibonacci F(55, 24, +) with a modified addition
   function to enhance the mixing of bits.
   If we use normal addition, the low-order bit fails tests 1 and 7
   of the Diehard test suite, and bits 1 and 2 also fail test 7.
   If we use multiplication as suggested by Marsaglia, it doesn't fare
   much better.
   By mixing the bits of one of the numbers before addition (XOR the
   5 high-order bits into the low-order bits), we get a generator that
   passes all the Diehard tests.
*)

external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  type t = { st : int array; mutable idx : int }

  let new_state () = { st = Array.make 55 0; idx = 0 }
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx


  let serialization_prefix =
    "lfsr2:"
    (* "lfsr" denotes the algorithm currently in use, and '2' is
       a version number. Each Random algorithm or serialization format
       should have distinct prefix , so that users get a clean error
       instead of believing that they faithfully reproduce their
       previous state and in fact get a differrent stream.

       Note that there is no constraint to keep the same
       "<name><ver>:<data>" format or message size in future versions,
       we could change the format completely if we wanted as long
       as there is no confusion possible with the previous formats. *)

  let serialization_prefix_len =
    String.length serialization_prefix

(* Compatibility functions Imported from standard library *)
#if OCAML_VERSION < (4,13,0)
  let get_int32_le s off =
    let res = ref Int32.zero in
    for i = 3 downto 0 do
      let v = Int32.of_int (Char.code s.[off+i]) in
      res := Int32.(add v (shift_left !res 8))
    done;
    !res
  let get_int8 s off = Char.code s.[off]
  let starts_with ~prefix s =
    let open String in
    let len_s = length s
    and len_pre = length prefix in
    let rec aux i =
      if i = len_pre then true
      else if unsafe_get s i <> unsafe_get prefix i then false
      else aux (i + 1)
    in len_s >= len_pre && aux 0
#else
  let get_int32_le = String.get_int32_le
  let get_int8 = String.get_int8
  let starts_with = String.starts_with
#endif

  let to_binary_string s =
    let prefix = serialization_prefix in
    let preflen = serialization_prefix_len in
    let buf = Bytes.create (preflen + 55 * 4 + 1) in
    Bytes.blit_string prefix 0 buf 0 preflen;
    for i = 0 to 54 do
      Bytes.set_int32_le buf (preflen + i * 4)
        (Int32.of_int (s.st.(i) land 0x3FFFFFFF))
    done;
    Bytes.set_int8 buf (preflen + 55 * 4) s.idx;
    Bytes.unsafe_to_string buf

  let of_binary_string buf =
    let prefix = serialization_prefix in
    let preflen = serialization_prefix_len in
    if String.length buf <> preflen + 1 + 55 * 4
       || not (starts_with ~prefix buf)
    then
      failwith
        ("Random4.State.of_binary_string: expected a format \
          compatible with Random4 PRNG");
    let st = new_state () in
    for i=0 to 54 do
      let n = get_int32_le buf (preflen + i * 4) in
      st.st.(i) <- Int32.(to_int @@ logand n 0x3FFFFFFFl)
    done;
    st.idx <- get_int8 buf (preflen + 55 * 4);
    st

  let full_init s seed =
    let combine accu x = Digest.string (accu ^ Int.to_string x) in
    let extract d =
      Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
      + (Char.code d.[3] lsl 24)
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
      s.st.(j) <- (s.st.(j) lxor extract !accu) land 0x3FFFFFFF;  (* PR#5575 *)
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


  let min_int31 = -0x4000_0000
      (* = -2{^30}, which is [min_int] for 31-bit integers *)
  let max_int31 = 0x3FFF_FFFF
      (* =  2{^30}-1, which is [max_int] for 31-bit integers *)
  (* avoid integer literals for these, 32-bit OCaml would reject them: *)
  let min_int32 = -(1 lsl 31)
      (* = -0x8000_0000 on platforms where [Sys.int_size >= 32] *)
  let max_int32 = (1 lsl 31) - 1
      (* =  0x7FFF_FFFF on platforms where [Sys.int_size >= 32] *)

  (* Return 30 random bits as an integer 0 <= x < 2^30 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let curval = s.st.(s.idx) in
    let newval = s.st.((s.idx + 24) mod 55)
                 + (curval lxor ((curval lsr 25) land 0x1F)) in
    let newval30 = newval land 0x3FFFFFFF in  (* PR#5575 *)
    s.st.(s.idx) <- newval30;
    newval30

 let bits32 s =
    let b1 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    let b2 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    Int32.(logor b1 (shift_left b2 16))

  let bits64 s =
    let b1 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b2 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b3 = Int64.(shift_right_logical (of_int (bits s)) 8) in  (* 22 bits *)
    Int64.(logor b1 (logor (shift_left b2 21) (shift_left b3 42)))

  let rec int31aux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > max_int31 - n + 1 then int31aux s n else v

  (* Return an integer between 0 (included) and [bound] (excluded).
     The bound must fit in 31-bit signed integers.
     This function yields the same output regardless of the integer size. *)
  let int s bound =
    if bound > max_int31 || bound <= 0
    then invalid_arg "Random.int"
    else int31aux s bound

  let rec int63aux s n =
    let b1 = bits s in
    let b2 = bits s in
    let (r, max_int) =
      if n <= max_int32 then
        (* 31 random bits on both 64-bit OCaml and JavaScript.
           Use upper 15 bits of b1 and 16 bits of b2. *)
        let bpos =
          (((b2 land 0x3FFFC000) lsl 1) lor (b1 lsr 15))
        in
          (bpos, max_int32)
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
      int31aux s bound

 (* Return an integer between 0 (included) and [n] (excluded).
     [bound] may be any positive [int].  [mask] must be of the form [2{^i}-1]
     and greater or equal to [n].  Larger values of [mask] make the function
     run faster (fewer samples are rejected).  Smaller values of [mask]
     are usable on a wider range of OCaml implementations.  *)
  let rec int_aux s n mask =
    (* We start by drawing a non-negative integer in the [ [0, mask] ] range *)
    let r = Int64.to_int (bits64 s) land mask in
    let v = r mod n in
    (* For uniform distribution of the result between 0 included and [n]
     * excluded, the random number [r] must have been drawn uniformly in
     * an interval whose length is a multiple of [n]. To achieve this,
     * we use rejection sampling on the greatest interval [ [0, k*n-1] ]
     * that fits in [ [0, mask] ].  That is, we reject the
     * sample if it falls outside of this interval, and draw again.
     * This is what the test below does, while carefully avoiding
     * overflows and sparing a division [mask / n]. *)
    if r - v > mask - n + 1 then int_aux s n mask else v


(* Return an integer between [min] (included) and [max] (included).
     The [nbits] parameter is the size in bits of the signed integers
     we draw from [s].
     We must have [-2{^nbits - 1} <= min <= max < 2{^nbits - 1}].
     Moreover, for the iteration to converge quickly, the interval
     [[min, max]] should have width at least [2{^nbits - 1}].
     As the width approaches this lower limit, the average number of
     draws approaches 2, with a quite high standard deviation (2 + epsilon). *)
  let rec int_in_large_range s ~min ~max ~nbits =
    let drop = Sys.int_size - nbits in
    (* The bitshifts replicate the [nbits]-th bit (sign bit) to higher bits: *)
    let r = ((Int64.to_int (bits64 s)) lsl drop) asr drop in
    if r < min || r > max then int_in_large_range s ~min ~max ~nbits else r

  (* Return an integer between [min] (included) and [max] (included).
     [mask] is as described for [int_aux].
     [nbits] is as described for [int_in_large_range]. *)
  let int_in_range_aux s ~min ~max ~mask ~nbits =
    let span = max - min + 1 in
    if span <= mask (* [span] is small enough *)
    && span > 0     (* no overflow occurred when computing [span] *)
    then
      (* Just draw a number in [[0, span)] and shift it by [min]. *)
      min + int_aux s span mask
    else
      (* Span too large, use the alternative drawing method. *)
      int_in_large_range s ~min ~max ~nbits

  (* 31bits version to account for the 31 bit PRNG *)
  let rec int31_in_large_range s ~min ~max =
    let r = Int32.to_int (bits32 s) in
    if r < min || r > max then int31_in_large_range s ~min ~max else r

  let int31_in_range_aux s ~min ~max =
    let mask = max_int31 in
    let span = max - min + 1 in
    if span <= mask (* [span] is small enough *)
    && span > 0     (* no overflow occurred when computing [span] *)
    then
      (* Just draw a number in [[0, span)] and shift it by [min]. *)
      min + int31aux s span
    else
      (* Span too large, use the alternative drawing method. *)
      int31_in_large_range s ~min ~max


  (* Return an integer between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let int_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int_in_range";
    (* When both bounds fit in 31-bit signed integers, we use parameters
       [mask] and [nbits] appropriate for 31-bit integers, so as to
       yield the same output on all platforms supported by OCaml.
       When both bounds fit in 32-bit signed integers, we use parameters
       [mask] and [nbits] appropriate for 32-bit integers, so as to
       yield the same output on JavaScript and on 64-bit OCaml. *)
    if min >= min_int31 && max <= max_int31 then
      int31_in_range_aux s ~min ~max
    else if min >= min_int32 && max <= max_int32 then
      int_in_range_aux s ~min ~max ~mask:max_int32 ~nbits:32
    else
      int_in_range_aux s ~min ~max ~mask:max_int ~nbits:Sys.int_size

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

  (* Return an [int32] between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let rec int32_in_range_aux s ~min ~max =
    let r = bits32 s in
    if r < min || r > max then int32_in_range_aux s ~min ~max else r

  let int32_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int32_in_range"
    else
      let span = Int32.succ (Int32.sub max min) in
      (* Explanation of this test: see comment in [int_in_range_aux]. *)
      if span <= Int32.zero then
        int32_in_range_aux s ~min ~max
      else
        Int32.add min (int32aux s span)

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

  (* Return an [int64] between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let rec int64_in_range_aux s ~min ~max =
    let r = bits64 s in
    if r < min || r > max then int64_in_range_aux s ~min ~max else r

  let int64_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int64_in_range"
    else
      let span = Int64.succ (Int64.sub max min) in
      (* Explanation of this test: see comment in [int_in_range_aux]. *)
      if span <= Int64.zero then
        int64_in_range_aux s ~min ~max
      else
        Int64.add min (int64aux s span)

  (* Return 32 or 64 random bits as a [nativeint] *)
  let nativebits =
    if Nativeint.size = 32
    then fun s -> Nativeint.of_int32 (bits32 s)
    else fun s -> Int64.to_nativeint (bits64 s)

  (* Return a [nativeint] between 0 (included) and [bound] (excluded). *)
  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))

  (* Return a [nativeint] between [min] (included) and [max] (included). *)
  let nativeint_in_range =
    if Nativeint.size = 32
    then fun s ~min ~max ->
      Nativeint.of_int32 (int32_in_range s
        ~min:(Nativeint.to_int32 min) ~max:(Nativeint.to_int32 max))
    else fun s ~min ~max ->
      Int64.to_nativeint (int64_in_range s
        ~min:(Int64.of_nativeint min) ~max:(Int64.of_nativeint max))

  (* Returns a float 0 <= x <= 1 with at most 60 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0  (* 2^30 *)
    and r1 = Stdlib.float (bits s)
    and r2 = Stdlib.float (bits s)
    in (r1 /. scale +. r2) /. scale


  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)

end

(* This is the state you get with [init 27182818] and then applying
   the "land 0x3FFFFFFF" filter to them.  See #5575, #5793, #5977. *)
let default = {
  State.st = [|
      0x3ae2522b; 0x1d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x3b086c47;
      0x16d467d6; 0x101d91c7; 0x321df177; 0x0176c193; 0x1ff72bf1; 0x1e889109;
      0x0b464b18; 0x2b86b97c; 0x0891da48; 0x03137463; 0x085ac5a1; 0x15d61f2f;
      0x3bced359; 0x29c1c132; 0x3a86766e; 0x366d8c86; 0x1f5b6222; 0x3ce1b59f;
      0x2ebf78e1; 0x27cd1b86; 0x258f3dc3; 0x389a8194; 0x02e4c44c; 0x18c43f7d;
      0x0f6e534f; 0x1e7df359; 0x055d0b7e; 0x10e84e7e; 0x126198e4; 0x0e7722cb;
      0x1cbede28; 0x3391b964; 0x3d40e92a; 0x0c59933d; 0x0b8cd0b7; 0x24efff1c;
      0x2803fdaa; 0x08ebc72e; 0x0f522e32; 0x05398edc; 0x2144a04c; 0x0aef3cbd;
      0x01ad4719; 0x35b93cd6; 0x2a559d4f; 0x1e6fd768; 0x26e27f36; 0x186f18c3;
      0x2fbf967a;
    |];
  State.idx = 0;
}

let bits () = State.bits default
let int bound = State.int default bound
let full_int bound = State.full_int default bound
let int_in_range ~min ~max = State.int_in_range default ~min ~max
let int32 bound = State.int32 default bound
let int32_in_range ~min ~max = State.int32_in_range default ~min ~max
let nativeint bound = State.nativeint default bound
let nativeint_in_range ~min ~max = State.nativeint_in_range default ~min ~max
let int64 bound = State.int64 default bound
let int64_in_range ~min ~max = State.int64_in_range default ~min ~max
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
- : float * float * float = (936.754446796632465, 997.5, 1063.24555320336754)
# - : float * float * float = (80., 89.7400000000052387, 120.)
# - : float * float * float = (4858.57864376269, 5045.5, 5141.42135623731)
# - : float * float * float =
(936.754446796632465, 944.805999999982305, 1063.24555320336754)
# - : float * float * float = (960., 1019.19744000000355, 1088.)
# - : float * float * float = (960., 1059.31776000000536, 1088.)
# - : float * float * float = (960., 1039.98463999999512, 1088.)
# - : float * float * float = (960., 1054.38207999999577, 1088.)
# - : float * float * float = (80., 90.096000000005, 120.)
# - : float * float * float = (960., 1076.78720000000612, 1088.)
# - : float * float * float = (80., 85.1760000000067521, 120.)
# - : float * float * float = (80., 85.2160000000003492, 120.)
# - : float * float * float = (80., 80.6220000000030268, 120.)

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
