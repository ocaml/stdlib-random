module type state = sig
  type t
  val to_binary_string : t -> string
  val of_binary_string : string -> t
end

module type random = sig
  module State: state
  val init: int -> unit
  val get_state: unit -> State.t
end

let test name (module R: random) =
  let init n = R.init n; R.get_state () in

  let roundtrip seed =
    let s = init seed in
    s = R.State.(of_binary_string @@ to_binary_string s)
  in
  if not @@ List.for_all roundtrip (List.init 100 Fun.id) then
    failwith (Format.asprintf "Roundtrip failed for %s generator" name)

let () =
  test "random3" (module Random3);
  test "random4" (module Random4);
  test "random5" (module Random5);
  test "random5o" (module Random5o)
