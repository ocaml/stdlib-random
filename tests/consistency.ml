(* Check that only split is missing from implementation anterior to Random5 *)

module type r4 = module type of Random4
module type r5 = module type of Random5

module F(R4:r4)(R5:r5): r5 = struct
  module State = struct
     include R4.State
     let split x = x
  end
  include (R4: r4 with module State := State)
  let split () = State.split (get_state ())
end
