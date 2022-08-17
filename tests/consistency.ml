(* Check that only split is missing from implementations anterior to Random5 *)

module type r3 = module type of Random3
module type r4 = module type of Random4
module type r5 = module type of Random5
module type r5o = module type of Random5o

module R5_to_R4(R5:r5): r4 = R5

module R4_to_R5(R4:r4): r5 = struct
  module State = struct
     include R4.State
     let split x = x
  end
  include (R4: r4 with module State := State)
  let split () = State.split (get_state ())
end


module R5o_equal_R5:sig module type t = r5 end = struct
  module type t = r5o
end

module R3_equal_R4:sig module type t = r3 end = struct
  module type t = r4
end

#if OCAML_VERSION > (5,0,0)
module R5_equal_Stdlib: sig module type t = r5 end = struct
  module type t = module type of Random
end
#endif
