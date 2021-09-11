open Kosen

let pprint_vec3 ppf (v :Vec3.t) =
  let open Vec3 in
  Fmt.pf ppf "%f %f %f\n" v.x v.y v.z

let vec3_eq a b = (a = b)

let vec3_testable = Alcotest.testable pprint_vec3 vec3_eq

let test_add_positives () =
  let u = Vec3.create 1. 1. 1.
  and v = Vec3.create 1. 1. 1.
  and w = Vec3.create 2. 2. 2. in
  Alcotest.(check vec3_testable) "same" (Vec3.add u v) w

let test_add_pos_neg () =
  let u = Vec3.create 1. 1. 1.
  and v = Vec3.create (-1.) (-1.) (-1.)
  and w = Vec3.create 0. 0. 0. in
  Alcotest.(check vec3_testable) "same" (Vec3.add u v) w

let test_add_negatives () =
  let u = Vec3.create (-1.) (-1.) (-1.)
  and v = Vec3.create (-1.) (-1.) (-1.)
  and w = Vec3.create (-2.) (-2.) (-2.) in
  Alcotest.(check vec3_testable) "same" (Vec3.add u v) w

let () =
  let open Alcotest in
  run "Vec3" [
    "add", [
      test_case "positives" `Quick test_add_positives;
      test_case "positives and negatives" `Quick test_add_pos_neg;
      test_case "negatives and negatives" `Quick test_add_negatives
    ];
    "subtract", [];
    "multiply", [];
    "divide", [];
    "length", [];
    "negate", [];
    "dot", [];
    "cross", [];
    "normalise", [];
    "reflect", [];
    "refract", []
  ]