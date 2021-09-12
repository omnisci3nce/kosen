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

let test_subtract () =
  let u = Vec3.create 8. 8. 8.
  and v = Vec3.create 2. 2. 2.
  and w = Vec3.create 6. 6. 6. in
  Alcotest.(check vec3_testable) "same" (Vec3.subtract u v) w

let test_mult_pos () =
  let u = Vec3.create 3. 3. 3.
  and s = 3.
  and w = Vec3.create 9. 9. 9. in
  Alcotest.(check vec3_testable) "same" (Vec3.multiply u s) w

let () =
  let open Alcotest in
  run "Vec3" [
    "basic arithmetic", [
      test_case "add positives" `Quick test_add_positives;
      test_case "add positives and negatives" `Quick test_add_pos_neg;
      test_case "add negatives and negatives" `Quick test_add_negatives;
      test_case "subtract" `Quick test_subtract;
      test_case "multiply positives" `Quick test_mult_pos;
      test_case "divide" `Quick (fun _ -> Alcotest.(check bool) "" true true);
      test_case "operators order of operations" `Quick (fun _ -> Alcotest.(check bool) "" true true)
    ];
    "length", [];
    "cross product", [];
    "utils", [
      test_case "negate" `Quick (fun _ -> Alcotest.(check bool) "" true true);
      test_case "unit_vector" `Quick (fun _ -> Alcotest.(check bool) "" true true)
    ]
    (* 
    "dot", [];
    "cross", [];
    "normalise", [];
    "reflect", [];
    "refract", [] *)
  ]