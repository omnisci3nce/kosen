type color = Vec3.t

let write_color oc (color: color) =
  Printf.fprintf oc "%d %d %d\n" (Int.of_float(255.999 *. color.x)) (Int.of_float(255.999 *. color.y)) (Int.of_float(255.999 *. color.z))