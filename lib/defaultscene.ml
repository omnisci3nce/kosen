(* Setup *)
let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = Int.of_float(Float.of_int(image_width) /. aspect_ratio)
(* Camera *)
let viewport_height = 2.0
let viewport_width = aspect_ratio *. viewport_height
let focal_length = 1.0

let origin = Vec3.zero
let horizontal = Vec3.create viewport_width 0. 0.
let vertical = Vec3.create 0. viewport_height 0.
let lower_left_corner = Vec3.(origin -| (horizontal /| 2.) -| (vertical /| 2.) -| (Vec3.create 0. 0. focal_length))