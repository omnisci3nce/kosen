open Types

let write_color oc (color: color) samples_per_pixel =
  let scale = 1.0 /. samples_per_pixel in
  let r = color.x *. scale *. 255.999 |> Int.of_float in
  let g = color.y *. scale *. 255.999 |> Int.of_float in
  let b = color.z *. scale *. 255.999 |> Int.of_float in
  Printf.fprintf oc "%d %d %d\n" r g b
