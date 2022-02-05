open Types

let clamp min max x = if x < min then min else if x > max then max else x

let color_255_from_float f = 
  Float.to_int (255.999 *. f)

let write_color oc (color: color) samples_per_pixel =
  let scale = 1.0 /. (Float.of_int samples_per_pixel) in
  let r = color.x *. scale |> sqrt |> clamp 0.0 0.999 |> color_255_from_float in
  let g = color.y *. scale |> sqrt |> clamp 0.0 0.999 |> color_255_from_float in
  let b = color.z *. scale |> sqrt |> clamp 0.0 0.999 |> color_255_from_float in
  Printf.fprintf oc "%d %d %d\n" r g b
