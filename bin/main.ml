open Kosen

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

let ray_color (r: Ray.t) =
  let s = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 in
  let t = Sphere.hit_sphere s r in
  if t > 0. then
    let n = Vec3.unit_vector (Vec3.subtract (Ray.at t r) (Vec3.create 0. 0. (-1.))) in
    Vec3.multiply (Vec3.create (n.x +. 1.) (n.y +. 1.) (n.z +. 1.)) 0.5
  else
    let unit_direction = Vec3.unit_vector r.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    Vec3.(
      ((create 1. 1. 1.) *| (1.0 -. t)) +| ((create 0.5 0.7 1.0) *| t)
    )

let () = 
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;
  for j = (image_height - 1) downto 0 do
    for i = 0 to (image_width - 1) do
    let open Vec3 in
      let u =  Float.of_int(i) /. (Float.of_int(image_width) -. 1.0)
      and v =  Float.of_int(j) /. (Float.of_int(image_height) -. 1.0) in
      let r =
        Ray.create
          origin
          (lower_left_corner +| (horizontal *| u) +| (vertical *| v) -| origin) in
      let color = ray_color r in
      Color.write_color oc color
    done
  done;

  close_out oc;