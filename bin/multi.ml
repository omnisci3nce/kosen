open Kosen
open Kosen.Defaultscene

let ray_color (r: Ray.t) =
  let s = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 in
  let t = Sphere.hit s r 0.001 999999. in
  match t with
  | None -> let unit_direction = Vec3.unit_vector r.direction in
            let t = 0.5 *. (unit_direction.y +. 1.0) in
            Vec3.(
    ((create 1. 1. 1.) *| (1.0 -. t)) +| ((create 0.5 0.7 1.0) *| t)
  )
  | Some t ->
    let n = Vec3.unit_vector (Vec3.subtract (Ray.at t r) (Vec3.create 0. 0. (-1.))) in
    Vec3.multiply (Vec3.create (n.x +. 1.) (n.y +. 1.) (n.z +. 1.)) 0.5

let () = 
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;

  (* TODO: create domains *)

  close_out oc;