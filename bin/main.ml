open Kosen
open Kosen.Defaultscene

(* if (world.hit(r, 0, infinity, rec)) {
  return 0.5 * (rec.normal + color(1,1,1));
} *)

let ray_color (r: Ray.t) scene =
  let t = Scene.hit r scene
  ()
  (* let s = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 in
  let t = Sphere.hit r s in
  match t with
  | None ->
    let unit_direction = Vec3.unit_vector r.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    Vec3.(((create 1. 1. 1.) *| (1.0 -. t)) +| ((create 0.5 0.7 1.0) *| t)
  )
  | Some hit_rec ->
    Vec3.( (hit_rec.normal +| (Vec3.create 1. 1. 1.)) *| 0.5 ) *)
    
    (* hittable_list world;
    world.add(make_shared<sphere>(point3(0,0,-1), 0.5));
    world.add(make_shared<sphere>(point3(0,-100.5,-1), 100)); *)

let ground = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.
let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
let world = [ground; sphere1]

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
      let color = ray_color r world in
      Color.write_color oc color
    done
  done;

  close_out oc;