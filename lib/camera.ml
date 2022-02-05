type t = {
  origin: Vec3.t;
  horizontal: Vec3.t;
  vertical: Vec3.t;
  lower_left_corner: Vec3.t;
  (*u: Vec3.t; v: Vec3.t; w: Vec3.t;
  lens_radius: float*)
}

let radian_of_degree x = x *. Float.pi /. 180.

let rec random_in_unit_disk () =
  let p = Vec3.create (Base.Random.float_range (-1.) 1.) (Base.Random.float_range (-1.) 1.) 0. in
  if Vec3.length_squared p >= 1. then
    random_in_unit_disk ()
  else
    p

let create = 
  let aspect_ratio = 16.0 /. 9.0 in
  let viewport_height = 2.0 in
  let viewport_width = aspect_ratio *. viewport_height in
  let focal_length = 1.0 in
  let origin = Vec3.zero in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  let lower_left_corner = Vec3.(origin -| (horizontal /| 2.) -| (vertical /| 2.) -| (Vec3.create 0. 0. focal_length)) in
  { origin; horizontal; vertical; lower_left_corner }

let get_ray u v camera = let open Vec3 in
  Ray.create
    camera.origin
    (camera.lower_left_corner +| (camera.horizontal *| u) +| (camera.vertical *| v) -| camera.origin)

    (*
let create lookfrom lookat v_up fov aspect_ratio aperture focus_dist = let open Vec3 in
  let theta = radian_of_degree fov in
  let h = tan (theta /. 2.) in
  let viewport_height = 2.0 *. h in
  let viewport_width = aspect_ratio *. viewport_height in
  let w = Vec3.normalise (Vec3.subtract lookfrom lookat) in
  let u = Vec3.normalise (Vec3.cross v_up w) in
  let v = Vec3.cross w u in
  let origin = lookfrom in
  let horizontal = u *| (focus_dist *. viewport_width) in
  let vertical = v *| (focus_dist *. viewport_height) in
  let lower_left_corner = origin -| (horizontal /| 2.0) -| (vertical /| 2.0) -| (w *| focus_dist) in
  let lens_radius = aperture /. 2.0 in
  { origin; horizontal; vertical; lower_left_corner; u; v; w; lens_radius}

let get_ray s t camera = let open Vec3 in
  let rd = random_in_unit_disk () *| camera.lens_radius  in
  let offset = (camera.u *| rd.x ) +| (camera.v *| rd.y) in
  Ray.create
    (camera.origin +| offset)
    (camera.lower_left_corner +| (camera.horizontal *| s) +| (camera.vertical *| t) -| camera.origin -| offset)
    *)
