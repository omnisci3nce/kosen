open Types

type t =  Lambertian of { albedo: Vec3.t }
        | Metal of { albedo: Vec3.t; fuzz: float }
        | Dielectric of { refraction_index: float }

type scatter_result = { scattered: Ray.t; attenuation: Vec3.t }

type hit_record = {
  p: point;
  normal: direction;
  t: float;
  front_face: bool;
  material: t
}

let rec float_pow n e =
  if e < 1 then n else n *. float_pow n (e - 1)

let schlick cosine ref_idx =
  let r0 = (1.0 -. ref_idx) /. (1.0 +. ref_idx) in
  let r1 = r0 *. r0 in
  r1 +. (1.0 -. r1) *. float_pow (1.0 -. cosine) 5

let scatter (r_in: Ray.t) hitrec = let open Vec3 in function
  | Lambertian { albedo } ->
      let scatter_direction = hitrec.normal +| Vec3.random_unit_vector () in
      let scattered = Ray.create hitrec.p scatter_direction in
      let attenuation = albedo in 
      Some { scattered; attenuation }
  | Metal {albedo; fuzz}     ->
    let reflected = reflect (normalise r_in.direction) hitrec.normal in
    let scattered = Ray.create hitrec.p (reflected +| (random_in_unit_sphere () *| fuzz)) in
    let attenuation = albedo in
    if (dot scattered.direction hitrec.normal) > 0. then
      Some { scattered; attenuation }
    else None
  | Dielectric { refraction_index } ->
      let attenuation = Vec3.create 1. 1. 1.
    and refraction_ratio = if hitrec.front_face then 1.0 /. refraction_index else refraction_index
    and unit_direction = normalise r_in.direction in
    let cos_theta = min (dot (negate unit_direction) hitrec.normal) 1.0 in
    let sin_theta = sqrt (1.0 -. cos_theta *. cos_theta) in
    let direction = if refraction_ratio *. sin_theta > 1.0 || schlick cos_theta refraction_ratio > Random.float(1.0)
    then
      reflect unit_direction hitrec.normal
    else
      refract unit_direction hitrec.normal refraction_ratio
    in
    let scattered = Ray.create hitrec.p direction in

    Some { scattered; attenuation }
