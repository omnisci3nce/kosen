open Types

type t =  Lambertian of { albedo: Vec3.t }
        | Metal of { albedo: Vec3.t; fuzz: float }
     (*   | Dialectric of { refraction_index: float } *)

type scatter_result = { scattered: Ray.t; attenuation: Vec3.t }

type hit_record = {
  p: point;
  normal: direction;
  t: float;
  front_face: bool;
  material: t
}

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
