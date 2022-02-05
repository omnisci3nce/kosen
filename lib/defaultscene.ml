open Sphere
open Utils

(* Setup *)
(* let aspect_ratio = 16.0 /. 9.0 *)
let aspect_ratio = 1.5
let image_width = 1200
let image_height = Int.of_float(Float.of_int(image_width) /. aspect_ratio)

let lookfrom = Vec3.create 13. 2. 3.
let lookat = Vec3.create 0. 0. 0.
let camera = Camera.create
  lookfrom
  lookat
  (Vec3.create 0. 1. 0.)
  20.0 aspect_ratio
  0.1
  10.0

let random_spheres = let open Vec3 in
  let sequence = product
    ( range (-11) 11 ) 
    ( range (-11) 11 ) in
  let results = List.map (fun (a, b) -> 
    let choose_mat = Random.float 1. in
    let center = Vec3.create
      (Int.to_float(a) +. 0.9 *. (Random.float 1.))
      0.2
      (Int.to_float(b) +. 0.9 *. (Random.float 1.)) in
    if length (center -| (Vec3.create 4. 0.2 0.)) > 0.9 then
      match choose_mat with
      | x when x < 0.8 ->
        let albedo =  elem_wise_product (random_vec()) (random_vec()) in
        let material = Material.Lambertian { albedo } in
        Some { center; radius = 0.2; material }
      | x when x < 0.95 -> 
        let albedo = Vec3.create
          (Base.Random.float_range 0.5 1.)
          (Base.Random.float_range 0.5 1.)
          (Base.Random.float_range 0.5 1.) in
        let fuzz = Base.Random.float_range 0. 0.5 in
        let material = Material.Metal { albedo; fuzz} in
        Some { center; radius = 0.2; material}
      | _ -> (* Glass *)
        let material = Material.Dielectric { refraction_index = 1.5 } in
        Some { center; radius = 0.2; material}
    else
      None
    ) sequence  in
  deoptionalize results

let ground_material = Material.Lambertian { albedo = Vec3.create 0.5 0.5 0.5 }
let ground = { center = Vec3.create 0. (-1000.) 0.; radius = 1000. ; material = ground_material}
let material1 = Material.Dielectric { refraction_index = 1.5 }
let material2 = Material.Lambertian { albedo = Vec3.create 0.4 0.2 0.1 }
let material3 = Material.Metal { albedo = Vec3.create 0.7 0.6 0.5; fuzz = 0.0}
let sphere1 = { center = Vec3.create 0. 1. 0.; radius = 1.0; material = material1 }
let sphere2 = { center = Vec3.create (-4.) 1. 0.; radius = 1.0; material = material2 }
let sphere3 = { center = Vec3.create 4. 1. 0.; radius = 1.0; material = material3 }
let world = [ground; sphere1; sphere2; sphere3] @ random_spheres


