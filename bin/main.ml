open Kosen
open Kosen.Defaultscene
open Kosen.Sphere

let material_ground = Material.Lambertian { albedo = Vec3.create 0.8 0.8 0.0 }
let material_center = Material.Lambertian { albedo = Vec3.create 0.1 0.2 0.5 } 
let material_left = Material.Dielectric { refraction_index = 1.5 }
let material_right = Material.Metal { albedo = Vec3.create 0.8 0.6 0.2; fuzz = 0.0}
let sphere1 = { center = Vec3.create 0. (-100.5) (-1.); radius = 100.; material = material_ground }
let sphere2 = { center = Vec3.create 0. 0. (-1.); radius = 0.5; material = material_center }
let sphere3 = { center = Vec3.create (-1.) 0. (-1.); radius = 0.5; material = material_left }
let sphere4 = { center = Vec3.create 1. 0. (-1.); radius = 0.5; material = material_right }
let sphere5 = { center = Vec3.create (-1.) 0. (-1.); radius = -0.4; material = material_left}
let world = [sphere1; sphere2; sphere3; sphere4; sphere5]

let rec world_hit t_min t_max (r: Ray.t) = function
  | [] -> None
  | h::t -> begin
    match Sphere.hit r h with
    | None -> world_hit t_min t_max r t
    | Some hitrec -> begin
      match world_hit t_min t_max r t with
        | Some (next: Material.hit_record) -> if hitrec.t < next.t then Some hitrec else Some next
        | None -> Some hitrec
    end
  end

let ray_color (r: Ray.t) world =
  let max_recursion_depth = 50 in

  let rec _ray_color r world depth =
    if depth >= max_recursion_depth then Vec3.zero
    else match world_hit 0.0001 max_float r world with
    | None ->
      let unit_direction = Vec3.unit_vector r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      Vec3.(((create 1. 1. 1.) *| (1.0 -. t)) +| ((create 0.5 0.7 1.0) *| t))

    | Some hitrec -> begin
        match Material.scatter r hitrec hitrec.material with
          | Some { scattered; attenuation } ->
              Vec3.elem_wise_product attenuation (_ray_color scattered world (depth+1))
          | None -> Vec3.zero
        end in
  _ray_color r world 0

let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let () =
  let cam = Camera.create in
  let samples_per_pixel = 100 in
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;

  (* Iterate through every pixel in row, for each row *)
  for j = (image_height - 1) downto 0 do
    Printf.printf "Scanlines remaining: %d\n" j; flush stdout;
    for i = 0 to (image_width - 1) do

      let color_total =
        List.fold_left (fun (acc: Vec3.t) _ -> 
          let u = (Float.of_int(i) +. Random.float(1.)) /. Float.of_int(image_width)
          and v = (Float.of_int(j) +. Random.float(1.)) /. Float.of_int(image_height) in
          let r = cam |> Camera.get_ray u v in
          Vec3.add (ray_color (r: Ray.t) world) acc
        ) (Vec3.create 0. 0. 0.) (range 0 (samples_per_pixel - 1)) in
   
      Color.write_color oc color_total samples_per_pixel
    done
  done;

  close_out oc;
