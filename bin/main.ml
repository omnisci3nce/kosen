open Kosen
open Kosen.Defaultscene


let ground = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.
let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
let world = [ground; sphere1]

let rec world_hit t_min t_max (r: Ray.t) = function
  | [] -> None
  (* | h::[] -> hit_sphere h r t_min t_max *)
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

    | Some hitrec ->
        let target = Vec3.(hitrec.p +| hitrec.normal +| (Vec3.random_in_unit_sphere ())) in
        let new_ray = Ray.create hitrec.p (Vec3.subtract target hitrec.p) in
        Vec3.( (_ray_color new_ray world (depth+1)) *| 0.5 ) in
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
