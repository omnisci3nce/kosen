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
  match world_hit 0.0001 max_float r world with
  | None ->
    let unit_direction = Vec3.unit_vector r.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    Vec3.(((create 1. 1. 1.) *| (1.0 -. t)) +| ((create 0.5 0.7 1.0) *| t))
  | Some hitrec ->
      Vec3.( (hitrec.normal +| (Vec3.create 1. 1. 1.)) *| 0.5 )

let () = 
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;

  (* Iterate through every pixel in row, for each row *)
  for j = (image_height - 1) downto 0 do
    Printf.printf "Scanlines remaining: %d\n" j;
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
