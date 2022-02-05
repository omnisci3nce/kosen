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


