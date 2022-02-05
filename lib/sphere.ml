type t = {
  center: Vec3.t;
  radius: float;
  material: Material.t
}

let create v r m = {
  center = v;
  radius = r;
  material = m
}
  
let hit (r: Ray.t) sphere = let open Vec3 in let open Material in
  let t_max = max_float
  and t_min = 0.0001 in
  let oc = r.origin -| sphere.center in
  let a = length_squared r.direction
  and half_b = dot oc r.direction
  and c = (length_squared oc) -. (sphere.radius *. sphere.radius) in
  let discriminant =  half_b *. half_b -. a *. c in

  if discriminant > 0. then
    let root = sqrt discriminant in
    let t1 = ((-.half_b) -. root) /. a in
    if t1 < t_max && t1 > t_min then
      let t = t1 in
      let p = Ray.at t1 r in
      let outward_normal = (p -| sphere.center) /| sphere.radius in
      let front_face = (dot r.direction outward_normal) < 0. in
      let normal = if front_face then outward_normal else negate outward_normal in
      Some {p;normal;t;front_face; material = sphere.material}
    else
      let t2 = ((-.half_b) +. root) /. a in
      if t2 < t_max && t2 > t_min then
        let t = t2 in
        let p = Ray.at t2 r in
        let outward_normal = (p -| sphere.center) /| sphere.radius in
        let front_face = (dot r.direction outward_normal) < 0. in
        let normal = if front_face then outward_normal else negate outward_normal in
        Some {p;normal;t;front_face; material = sphere.material}
      else
        None
      
  else None
