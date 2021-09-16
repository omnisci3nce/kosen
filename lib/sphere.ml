type t = {
  center: Vec3.t;
  radius: float
}

let create v r = {
  center = v;
  radius = r
}

type hit_record = {
  point:  Vec3.t;
  normal: Vec3.t;
  t:      float
}
  
let hit s (r: Ray.t) t_min t_max =
  let open Vec3 in
  let oc = r.origin -| s.center in
  let a = Vec3.length_squared r.direction in
  let half_b = dot oc r.direction in
  let c = (Vec3.length_squared oc) -. (s.radius *. s.radius) in
  let discriminant = (half_b *. half_b) -. (a *. c) in
  if discriminant < 0. then
    None
  else
    let sqrtd = sqrt discriminant in
    let root1  = -.half_b -. sqrtd /. a
    and root2  = -.half_b +. sqrtd /. a in
    if (root1 < t_min || t_max < root1) then
      if (root2 < t_min || t_max < root2) then
        None
      else
        Some root2
        (* Some {
          t = root2;
          point = Ray.at root2 r;
          normal = ((Ray.at root2 r) -| s.center) /| s.radius
        } *)
    else
      Some root1
      (* Some {
        t = root1;
        point = Ray.at root1 r;
        normal = ((Ray.at root1 r) -| s.center) /| s.radius
      } *)