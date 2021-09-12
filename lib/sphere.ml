type t = {
  center: Vec3.t;
  radius: float
}

let create v r = {
  center = v;
  radius = r
}
  
let hit s (r: Ray.t) =
  let open Vec3 in
  let oc = r.origin -| s.center in
  let a = Vec3.length_squared r.direction in
  let half_b = dot oc r.direction in
  let c = (Vec3.length_squared oc) -. (s.radius *. s.radius) in
  let discriminant = (half_b *. half_b) -. (a *. c) in
  if discriminant < 0. then
    None
  else
    Some (((-.half_b) -. sqrt(discriminant)) /. a)