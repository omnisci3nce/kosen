type t = {
  center: Vec3.t;
  radius: float
}

let create v r = {
  center = v;
  radius = r
}

let hit_sphere s (r: Ray.t) =
  let open Vec3 in
  let oc = r.origin -| s.center in
  let a = dot r.direction r.direction in
  let b = 2.0 *. (dot oc r.direction) in
  let c = (dot oc oc) -. (s.radius *. s.radius) in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  if discriminant < 0. then
    (-1.)
  else
    ((-.b) -. sqrt(discriminant)) /. (2. *. a)