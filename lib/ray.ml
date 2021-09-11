type t = {
  origin: Vec3.t;
  direction: Vec3.t
}

let create origin direction = { origin; direction }

let at t ray = Vec3.( ray.origin +| (ray.direction *| t) )