type t = { center : Vec3.t; radius : float; }

val create : Vec3.t -> float -> t

val hit_sphere : t -> Ray.t -> float