type t

val create : Vec3.t -> float -> t

val hit : Ray.t -> t -> Material.hit_record option