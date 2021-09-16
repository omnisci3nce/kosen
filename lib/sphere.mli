type t
type hit_record

val create : Vec3.t -> float -> t

(* val hit : t -> Ray.t -> float -> float -> hit_record option *)
val hit : t -> Ray.t -> float -> float -> float option