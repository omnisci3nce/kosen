type t = Sphere.t list

let empty : t = []

let add (x: Sphere.t) (s: t) = x :: s

let hit (r: Ray.t) (s: t) : Material.hit_record =
  s
  (* |> List.map (Sphere.hit r) *)
  (* |> List. *)