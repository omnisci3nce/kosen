(* Vector 3 *)
type t = {
  x: float; y: float; z: float
}

let create x y z = { x; y; z }
let zero = { x=0.; y=0.; z=0. }

let add u v = {
  x = u.x +. v.x;
  y = u.y +. v.y;
  z = u.z +. v.z
}

let subtract u v = {
  x = u.x -. v.x;
  y = u.y -. v.y;
  z = u.z -. v.z
}

let multiply u s = {
  x = u.x *. s;
  y = u.y *. s;
  z = u.z *. s;
}

let divide u s = multiply u (1. /. s)

(* Operators *)
let (+|) = add
let (-|) = subtract
let ( *| ) = multiply
let (/|) = divide

let length_squared u = (u.x *. u.x) +. (u.y *. u.y) +. (u.z *. u.z)
let length u = sqrt (length_squared u)

let negate u = {
  x = -.u.x;
  y = -.u.y;
  z = -.u.z
}

let unit_vector v = v /| (length v)

let dot u v =
  (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)

let cross u v = {
  x = (u.y *. v.z) -. (u.z *. v.y);
  y = (u.z *. v.x) -. (u.x *. v.z);
  z = (u.x *. v.y) -. (u.y *. v.x)
}