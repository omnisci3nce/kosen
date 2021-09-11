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