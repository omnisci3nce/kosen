(* Cartesian product *)
let product l1 l2 =
  let rec aux acc la lb =
    match la, lb with 
    | [], _ -> acc
    | _::ta, [] -> aux acc ta l2
    | ha::_, hb::tb -> aux ((ha, hb)::acc) la tb
  in aux [] l1 l2;;

let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in 
  deopt [] l

