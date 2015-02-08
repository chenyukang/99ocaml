
let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n / d) with
        | (h,n) :: t when h = d -> (h,n+1) :: t
        | l -> (d,1) :: l
      else aux (d+1) n
  in
  aux 2 n;;

let rec pow n p =
  if p < 1 then 1 else
    n * pow n (p-1);;


let phi_imporved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m)::tl -> aux ((p-1) * pow p (m-1) * acc) tl in
  aux 1 (factors n);;


let () =
  assert(phi_imporved 10 = 4);
  assert(phi_imporved 13 = 12);;
