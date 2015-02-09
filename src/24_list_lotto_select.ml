

(* rand_select from 23_list_extract_random_sub_list.ml *)
let rand_select list n =
  let rec extract acc idx = function
    | [] -> raise Not_found
    | x::tl -> if idx = 0 then (x, acc @ tl) else
                 extract (x::acc) (idx-1) tl in
  let extract_rand list =
    let len = List.length list in
    extract [] (Random.int len) list in
  let rec iter n acc list =
    if n = 0 then acc else
      let picked, rest = extract_rand list in
      iter (n-1) (picked::acc) rest in
  iter n [] list;;

let lotto_select n m = rand_select (range 1 m) n;;

let () =
  assert(List.length(lotto_select 3 10) = 3);
