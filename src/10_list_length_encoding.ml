
(* a complicated version :( *)
let encode list =
  let rec encode_iter cur num list =
    match list with
    | [] -> []
    | [x] -> (match cur with
              | None -> [(1, x)]
              | Some(v) -> (if x = v then
                              [(num+1, x)] else
                              [(num, v); (1, x)]))
    | x::tl -> (match cur with
                | None -> (encode_iter (Some x) 1 tl)
                | Some(v) -> (if x = v then
                                (encode_iter cur (num + 1) tl) else
                                (num, v)::(encode_iter (Some x) 1 tl))) in
  encode_iter None 0 list;;


let () =
  assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
           [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;


(* a simpler version *)
let encode list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((count+1,a) :: acc) t in
  List.rev (aux 0 [] list);;

let () =
  assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
           [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;
