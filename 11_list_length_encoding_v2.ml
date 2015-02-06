
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l =
  let create cnt elem =
    if cnt == 1 then One elem else
      Many(cnt, elem) in
  let rec iter cnt acc = function
    | [] -> []
    | [x] -> (create (cnt + 1) x) :: acc
    | hd::(snd :: _ as tl) ->
       if hd = snd then
         iter (cnt + 1) acc tl
       else iter 0 ((create (cnt + 1) hd)::acc) tl in
  List.rev (iter 0 [] l);;

let () =
  assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
           [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;
