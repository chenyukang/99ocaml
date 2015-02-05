
let pack list =
  let rec aux cur acc = function
    | [] -> []
    | [x] -> (x::cur)::acc
    | a::(b:: _ as t) ->
       if a = b then aux (a::cur) acc t else
         aux [] ((a::cur)::acc) t in
  List.rev (aux [] [] list);;

let () =
  assert(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
           [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
            ["e"; "e"; "e"; "e"]]);;
