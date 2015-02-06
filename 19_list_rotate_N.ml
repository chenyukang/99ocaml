
let rotate list n =
  let len = List.length list in
  let pos = (n mod len + len) mod len in
  let rec iter cur k list =
    match list with
    | [] -> cur
    | x::tl -> if k < pos
               then (iter (x::cur) (k + 1) tl)
               else (List.append tl (List.rev (x::cur))) in
  iter [] 1 list;;

let () =
  assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 =
           ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);;

let () =
  assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) =
           ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);;
