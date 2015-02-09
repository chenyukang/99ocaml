
let compress list =
  let rec comp cur list =
    match list with
    | [] -> []
    | x::tl -> (match cur with
                | None -> x::(comp (Some x) tl)
                | Some(v) -> (if x = v then
                                comp cur tl else
                                x::(comp (Some x) tl))) in
  comp None list;;


let () =
  assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
           ["a"; "b"; "c"; "a"; "d"; "e"]);;

let () =
  assert(compress [] = []);;
