

let prepend c str =
  let s' = String.make (String.length str + 1) c in
  String.blit str 0 s' 1 (String.length str);
  s';;

let rec gray n =
  if n <= 1 then ["0"; "1"] else
    let g = gray(n-1) in
    List.map (prepend '0') g @ List.map (prepend '1') g;;

let () =
  assert( gray 3 =
            ["000"; "001"; "010"; "011"; "100"; "101"; "110"; "111"]);;
