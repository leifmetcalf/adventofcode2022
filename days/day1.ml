open Core

let () =
  let f (elves, acc) line =
    if String.is_empty line then
      (acc :: elves, 0)
    else
      (elves, acc + int_of_string line)
  in
  let (elves, acc) = In_channel.fold_lines In_channel.stdin ~init:([], 0) ~f in
  match List.sort ~compare:Int.descending (acc :: elves) with
    | a :: b :: c :: _ -> printf "Part 1: %d\nPart 2: %d\n" a (a + b + c)
    | _ -> print_string "Invalid input"
