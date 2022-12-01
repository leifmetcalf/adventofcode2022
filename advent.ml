open Core

let () =
  let f (elves, acc) line =
    if String.is_empty line then
      (acc :: elves, 0)
    else
      (elves, acc + int_of_string line)
  in
  let (elves, acc) = In_channel.fold_lines In_channel.stdin ~init:([], 0) ~f in
  let elves = List.sort ~compare:Int.descending (acc :: elves) in
  printf "%d\n" @@ List.fold (List.take elves 3) ~init:0 ~f:(+)
