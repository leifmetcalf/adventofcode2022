open Core

let () =
  let f ((a, b, c) as burdened, acc) line =
    if String.is_empty line then
      if acc > a then
        ((acc, a, b), 0)
      else if acc > b then
        ((a, acc, b), 0)
      else if acc > c then
        ((a, b, acc), 0)
      else
        (burdened, 0)
    else
      (burdened, acc + int_of_string line)
  in
  let ((a, b, c), _) = f (In_channel.fold_lines In_channel.stdin ~init:((0, 0, 0), 0) ~f) "" in
  printf "Part 1: %d\nPart 2: %d\n" a (a + b + c)
