open Core

let () =
  let f (acc_1, acc_2) line =
    let a = Char.to_int line.[0] - 65 in
    let b = Char.to_int line.[2] - 88 in
    (acc_1 + (b - a + 1) % 3 * 3 + b + 1,
     acc_2 + b * 3 + (a + b - 1) % 3 + 1)
  in
  let (part_1, part_2) = In_channel.fold_lines In_channel.stdin ~init:(0, 0) ~f in
  printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
