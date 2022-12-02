open Core

let () =
  let f (acc1, acc2) line =
    let a = Char.to_int line.[0] - 65 in
    let b = Char.to_int line.[2] - 88 in
    (acc1 + (b - a + 1) % 3 * 3 + b + 1,
     acc2 + b * 3 + (a + b - 1) % 3 + 1)
  in
  let (part1, part2) = In_channel.fold_lines In_channel.stdin ~init:(0, 0) ~f in
  printf "Part 1: %d\nPart 2: %d\n" part1 part2
