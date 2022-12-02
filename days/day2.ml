open Core

let () =
  let parse_play = function
    | 'A' -> 0 | 'B' -> 1 | 'C' -> 2
    | 'X' -> 0 | 'Y' -> 1 | 'Z' -> 2
    | _ -> failwith "parse failed"
  in
  let parse_line line = match String.to_list line with
    | [a; _; b] -> (parse_play a, parse_play b)
    | _ -> failwith "parse failed"
  in
  let lines = List.rev_map (In_channel.input_lines In_channel.stdin) ~f:parse_line in
  let score (a, b) = (b - a + 1) % 3 * 3 + b + 1 in
  printf "Part 1: %d\n" @@ List.fold lines ~init:0 ~f:(fun b a -> b + score a);
  let massage (a, b) = (a, (a + b - 1) % 3) in
  printf "Part 2: %d\n" @@ List.fold lines ~init:0 ~f:(fun b a -> b + score (massage a))
