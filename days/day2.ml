open Core

let () =
  let parse_line line = match String.to_list line with
    | [a; _; b] -> (Char.to_int a - 65, Char.to_int b - 88)
    | _ -> failwith "parse failed"
  in
  let lines = List.rev_map (In_channel.input_lines In_channel.stdin) ~f:parse_line in
  let score (a, b) = (b - a + 1) % 3 * 3 + b + 1 in
  printf "Part 1: %d\n" @@ List.fold lines ~init:0 ~f:(fun b a -> b + score a);
  let massage (a, b) = (a, (a + b - 1) % 3) in
  printf "Part 2: %d\n" @@ List.fold lines ~init:0 ~f:(fun b a -> b + score (massage a))
