open Core

let parse_line line =
  match List.map ~f:int_of_string @@ String.split_on_chars ~on:['-'; ','] line with
    | [a; b; c; d] -> ((a, b), (c, d))
    | _ -> failwith "parse error"

let () =
  let pairs = List.map ~f:parse_line @@ In_channel.input_lines In_channel.stdin in
  let f acc ((a, b), (c, d)) = if a <= c && d <= b || c <= a && b <= d then acc + 1 else acc in
  let part_1 = List.fold ~f ~init:0 pairs in
  let f acc ((a, b), (c, d)) = if Int.max a c <= Int.min b d then acc + 1 else acc in
  let part_2 = List.fold ~f ~init:0 pairs in
  printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
