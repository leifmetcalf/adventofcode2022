open Core

let parse_line line =
  match List.map ~f:int_of_string @@ String.split_on_chars ~on:['-'; ','] line with
    | [a; b; c; d] -> (a, b, c, d)
    | _ -> failwith "parse error"

let () =
  let pairs = List.map ~f:parse_line @@ In_channel.input_lines In_channel.stdin in
  let part_1 = List.count pairs ~f:(fun (a, b, c, d) -> a <= c && d <= b || c <= a && b <= d) in
  let part_2 = List.count pairs ~f:(fun (a, b, c, d) -> Int.max a c <= Int.min b d) in
  printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
