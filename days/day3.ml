open Core

let single_intersection lists =
  let intersection = List.reduce_exn ~f:Set.inter @@
    List.rev_map ~f:(Set.of_list (module Int)) lists
  in
  match Set.to_list intersection with
    | [x] -> x
    | _ -> failwith "no single intersection"

let parse_rucksack line =
  let parse_letter letter =
    Char.to_int letter + if Char.is_lowercase letter then
      1 - Char.to_int 'a'
    else
      27 - Char.to_int 'A'
  in
  List.map ~f:parse_letter @@ String.to_list line

let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let rucksacks = List.map ~f:parse_rucksack lines in
  let f sum rucksack =
    sum + single_intersection
      (List.chunks_of ~length:(List.length rucksack / 2) rucksack)
  in
  let part_1 = List.fold ~f ~init:0 rucksacks in
  let f sum group = sum + single_intersection group in
  let part_2 = List.fold ~f ~init:0 @@ List.chunks_of ~length:3 rucksacks in
  printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
