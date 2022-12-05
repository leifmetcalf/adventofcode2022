open Core

let parse_input lines =
  let rec go acc = function
    | "" :: rest -> (List.rev acc, rest)
    | line :: rest -> go (line :: acc) rest
    | _ -> failwith "invalid input"
  in
  let (stacks, moves) = go [] lines in
  let f = function
    | (x :: rest) when Char.is_digit x -> Some (List.drop_while ~f:(fun c -> Char.(c = ' ')) @@ List.rev rest)
    | _ -> None
  in
  let stacks = List.filter_map ~f @@ Option.value_exn @@ List.transpose @@ List.rev_map ~f:String.to_list stacks in
  let f line = match String.split ~on:' ' line with
    | [_; n; _; a; _; b] -> (int_of_string n, int_of_string a - 1, int_of_string b - 1)
    | _ -> failwith "invalid move"
  in
  let moves = List.map ~f moves in
  (stacks, moves)

let () =
  let (stacks, moves) = parse_input @@ In_channel.input_lines In_channel.stdin in
  let solve rev =
    let stacks = Array.of_list stacks in
    let f (n, a, b) =
      let (top, bottom) = List.split_n stacks.(a) n in
      stacks.(a) <- bottom;
      stacks.(b) <- List.append (rev top) stacks.(b)
    in
    List.iter ~f moves;
    String.of_char_list @@ Array.to_list @@ Array.map ~f:List.hd_exn stacks
  in
  printf "Part 1: %s\nPart 2: %s\n" (solve List.rev) (solve Fun.id)
