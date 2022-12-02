open Core

let primes n =
  let arr = Array.create ~len:(n + 1) true in
  let res = ref [] in
  for i = 2 to n do
    if arr.(i) then
      res := i :: !res;
      let j = ref (i * i) in
      while !j <= n do
        arr.(!j) <- false;
        j := !j + i
      done
  done;
  List.rev !res
