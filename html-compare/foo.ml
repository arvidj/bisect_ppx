let f = function
  | 0 -> "0" (* seen by x only *)
  | 1 -> "1" (* seen by y only *)
  | 2 -> "2" (* seen by x and y *)
  | 3 -> "3" (* seen neither by x or y *)
  | n -> if n = 4 then "4" else if n = 5 then "5" else "n"
[@@ocamlformat "disable"]
