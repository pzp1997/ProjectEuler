(*
 * Author: Palmer Paul
 * Problem no.: 1
 * Language: OCaml
 * Date: 2/8/2017
 *)


let sum (l: int list) : int = List.fold_left ( + ) 0 l;;

let range (start: int) (stop: int) (step: int) : int list =
  let so_mod_se = stop mod step in
  let stop = if so_mod_se = 0 then stop else stop + step - so_mod_se in
  let rec range_inner (n: int) (acc: int list) : int list =
    if n <= start then acc
    else let m = n - step in range_inner m (m :: acc)
  in range_inner stop []
;;

let ans : int = sum (List.filter
  (fun x -> x mod 3 = 0 || x mod 5 = 0) (range 0 1000 1)) in
print_endline (string_of_int ans);;


(* let range (start: int) (stop: int) : int list =
  let rec range_inner (n: int) (acc: int list) : int list =
    if n <= start then acc
    else let m = n - 1 in range_inner m (m :: acc)
  in range_inner stop []
;;

let range (stop: int) : int list =
  let rec range_inner (n: int) (acc: int list) : int list =
    if n <= 0 then acc
    else let m = n - 1 in range_inner m (m :: acc)
  in range_inner stop []
;; *)
