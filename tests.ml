(* TESTS *)
open Block_sort

let quiet = Array.length Sys.argv = 2 && Sys.argv.(1) = "--quiet"
(* Printing *)
let print_list f ch =
  Printf.fprintf ch "[%a]" (fun ch l ->
                   List.iteri (fun i x ->
                       Printf.fprintf ch "%s%a" (if i > 0 then ";" else "") f x) l)

let print_il = print_list (fun ch -> Printf.fprintf ch "%d")
let print_ill = print_list (fun ch -> Printf.fprintf ch "%a" print_il)

(* Timing *)
let chrono f x =
  Gc.compact ();
  let t1 = Sys.time () in
  let r = f x in
  let t2 = Sys.time () in
  (t2 -. t1, r)

let chronos msg f g x =
  Gc.full_major (); (** GC to avoid paying for the allocation of the other *)
  let tf, lf = chrono f x in
  Gc.full_major ();
  let tg, lg = chrono g x in
  assert(lf = lg);
  let g = 100. *. (tf -. tg) /. tf in
  let f = tf /. tg in
  if quiet then
    Printf.printf "%16s: passed\n%!" msg
  else
    Printf.printf "%16s: tf = %.2f, tg = %.2f, factor = %.2fx, gain = %.2f%%\n%!" msg tf tg f g

(* TEST CORRECTNESS *)

let alea n p =
  let rec fn acc n =
    if n <= 0 then acc else fn (Random.int p :: acc) (n - 1)
  in fn [] n

let _ =
  for i = 0 to 1000 do
    let l = alea i 10_000 in
    assert (sort compare l = List.sort compare l)
  done

let _ = Printf.printf "Correctness test passed\n%!"

(* TEST STABILITY *)

let alea2 n p =
  let rec fn acc n =
    if n <= 0 then acc else fn ((Random.int p, Random.int p) :: acc) (n - 1)
  in fn [] n

let _ =
  for i = 0 to 1000 do
    let l = alea2 i 100 in
    let cmp (x,_) (y,_) = compare x y in
    assert (sort cmp l = List.sort cmp l)
  done

let _ = Printf.printf "Stability test passed\n%!"

(** Random lists *)

let _ = Printf.printf "Random lists:\n%!"

let l0 = alea 2_000_000 100_000_000
let _ = chronos "random" (List.stable_sort compare) (sort compare) l0

let l0 = alea 2_000_000 5
let _ = chronos "random small" (List.stable_sort compare) (sort compare) l0

(** TWO WORST CASES *)
let _ = Printf.printf "Worst cases:\n%!"

let worst1 n =
  let rec fn acc n =
    if n <= 0 then acc else
    fn (n-3::n::acc) (n-2)
  in fn [] n

let worst2 n =
  let rec fn acc n =
    if n <= 0 then acc else
    fn (n-4::n+1::n::acc) (n-3)
  in fn [] n

let l0 = worst1 2_000_000
let _ = chronos "worst1" (List.stable_sort compare) (sort compare) l0

let l0 = worst2 2_000_000
let _ = chronos "worst2" (List.stable_sort compare) (sort compare) l0

(** SORTED LISTS *)
let _ = Printf.printf "Sorted (partially) lists:\n%!"

let sorted n a b =
  let rec fn acc n =
    if n <= 0 then acc else fn (a*n+b :: acc) (n - 1)
  in fn [] n

(** Almost sorted *)
let l0 = sorted 2_000_000 1 0
let _ = chronos "sorted" (List.stable_sort compare) (sort compare) l0

let l0 = sorted 2_000_000 (-1) 0
let _ = chronos "reversed" (List.stable_sort compare) (sort compare) l0

let l0 = sorted 1_000_000 1 0 @ sorted 1_000_000 (-1) 0
let _ = chronos "sorted@rev" (List.stable_sort compare) (sort compare) l0

let l0 = sorted 1_000_000 (-1) 0 @ sorted 1_000_000 1 0
let _ = chronos "rev@sorted" (List.stable_sort compare) (sort compare) l0

(** Shuffled lists (permute k times 2 elements in a sorted list) *)
let _ = Printf.printf
          "Shuffled lists (permute k times 2 elements in a sorted list):\n%!"

let shuffle n k =
  let array = Array.init n (fun i -> i) in
  for i = 1 to k; do
    let a = Random.int n and b = Random.int n in
    let tmp = array.(a) in
    array.(a) <- array.(b);
    array.(b) <- tmp
  done;
  Array.to_list array

let l0 = shuffle 2_000_000 10
let _ = chronos "shuffle 10" (List.stable_sort compare) (sort compare) l0

let l0 = shuffle 2_000_000 100
let _ = chronos "shuffle 100" (List.stable_sort compare) (sort compare) l0

let l0 = shuffle 2_000_000 1_000
let _ = chronos "shuffle 1000" (List.stable_sort compare) (sort compare) l0

let l0 = shuffle 2_000_000 10_000
let _ = chronos "shuffle 10000" (List.stable_sort compare) (sort compare) l0

let l0 = []
