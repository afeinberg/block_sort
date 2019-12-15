(** Implementation of a sorting algorithm for lists
    in O(n ln(k)), where
    - n is the list size
    - k is the number of changes of direction *)

(** The first phase of the algorithm is
    to split the list in block which are either
    in order of in reverse order.

    The block have a size and a pointer to the original list.
 *)

type 'a blocks =
  | Ord of { size : int
           ; list : 'a list (* beginning of the block *)
           ; next : 'a blocks
           }
  | Rev of { size : int
           ; list : 'a list (* beginning of the block *)
           ; next : 'a blocks
           }
  | Fin

(** [split cmp x l l0] the list [x::l as l0] with [cmp] as comparison.
    Beware, the block are returned with first the first block being
    the last in the list.
 *)
let split cmp x l l0 =
  (** Start of a new block *)
  let rec split_ini len next x l l0 =
    match l with
    | [] -> (len, Ord { size=1; list=l0; next })
    | y :: l' ->
       if cmp x y <= 0 then
         split_sam len next 2 y l' l0
       else
         split_rev len next 2 y l' l0

  (** We are building a reverse block *)
  and split_rev len next size x l l0 =
    match l with
    | [] -> (len, Rev { size; list=l0; next })
    | y :: l' ->
       (** Note: < 0 here is a bit faster but not stable *)
       if cmp x y <= 0 then
         let next = Rev { size; list=l0; next } in
         split_ini (len+1) next y l' l
       else
         split_rev len next (size+1) y l' l0

  and split_sam len next size x l l0 =
    match l with
    | [] -> (len, Ord { size; list=l0; next })
    | y :: l' ->
       if cmp x y <= 0 then
         split_sam len next (size+1) y l' l0
       else
         let next = Ord { size; list=l0; next } in
         split_ini (len+1) next y l' l

  in
  split_ini 1 Fin x l l0

(* The two next functions are used to convert block cells in list *)

(** [rev_heads n l] gives the reversal of the n firsts elements of l.*)
let rev_heads n l =
  let rec gn acc n l =
    match n,l with
    | 0, _ -> acc
    | _, x::l -> gn (x::acc) (n-1) l
    | _ -> assert false
  in
  gn [] n l

(** [heads n l] gives the n firsts elements of l *)
let rec heads n l =
  let rec fn n l =
    match n,l with
    | 0, _ -> []
    | _, x::l -> x::fn (n-1) l
    | _ -> assert false
  in
  if n >= 100_000 then (** use a tail rec version if n is large *)
    List.rev (rev_heads n l)
  else
    fn n l

(** Now a merge sort with a spacial treatment of the leafs *)

(** reversal of the merge of l1 l2 which are sorted,
    the result is reverse sorted*)
let rec rev_merge cmp acc l1 l2 =
  match l1, l2 with
  | ([], l) | (l, []) -> List.rev_append l acc
  | (x::l1'), (y::l2') ->
     if cmp x y <= 0 then rev_merge cmp (x::acc) l1' l2
     else rev_merge cmp (y::acc) l1 l2'

(** merge of l1 l2 which are reverse sorted, the result is sorted *)
let rec merge_rev cmp acc l1 l2 =
  match l1, l2 with
  | ([], l) | (l, []) -> List.rev_append l acc
  | (x::l1'), (y::l2') ->
     if cmp x y > 0 then merge_rev cmp (x::acc) l1' l2
     else merge_rev cmp (y::acc) l1 l2'

(** the two mutually recursive sort functions, one returning a sorted
    list the second a reverse sorted list. The first argument is a
    reference to the blocks *)
let rec sort cur cmp n =
  match n with
  | 1 -> (* 1 block, we transform it into a list *)
     begin
       match !cur with
       | Ord{size;list;next} ->
          cur := next;
          (* This case is a bit of a waste ... *)
          heads size list;
       | Rev{size;list;next} ->
          cur := next;
          rev_heads size list
       | Fin ->
          assert false
     end
  | _ -> (* standard merge *)
     let n1 = n lsr 1 in
     let n2 = n - n1 in
     (** end of the list first as cur point to the end first *)
     let l2 = rev_sort cur cmp n2 in
     let l1 = rev_sort cur cmp n1 in
     merge_rev cmp [] l1 l2

and rev_sort cur cmp n =
  match n with
  | 1 -> (* 1 block, we transform it into a list *)
     begin
       match !cur with
       | Ord{size;list;next} ->
          cur := next;
          rev_heads size list;
       | Rev{size;list;next} ->
          cur := next;
          (* This case is a bit of a waste ... *)
          heads size list
       | Fin ->
          assert false
     end
  | _ ->
     let n1 = n lsr 1 in
     let n2 = n - n1 in
     (** end of the list first as cur point to the end first *)
     let l2 = sort cur cmp n2 in
     let l1 = sort cur cmp n1 in
     rev_merge cmp [] l1 l2

(** Final sorting algorithm *)
let sort : type a. (a -> a -> int) -> a list -> a list = fun cmp l0 ->
  match l0 with [] | [_] -> l0 | x::l ->
  let (len, blocks) = split cmp x l l0 in
  match blocks with
  | Ord{list;next=Fin} -> list
  | Rev{list;next=Fin} -> List.rev list
  | _ -> sort (ref blocks) cmp len

(* TESTS *)

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
  let tf, lf = chrono f x in
  let tg, lg = chrono g x in
  assert(lf = lg);
  let g = 100. *. (tf -. tg) /. tf in
  let f = tf /. tg in
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
