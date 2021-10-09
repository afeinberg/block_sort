(** Implementation of a sorting algorithm for lists in O(n ln(k)), where
    - n is the list size
    - k is the number of changes of direction *)

(** The first phase of the algorithm is to split the list in blocks
    which are either in order of in reverse order.

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

(** [split cmp x l l0] split the list [x::l as l0] with [cmp] as comparison.
    Beware, the blocks are returned with the first block being the last in
    the list.
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

  (** We are building a sorted block *)
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

(** The two next functions are used to convert blocks in lists *)

(** [rev_heads n l] gives the reversal of the n first elements of l.*)
let rev_heads n l =
  let rec gn acc n l =
    match n,l with
    | 0, _ -> acc
    | _, x::l -> gn (x::acc) (n-1) l
    | _ -> assert false
  in
  gn [] n l

(** [heads n l] gives the n first elements of l *)
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

(** Now a merge sort with a special treatment of the leafs, inspired by
    List.sort in ocaml stdlib. *)

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
    reference to the unused blocks *)
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
  | _ -> (** standard merge *)
     let n1 = n lsr 1 in
     let n2 = n - n1 in
     (** end of the list first as cur point to the end first *)
     let l2 = rev_sort cur cmp n2 in
     let l1 = rev_sort cur cmp n1 in
     merge_rev cmp [] l1 l2

and rev_sort cur cmp n =
  match n with
  | 1 -> (** 1 block, we transform it into a list *)
     begin
       match !cur with
       | Ord{size;list;next} ->
          cur := next;
          rev_heads size list;
       | Rev{size;list;next} ->
          cur := next;
          (** This case is a bit of a waste ... *)
          heads size list
       | Fin ->
          assert false
     end
  | _ -> (** standard merge *)
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
