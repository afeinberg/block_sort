(** Implementation of a sorting algorithm for lists in O(n ln(k)), where
    - n is the list size
    - k is the number of changes of direction *)

(** main sorting function. This is a stable sort, often much faster
    than List.sort sometimes a bit slower. *)
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
