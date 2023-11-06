open! Core

type tree = Leaf | Node of tree * int * tree [@@deriving sexp_of, quickcheck]

let rec to_list t =
  match t with Leaf -> [] | Node (l, x, r) -> to_list l @ [ x ] @ to_list r

let rec is_bst t =
  match t with
  | Leaf -> true
  | Node (l, x, r) ->
      List.for_all (to_list l) ~f:(fun y -> y < x)
      && List.for_all (to_list r) ~f:(fun y -> y > x)
      && is_bst l && is_bst r

let rec insert x t =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l, y, r) ->
      if x < y then Node (insert x l, y, r)
      else if x > y then Node (l, y, insert x r)
      else Node (l, y, r)

let rec size t =
  match t with Leaf -> 0 | Node (l, _, r) -> 1 + size l + size r
