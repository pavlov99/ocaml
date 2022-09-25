module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val size : 'a t -> int
end

module UnbalancedSet : Set = struct
  type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
  type 'a t = 'a tree

  let empty = Empty
  let rec size = function Empty -> 0 | Node (l, _, r) -> 1 + size l + size r

  let rec mem el = function
    | Empty -> false
    | Node (left, value, right) ->
        if el < value then mem el left
        else if el > value then mem el right
        else true

  let rec insert el = function
    | Empty -> Node (Empty, el, Empty)
    | Node (l, v, r) ->
        if el < v then Node (insert el l, v, r)
        else if el > v then Node (l, v, insert el r)
        else Node (l, v, r)
end

module ListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let size = List.length

  let rec mem el = function
    | [] -> false
    | h :: t -> if el = h then true else mem el t

  let insert el t = if mem el t then t else el :: t
end

let%test _ = UnbalancedSet.(mem 0 empty) = false
let%test _ = UnbalancedSet.(mem 0 (empty |> insert 0)) = true
let%test _ = UnbalancedSet.(mem 1 (empty |> insert 0 |> insert 1)) = true
let%test _ = UnbalancedSet.(empty |> insert 0 |> insert 0 |> size) = 1

module type Heap = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val find_min : 'a t -> 'a
  val delete_min : 'a t -> 'a t
end

module LeftistHeap = struct
  type 'a heap = Empty | Node of int * 'a * 'a heap * 'a heap

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let rank = function Empty -> 0 | Node (r, _, _, _) -> r

  let make_heap el h1 h2 =
    if rank h1 >= rank h2 then Node (rank h2 + 1, el, h1, h2)
    else Node (rank h1 + 1, el, h2, h1)

  let rec merge h1 h2 =
    match (h1, h2) with
    | Empty, h2 -> h2
    | h1, Empty -> h1
    | Node (_, x, a1, b1), Node (_, y, a2, b2) ->
        if x <= y then make_heap x a1 (merge b1 h2)
        else make_heap y a2 (merge h1 b2)

  let insert x h = merge (Node (1, x, Empty, Empty)) h
  let find_min = function Empty -> None | Node (_, x, _, _) -> x
  let delete_min = function Empty -> Empty | Node (_, _, a, b) -> merge a b
end
