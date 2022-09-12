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
