type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let rec mem t el =
  match t with
  | Empty -> false
  | Node (left, value, right) ->
      if el = value then true
      else if el < value then mem left el
      else mem right el

let%test _ = mem Empty 0 = false
let%test _ = mem (Node (Empty, 0, Empty)) 0 = true
let%test _ = mem (Node (Node (Empty, -1, Empty), 0, Empty)) (-1) = true

let rec insert t el =
  match t with
  | Empty -> Node (Empty, el, Empty)
  | Node (l, v, r) ->
      if el <= v then Node (insert l el, v, r) else Node (l, v, insert r el)

let%test _ = insert Empty 0 = Node (Empty, 0, Empty)

let%test _ =
  insert (Node (Empty, 0, Empty)) 1 = Node (Empty, 0, Node (Empty, 1, Empty))
