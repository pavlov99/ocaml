module UnbalancedSet = struct
  type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  let empty = Empty

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

let%test _ = UnbalancedSet.mem 0 Empty = false
let%test _ = UnbalancedSet.mem 0 (Node (Empty, 0, Empty)) = true

let%test _ =
  UnbalancedSet.mem (-1) (Node (Node (Empty, -1, Empty), 0, Empty)) = true

let%test _ = UnbalancedSet.insert 0 Empty = Node (Empty, 0, Empty)

let%test _ =
  UnbalancedSet.insert 0 (Node (Empty, 0, Empty)) = Node (Empty, 0, Empty)

let%test _ =
  UnbalancedSet.insert 1 (Node (Empty, 0, Empty))
  = Node (Empty, 0, Node (Empty, 1, Empty))
