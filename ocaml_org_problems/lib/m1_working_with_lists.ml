(** Tail of a list *)
let rec last = function [] -> None | [ e ] -> Some e | _ :: t -> last t

let%test _ = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test _ = last [] = None

(** Last two elements of a list *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ e1; e2 ] -> Some (e1, e2)
  | _ :: t -> last_two t

let%test _ = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test _ = last_two [ "a" ] = None

(** N'th element of a list *)
let rec list_nth n = function
  | [] -> None
  | h :: t -> if n = 0 then Some h else list_nth (n - 1) t

let%test _ = list_nth 2 [ "a"; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = list_nth 2 [ "a" ] = None

(** Length of a list *)
let rec list_length = function [] -> 0 | _ :: t -> 1 + list_length t

let%test _ = list_length [ "a"; "b"; "c" ] = 3
let%test _ = list_length [] = 0

(** Reverse a list
  See tail recursion notes https://v2.ocaml.org/api/List.html
  and appending to the end of the list anti-pattern
  https://stackoverflow.com/questions/6732524/what-is-the-easiest-way-to-add-an-element-to-the-end-of-the-list#answer-6735757*)
let list_rev list =
  let rec helper acc = function [] -> acc | h :: t -> helper (h :: acc) t in
  helper [] list

let%test _ = list_rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]

(** Palindrome
    Fast implementation: https://stackoverflow.com/questions/47098343/ocaml-function-to-check-if-list-is-palindrome-with-floorn-2-recursive-calls-an#answer-47100848*)
let is_palindrome list = list = List.rev list

let%test _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true
let%test _ = is_palindrome [ "a"; "b" ] = false

(** Flatten a list *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten list =
  match list with
  | [] -> []
  | One h :: t -> h :: flatten t
  | Many h :: t -> flatten h @ flatten t

let%test _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
