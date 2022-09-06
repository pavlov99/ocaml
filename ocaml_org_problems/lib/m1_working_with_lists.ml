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

(** Reverse a list *)
let rec list_rev = function [] -> [] | h :: t -> list_rev t @ [ h ]

let%test _ = list_rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]
