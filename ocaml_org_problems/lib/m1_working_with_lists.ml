(** Tail of a list *)
let rec last = function [] -> None | [ e ] -> Some e | _ :: t -> last t

let%test _ = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test _ = last [] = None

(** Last two elements of a list *)
let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ e1; e2 ] -> Some (e1, e2)
  | _ :: t -> last_two t

let%test _ = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test _ = last_two [ "a" ] = None
