(** Tail of a list *)
let rec last = function [] -> None | [ e ] -> Some e | _ :: t -> last t

(** Last two elements of a list *)
let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ e1; e2 ] -> Some (e1, e2)
  | _ :: t -> last_two t

let%test "failed" = false