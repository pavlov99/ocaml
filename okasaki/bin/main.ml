let time f =
  let t = Unix.gettimeofday () in
  let _ = f () in
  Printf.printf "Execution time: %f seconds\n" (Unix.gettimeofday () -. t);
  ()

let rec fib n = if n < 3 then 1 else fib (n - 1) + fib (n - 2)
let () = time (fun () -> fib 20)
