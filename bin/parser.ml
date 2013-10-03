open Core.Std
open Ocool

let () =
  let fname = Sys.argv.(1) in
  Ast.print fname (Cool.parse_exn fname)
