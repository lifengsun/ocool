open Core.Std
open Ocool

let () =
  let fname = Sys.argv.(1) in
  Cool.parse_exn fname |> Semantics.run |> Ast.print fname
