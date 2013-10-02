open Core.Std
open Ocool

let () = Semantics.semant (Cool.parse_exn Sys.argv.(1))
