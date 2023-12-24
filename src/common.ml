(* To make code generation functional (and composable), we use a rope
   abstraction. In addition to these two [Rope] functions, we use [Rope.empty],
   [Rope.concat] and [Rope.to_string]. *)
let v = Rope.of_string
let ( ^ ) = Rope.( ^ )

exception NotSupported of string
