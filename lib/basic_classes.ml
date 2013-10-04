open Ast

let basic_classes = [
  `Class ("Object", "",       [
	  `Method ("abort",     [], "Object",
		   `InterExpr ("Object.abort",     ref (Some "Object")));
	  `Method ("type_name", [], "String",
		   `InterExpr ("Object.type_name", ref (Some "String")));
	  `Method ("copy",      [], "SELF_TYPE",
		   `InterExpr ("Object.copy",      ref (Some "SELF_TYPE")))
	]);
  `Class ("IO",     "Object", [
	  `Method ("out_string", [`Formal ("x", "String")], "SELF_TYPE",
		   `InterExpr ("IO.out_string",    ref (Some "SELF_TYPE")));
	  `Method ("out_int",    [`Formal ("x", "Int")],    "SELF_TYPE",
		   `InterExpr ("IO.out_int",       ref (Some "SELF_TYPE")));
	  `Method ("in_string",  [], "String",
		   `InterExpr ("IO.in_string",     ref (Some "String")));
	  `Method ("in_int",     [], "Int",
		   `InterExpr ("IO.in_int",        ref (Some "Int")))
	]);
  `Class ("Int",    "Object", []);
  `Class ("String",    "Object", [
	  `Method ("length", [], "Int",
		   `InterExpr ("String.length",    ref (Some "Int")));
	  `Method ("concat", [`Formal ("s", "String")], "String",
		   `InterExpr ("String.concat",    ref (Some "String")));
	  `Method ("substr", [
		   `Formal ("i", "Int");
		   `Formal ("l", "Int")
		 ], "String",
		   `InterExpr ("String.substr",    ref (Some "String")))
	]);
  `Class ("Bool",   "Object", [])
]
