open Ast

let basic_classes = [
  `Class ("Object", "",       [
	  `Method ("abort",     [], "Object",
		   `InterExpr "Object.abort");
	  `Method ("type_name", [], "String",
		   `InterExpr "Object.type_name");
	  `Method ("copy",      [], "SELF_TYPE",
		   `InterExpr "Object.copy")
	]);
  `Class ("IO",     "Object", [
	  `Method ("out_string", [`Formal ("x", "String")], "SELF_TYPE",
		   `InterExpr "IO.out_string");
	  `Method ("out_int",    [`Formal ("x", "Int")],    "SELF_TYPE",
		   `InterExpr "IO.out_int");
	  `Method ("in_string",  [], "String",
		   `InterExpr "IO.in_string");
	  `Method ("in_int",     [], "Int",
		   `InterExpr "IO.in_int")
	]);
  `Class ("Int",    "Object", []);
  `Class ("String",    "Object", [
	  `Method ("length", [], "Int",
		   `InterExpr "String.length");
	  `Method ("concat", [`Formal ("s", "String")], "String",
		   `InterExpr "String.concat");
	  `Method ("substr", [
		   `Formal ("i", "Int");
		   `Formal ("l", "Int")
		 ], "String",
		   `InterExpr "String.substr")
	]);
  `Class ("Bool",   "Object", [])
]
