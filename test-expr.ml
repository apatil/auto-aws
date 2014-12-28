(* camlp4of test-expr.ml -printer o *)

let x = "Date.t"
let y = "int"
let values = ["A";"B"]
(* let sh = <:ctyp<int list>>;; *)

(* let el = (Ast.McArr (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "_")))),
             (Ast.ExNil _loc), <:expr<raise "BLARG">>)) *)
;;
let blarg = <:rec_binding<x : y,>>
(* <:str_item<let blarg x = match s with $l$>> *)
(* <:patt<Blah -> 0>> *)
(* <:str_item<type $lid:x$ = $sh$>> *)
(* <:str_item<type $lid:x$ = int list>> *)