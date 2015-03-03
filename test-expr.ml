(* camlp4of test-expr.ml -printer o *)

let x = "Date.t"
let y = "int"
let burgh = <:expr< a + 3 >>
let values = [ burgh; burgh ] in
(* let sh = <:ctyp<int list>>;; *)

(* let el = (Ast.McArr (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "_")))),
             (Ast.ExNil _loc), <:expr<raise "BLARG">>)) *)


<:rec_binding<$lid:y$=0>>
(* <:str_item<let blarg x = match s with $l$>> *)
(* <:patt<Blah -> 0>> *)
(* <:str_item<type $lid:x$ = $sh$>> *)
(* <:str_item<type $lid:x$ = int list>> *)