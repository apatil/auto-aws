open Camlp4.PreCast

let _loc = Loc.ghost in

let cons = ["brunch"; "holla"] in
let thang = <:str_item<
type t =
    $Ast.TySum (_loc,
               Ast.tyOr_of_list
                 (List.map
                     (fun c -> <:ctyp< $uid:c$ >>)
                     cons))$

let to_string = function
    $Ast.mcOr_of_list
      (List.map
          (fun c -> <:match_case< $uid:c$ -> $`str:c$ >>)
          cons)$

let of_string = function
    $let ors =
       Ast.mcOr_of_list
         (List.map
             (fun c -> <:match_case< $`str:c$ -> $uid:c$ >>)
             cons) in
     Ast.McOr(_loc,
             ors,
             <:match_case< _ -> invalid_arg "bad string" >>)$

>> in List.iter Printers.OCaml.print_implem [thang ; thang]

