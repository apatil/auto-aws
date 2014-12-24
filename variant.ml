open Camlp4.PreCast
open Yojson.Basic.Util

let fname = "/home/anand/auto_aws/botocore/botocore/data/aws/ec2/2014-09-01.api.json" in
let signature = Yojson.Basic.from_file fname in
let docs = member "documentation" signature in
let meta = member "metadata" signature in
(* This will produce a list of (string * json) tuples *)
let oper = signature |> member "operations" |> to_assoc in
(* You can use to_option, to_bool_option etc. with pattern matching to get a statically typed parser *)
(* Always fail on default and you'll be sure you cover everything *)
(* Don't use functors, just statically generate everything. *)


let _loc = Loc.ghost in

let cons = ["brunch"; "holla"] in
let thang = <:str_item<let x = 0>> in
(* let thang = <:str_item<
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

>> in *) 

List.iter Printers.OCaml.print_implem [thang ; thang]

