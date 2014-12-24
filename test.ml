open Yojson.Basic.Util
open Camlp4.PreCast

let fname = "/home/anand/auto_aws/botocore/botocore/data/aws/ec2/2014-09-01.api.json"
let signature = Yojson.Basic.from_file fname
(* let docs = member "documentation" signature in *)
(* let meta = member "metadata" signature in *)

(* This will produce a list of (string * json) tuples *)
(* let oper = signature |> member "operations" |> to_assoc in *)

let shapes = signature |> member "shapes" |> to_assoc

(* You can use to_option, to_bool_option etc. with pattern matching to get a statically typed parser *)
(* Always fail on default and you'll be sure you cover everything *)
(* Don't use functors, just statically generate everything. *)

let shape2EntryList _loc (name, field) =
  let fstring = String.uncapitalize @@ to_string @@ member "shape" field in 
  (_loc, (String.uncapitalize name), false, <:ctyp<$uid:fstring$>>)

let shape2Record _loc (name,shape) = 
  (* print_string name; *)
  (* FIXME: There is only 'members' if type is 'structure'. If type is 'string', there may be 'enum'. If type is 'list', there will be 'member', singular. The shape2EntryList function needs to pattern match on 'type' and do needful. *)
  let entries = List.map (shape2EntryList _loc) (shape |> member "members" |> to_assoc) in
  (* NOTE: Can this kind of thing be cleaned up without first-class variant constructors? *)
  let typeDef = Ast.TyRec (_loc, Ast.record_type_of_list entries) in
  let typeDecl = Ast.TyDcl (_loc, name, [], typeDef, []) in
  (* NOTE: can't figure out how to get the name into the quotation:
  <:str_item<type $name$ = ... >>;; 
  is not happy for some reason.*)
  Printers.OCaml.print_implem <:str_item<type $typeDecl$>> (* = (Ast.StTyp (_loc,
       (Ast.TyDcl (_loc, name, [],
          (Ast.TyRec (_loc,
             (Ast.TySem (_loc,
                (Ast.TyCol (_loc,
                   (Ast.TyId (_loc, (Ast.IdLid (_loc, "f1")))),
                   (Ast.TyId (_loc, (Ast.IdLid (_loc, "t1")))))),
                (Ast.TyCol (_loc,
                   (Ast.TyId (_loc, (Ast.IdLid (_loc, "f2")))),
                   (Ast.TyId (_loc, (Ast.IdLid (_loc, "t2")))))))))),
          [])))) *);;

  (* <:str_item<type $name$ = {
    f1 : t1;
    f2 : t2;
  }>>;;  *)

List.iter (shape2Record Loc.ghost) shapes
 (* @@ List.map shape2Record shapes *)

(* <:expr<$0$>> *)
(* <:str_item<0>> *)
(* let x = $Ast.ExInt (_loc, "0")$ in 1 *)
(* <:str_item< type host_info =
    { hostname   : string;
      os_name    : string;
      cpu_arch   : string;
      timestamp  : Time.t;
    };; >> *)