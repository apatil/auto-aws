open Yojson.Basic.Util
open Camlp4.PreCast
(* open Core.Std *)

(* Define composition *)
let (%) f g = function (x) -> f (g x)

let fname = "/home/anand/auto_aws/botocore/botocore/data/aws/ec2/2014-09-01.api.json"
let signature = Yojson.Basic.from_file fname
let notImplemented = 
  let _loc = Loc.ghost in
    <:str_item<failwith "Not Implemented">>

let transformBasicSubShape _loc shapeName = 
  match shapeName with
      "boolean" -> <:ctyp<bool>>
    | "integer" | "long" -> <:ctyp<int>>
    | "float" | "double" -> <:ctyp<float>>
    (* FIXME: What we do with blob. *)
    | "blob" -> <:ctyp<blob>>
    | "timestamp" | "dateTime" -> <:ctyp<Date.t>>
    | "string" -> <:ctyp<string>>
    | _ -> <:ctyp<$uid:shapeName$>>

(* TODO: Equip with an XML transformer. *)
let shape2Record _loc (name,shape) = 

  let shape2EntryList _loc (name, field) =
    let subShape = transformBasicSubShape _loc @@ String.uncapitalize @@ to_string @@ member "shape" field in 
    (_loc, (String.uncapitalize name), false, subShape)
  in
  
  let entries = List.map (shape2EntryList _loc) (shape |> member "members" |> to_assoc) in
  (* TODO: Would like to pipe the first argument through Ast.TyRec and Ast.TyDcl. Can this kind of thing be done without first-class variant constructors? *)
  (* TODO: Apply locationName here. *)
  (* TODO: Handle optional records here. *)
  let typeDef = Ast.TyRec (_loc, Ast.record_type_of_list entries) in
  (* TODO: Equip with an XML transformer. *)
  <:str_item<type $lid:name$ = $typeDef$>>

(* We try to make AWS enum strings into variant types, but some enums are not legal OCaml identifiers. Examples:
  * not-applicable : We translate hyphens to underscores, which are not used by ec2
  * t1.micro : We translate dots to single quotes, which are also not used by ec2 
  * Linux/UNIX (Amazon VPC) : We give up and type the field as a string, not an enum. 
These functions let us distinguish which enums can be variant types.*)
let canBeVariant name = 
  let testRe = Str.regexp "^[a-zA-Z0-9\\.-]+$" in
  Str.string_match testRe name 0
let allCanBeVariants enum =
  List.for_all (fun el -> el |> to_string |> canBeVariant) (to_list enum)
let name2Variant name =
  let hyphenToCamel name =
    String.concat "" @@ List.map String.capitalize @@ Str.split (Str.regexp "-") name
  in
  let dotToUnderscore name =
    Str.global_replace (Str.regexp "\\.") "_" name
  in
  String.capitalize @@ dotToUnderscore @@ hyphenToCamel name

(* TODO: Use the variantslib syntax transformer on enum types, and equip with an xml transformer. *)
let shape2Variant _loc (name, shape) =
  let getVariant el = 
    let s = name2Variant @@ to_string el in
    <:ctyp<$uid:s$ >>
  in 
  
  let values = List.map getVariant (shape |> member "enum" |> to_list) in
  <:str_item<type $lid:name$ = $list:values$>>

(* TODO: Equip with an XML transformer. *)
let shape2List _loc (name, shape) =
  let memberDef = shape |> member "member" in
  let memberTyp = memberDef |> member "shape" |> to_string |> String.uncapitalize |> transformBasicSubShape _loc in
  (* Is there a way to do this with all quotations? *)
  let typ = Ast.TyApp (_loc, <:ctyp<list>>, memberTyp) in
  let out = <:str_item<type $lid:name$ = $typ$>> in
  Printers.OCaml.print_implem out;
  (* The return value is computed, but validate locationName to be sure we haven't missed anything. *)
  match memberDef |> member "locationName" with
      `String "item" -> out
    (* If locationName is anything else, this can be a single value. But, for simplicity, we don't support the single value case. *)
    | `String x -> out
    | `Null -> out
    | _ -> failwith @@ Printf.sprintf "locationName was not string or null for %s" name

let shape2Compound _loc (name, shape) =
  let t = shape |> member "type" |> to_string in
  let transformer = match t with
      "structure" -> shape2Record
    | "list" -> shape2List
    | "string" -> shape2Variant
    | _ -> failwith @@ Printf.sprintf "Shape %s has unknown type %s" name t in
  transformer _loc (String.uncapitalize name, shape)

let isCompound (name, shape) = 
  match shape |> member "type" |> to_string with
      "structure" | "list" -> true
    | "string" -> (
      let enum = shape |> member "enum" in 
      match enum with
          `Null -> false
        | `List x when allCanBeVariants enum -> true
        | `List x -> true
        | _ -> failwith @@ Printf.sprintf "Shape %s has non-list enum" name)
    | _ -> false 

(* let docs = member "documentation" signature in *)
(* let meta = member "metadata" signature in *)

(* This will produce a list of (string * json) tuples *)
(* let oper = signature |> member "operations" |> to_assoc in *)
let shapes = signature |> member "shapes" |> to_assoc
let typeNodes = List.map (shape2Compound Loc.ghost) (List.filter isCompound shapes)

;;
List.iter Printers.OCaml.print_implem typeNodes
