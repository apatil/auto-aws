open Yojson.Basic.Util
open Camlp4.PreCast
open Cow
(* open Core.Std *)

(* Define composition *)
let (%) f g = function (x) -> f (g x)

let fname = "/home/anand/auto_aws/botocore/botocore/data/aws/ec2/2014-09-01.api.json"
let signature = Yojson.Basic.from_file fname
let shapes = signature |> member "shapes"
(* let docs = member "documentation" signature in *)
(* let meta = member "metadata" signature in *)
(* This will produce a list of (string * json) tuples *)
(* let oper = signature |> member "operations" |> to_assoc in *)

let rec transformList _loc shapeName =
  let shape = shapes |> member shapeName in
  let uncapShapeName = String.uncapitalize shapeName in
  match shapes |> member shapeName |> member "type" |> to_string with
    | "list" -> (
        let memberTyp = shape |> member "member" |> member "shape" |> to_string |> transformBasicSubShape _loc
        in
        <:ctyp<$memberTyp$ list>>)
    | _ -> <:ctyp<$uid:uncapShapeName$>>
and transformBasicSubShape _loc shapeName = 
  match shapeName with
    | "Boolean" -> <:ctyp<bool>>
    | "Integer" | "Long" -> <:ctyp<int>>
    | "Float" | "Double" -> <:ctyp<float>>
    (* TODO: What we do with blob? *)
    | "Blob" -> <:ctyp<blob>>
    | "Timestamp" | "DateTime" -> <:ctyp<Date.t>>
    | "String" -> <:ctyp<string>>
    | _ -> transformList _loc shapeName

let ofName origin target = Printf.sprintf "%s_of_%s" target origin

let list_expression_of_expression_list _loc l =
  let appendOp = (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))) in
  let append e el = Ast.ExApp (_loc, (Ast.ExApp (_loc, appendOp, e)), el) in
  let base = <:expr<[]>> in
  (* ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
  List.fold_right append l base
(* let createTag tag data = <:expr< [`El ((("", $str:tag$), []), [`Data $data$])] >> *)
let shape2Record _loc (name,shape) = 
  let assoc = (shape |> member "members" |> to_assoc) in
  let shape2Field _loc (name, field) =
    let subShape = transformBasicSubShape _loc @@ to_string @@ member "shape" field in
    (* Does not work: <:rec_binding< name : $uid:subShape$;>> *)
    (_loc, (String.uncapitalize name), false, subShape)
  in
  let shape2XmlOf _loc (name, field) =
    let uname = String.uncapitalize name in
    let xoName = ofName uname "xml" in
    <:expr< [`El ((("", $str:name$), []), $uid:xoName$ x.$uid:uname$)] >>
  in
  let shape2OfXml _loc (name, field) =
    let uname = String.uncapitalize name in
    let oxName = ofName "xml" uname in
    (* TODO: Inject code for the dictOfXml function at the top. *)
    <:rec_binding< $lid:uname$ = $uid:oxName$ @@ __d.find $str:name$ >>
  in
  let createOfXml _loc name =
    let oxName = ofName "xml" name in
    let bindings = List.map (shape2OfXml _loc) assoc in
    let recExpr = Ast.ExRec (_loc, (Ast.rbSem_of_list bindings), (Ast.ExNil _loc)) in
    <:str_item<let $lid:oxName$ x = 
      let __d = dictOfXml x in
      $recExpr$>>
  in
  let createXmlOf _loc name =
    let xoName = ofName name "xml" in
    let entries = List.map (shape2XmlOf _loc) assoc in
    let entriesExpr = list_expression_of_expression_list _loc entries in
    <:str_item<let $lid:xoName$ x = $entriesExpr$>>
  in
  let entries = List.map (shape2Field _loc) assoc in
  (* TODO: Apply locationName here. *)
  (* TODO: Handle optional records here. *)
  let typeDef = Ast.record_type_of_list entries in
  let typeDecl = <:str_item<type $lid:name$ = {$typeDef$}>> in

  let code = [
    typeDecl;
    createOfXml _loc name;
    createXmlOf _loc name
  ] in
  <:str_item<$list:code$>>

(* We try to make AWS enum strings into variant types, but some enums are not legal OCaml identifiers. Examples:
  * not-applicable : We translate hyphens to camel casing.
  * t1.micro : We translate dsos to underscores.
  * Linux/UNIX (Amazon VPC) : We give up and type the field as a string, not an enum. 
These functions let us distinguish which enums can be variant types.*)
let canBeVariant name = 
  let testRe = Str.regexp "^[a-zA-Z0-9\\.-\\ ]+$" in
  Str.string_match testRe name 0
let allCanBeVariants enum =
  List.for_all (fun el -> el |> to_string |> canBeVariant) (to_list enum)

let shape2Variant _loc (name, shape) =
  let name2Variant name =
    let toCamel sep =
      fun (name) -> String.concat "" @@ List.map String.capitalize @@ Str.split (Str.regexp sep) name
    in
    String.capitalize @@ (toCamel "\\.") @@ (toCamel "-") @@ (toCamel "\\ ") name
  in
  let ofStringMatch constructor str =
    <:match_case<$uid:constructor$ -> $str:str$>>
  in
  let stringOfMatch str constructor =
    <:match_case<$str:str$ -> $uid:constructor$>>
  in
  let declLine constructor =
    <:ctyp<$uid:constructor$ >>
  in
  let getVariant (decl, so, os) el = 
    let origName = to_string el in
    let constructor = name2Variant @@ origName in
    (
      (declLine constructor) :: decl,
      (stringOfMatch origName constructor) :: so,
      (ofStringMatch constructor origName) :: os
    )
  in
  let createDeclaration name decl = <:str_item<type $lid:name$ = $list:decl$>> in
  (* TODO: This should be createStringOf and so on, the names are mixed up. *)
  let createOfString name os =
    let osName = Printf.sprintf "string_of_%s" name in
    <:str_item<let $lid:osName$ x = match x with $list:os$>>
  in
  (* TODO: implement invalid_arg,  *)
  let createStringOf name so =
    let defaultCase = <:match_case< "other" -> invalid_arg @@ Printf.sprintf "No variant of type %s corresponds to received value %s" name other>> in
    let soName = Printf.sprintf "%s_of_string" name in
    let soWithDefault = so @ [defaultCase] in
    <:str_item<let $lid:soName$ x = match x with $list:soWithDefault$>>
  in
  let createXmlOf name =
    let oxName = Printf.sprintf "xml_of_%s" name in
    let osName = Printf.sprintf "string_of_%s" name in    
    <:str_item<let $lid:oxName$ x = [`Data ($uid:osName$ x)]>>
  in
  let (decl, so, os) = List.fold_left getVariant ([], [], []) (shape |> member "enum" |> to_list) in
  let code = [
    createDeclaration name decl;
    createOfString name os;
    createStringOf name so;
    createXmlOf name;
    (* TODO: make an ofXml that inverts XmlOf. *)
  ] in
  <:str_item<$list:code$>>

let shape2Compound _loc (name, shape) =
  let t = shape |> member "type" |> to_string in
  let transformer = match t with
    | "structure" -> shape2Record
    | "string" -> shape2Variant
    | _ -> failwith @@ Printf.sprintf "Shape %s has unknown type %s" name t in
  transformer _loc (String.uncapitalize name, shape)

let isCompound (name, shape) = 
  match shape |> member "type" |> to_string with
    | "structure" -> true
    | "string" -> (
      let enum = shape |> member "enum" in 
      match enum with
        | `Null -> false
        | `List x when allCanBeVariants enum -> true
        | `List x -> false
        | _ -> failwith @@ Printf.sprintf "Shape %s has non-list enum" name)
    (* We don't generate separate types for lists. *)
    | _ -> false 

let getMemberDocs (name, el) =
  match el |> member "documentation" with
    | `String x -> Printf.sprintf "\n  <li> %s: %s <\\li>" name x
    | `Null -> ""
    | _ -> failwith "Unrecognized member documentation type."

let getDocs (name, shape)  =
  let recordDocs = if (shape |> member "type" |> to_string) = "structure" 
    then "\n<ul>" ^ (String.concat "" @@ List.map getMemberDocs (shape |> member "members" |> to_assoc)) ^ "\n<\\ul>"
    else "" in
  (* Printf.printf "%s\n" name; *)
  match shape |> member "documentation" with
    | `String x -> Printf.sprintf "\n(**\n%s%s\n**)\n" x recordDocs
    | `Null -> "\n"
    | _ -> failwith "Unrecognized documentation type."

let generateShapeWithDocs (name, shape) =(
  let typeNode = shape2Compound Loc.ghost (name, shape) in
  Printf.printf "%s" (getDocs (name, shape));
  Printers.OCaml.print_implem @@ typeNode)

let printModuleDocs signature =
  Printf.printf "\n(**\n%s\n**)\n" (signature |> member "documentation" |> to_string)
;;

printModuleDocs signature;;
List.iter generateShapeWithDocs (shapes |> to_assoc |> (List.filter isCompound))


