let ec2_signatures =
	let open Yojson.Basic.Util in
	let fname = "/home/anand/ocaml_aws_client/botocore/botocore/data/aws/ec2/2014-09-01.api.json" in
	let signature = Yojson.Basic.from_file fname in
	let docs = member "documentation" signature in
	(* let meta = member "metadata" signature in *)
	(* let oper = member "operations" signature in *)

	print_string (to_string docs)