#require "yojson";;
open Yojson.Basic.Util
;;
let fname = "/home/anand/auto_aws/botocore/botocore/data/aws/ec2/2014-09-01.api.json";;
let signature = Yojson.Basic.from_file fname;;