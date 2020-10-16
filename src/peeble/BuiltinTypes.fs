module Peeble.BuiltinTypes

open PhpAst

module PhpList =
    let list  = { Name = "FSharpList"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = [] }
    let value = { Name = "value"; Type = "" }
    let next = { Name = "next"; Type = "FSharpList" }
    let cons = { Name = "Cons"; Fields = [ value; next ]; Methods = []; Abstract = false; BaseType = Some list; Interfaces = [] } 
    let nil = { Name = "Nil"; Fields = []; Methods = []; Abstract = false; BaseType = Some list; Interfaces = [] }

module PhpResult =
    let result = { Name = "Result"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []}
    let okValue = { Name = "ResultValue"; Type = ""}
    let ok = { Name = "Ok"; Fields = [okValue]; Methods = []; Abstract = true; BaseType = Some result; Interfaces = [] }
    let errorValue = { Name = "ErrorValue"; Type = ""}
    let error = { Name = "ResultError"; Fields = [errorValue] ; Methods = []; Abstract = true; BaseType = Some result; Interfaces = [] }

module PhpUnion =
    let union = { Name = "Union"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []}
    let fSharpUnion = { Name = "FSharpUnion"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []}

module Core =
    let icomparable = { Name = "iComparable"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = [] }

