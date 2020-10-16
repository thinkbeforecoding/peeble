module Peeble.Transforms

open PhpAst
open BuiltinTypes
open Fable.AST
open FSharp.Compiler.SourceCodeServices

type PhpCompiler =
    { mutable Types: Map<string,PhpType> 
      mutable DecisionTargets: (Fable.Ident list * Fable.Expr) list
      mutable LocalVars: string Set
      mutable CapturedVars: Capture Set
      mutable MutableVars: string Set
      mutable Id: int
    }
    static member empty =

        { Types = Map.ofList [ "List" , PhpList.list
                               "Cons" , PhpList.cons
                               "Nil", PhpList.nil
                               "Result", PhpResult.result
                               "Ok", PhpResult.ok
                               "ResultError", PhpResult.error
                               ]  
          DecisionTargets = []
          LocalVars = Set.empty
          CapturedVars = Set.empty
          MutableVars = Set.empty
          Id = 0
          }
    member this.AddType(phpType: PhpType) =
        this.Types <- Map.add phpType.Name phpType this.Types
        phpType

    member this.AddLocalVar(var, isMutable) =
        if isMutable then
            this.MutableVars <- Set.add var this.MutableVars

        if this.CapturedVars.Contains(Capture.ByRef var) then
            ()
        elif this.CapturedVars.Contains(Capture.ByValue var) then
            this.CapturedVars <- this.CapturedVars |> Set.remove (Capture.ByValue var)  |> Set.add(ByRef var)
        else
            this.LocalVars <- Set.add var this.LocalVars

    member this.UseVar(var) =
        if not (Set.contains var this.LocalVars) && not (Set.contains (ByRef var) this.CapturedVars) then
            if Set.contains var this.MutableVars then
                this.CapturedVars <- Set.add (ByRef var) this.CapturedVars
            else
                this.CapturedVars <- Set.add (ByValue var) this.CapturedVars

    member this.UseVarByRef(var) =
        this.MutableVars <- Set.add var this.MutableVars
        if not (Set.contains var this.LocalVars) && not (Set.contains (ByValue var) this.CapturedVars) then
            this.CapturedVars <- Set.add (ByRef var) this.CapturedVars

    member this.UseVar(var) =
        match var with 
        | ByValue name -> this.UseVar name
        | ByRef name -> this.UseVarByRef name

    member this.MakeUniqueVar(name) =
        this.Id <- this.Id + 1
        "_" + name + "__" + string this.Id

    member this.NewScope() =
        { this with 
            LocalVars = Set.empty
            CapturedVars = Set.empty }


let convertType (t: FSharpType) =
    if (t.IsAbbreviation) then
        t.Format(FSharpDisplayContext.Empty.WithShortTypeNames(true))
    else
        match t with
        | Symbol.TypeWithDefinition entity ->
            match entity.CompiledName with
            | "FSharpSet`1" -> "Set"
            | name -> name
        | _ ->
            failwithf "%A" t
       

let fixName (name: string) =
    name.Replace('$','_')

let caseName (case: FSharpUnionCase) =
    let entity = case.ReturnType.TypeDefinition
    if entity.UnionCases.Count = 1 then
        case.Name
    elif entity.CompiledName = "FSharpResult`2" then
        if case.Name = "Ok" then
            case.Name
        else
            "ResultError"

    else
        entity.CompiledName + "_" + case.Name


let convertUnion (ctx: PhpCompiler) (info: Fable.UnionConstructorInfo) = 
    if info.Entity.UnionCases.Count = 1 then
        let case = info.Entity.UnionCases.[0] 
        [ let t =
            { Name = case.Name
              Fields = [ for e in case.UnionCaseFields do 
                            { Name = e.Name 
                              Type  = convertType e.FieldType } ]
              Methods = [ 
                  { PhpFun.Name = "get_FSharpCase"
                    PhpFun.Args = []
                    PhpFun.Matchings = []
                    PhpFun.Static = false
                    PhpFun.Body = 
                      [ PhpStatement.Return(PhpConst(PhpConstString(case.Name)))] } 
                  { PhpFun.Name = "CompareTo"
                    PhpFun.Args = ["other"]
                    PhpFun.Matchings = []
                    PhpFun.Static = false
                    PhpFun.Body =
                                      [ for e in case.UnionCaseFields do 
                                            let cmp = PhpVar(ctx.MakeUniqueVar "cmp",None)
                                            match e.FieldType.TypeDefinition.CompiledName with
                                            | "int" -> 
                                                Assign(cmp, 
                                                    PhpTernary( PhpBinaryOp(">", 
                                                                    PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                    PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ),
                                                                    PhpConst(PhpConstNumber 1.),
                                                                       PhpTernary(
                                                                           PhpBinaryOp("<", 
                                                                               PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                               PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None)),
                                                                               PhpConst(PhpConstNumber -1.), 
                                                                                PhpConst(PhpConstNumber 0.)
                                                                        
                                                    
                                                   ) ) )
                                            | _ ->
                                                Assign(cmp, 
                                                    PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                              "CompareTo",
                                                              [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ])
                                                
                                                )
                                            If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                [PhpStatement.Return cmp],
                                                []
                                            )
                                        PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                      ]
                    }
              ]
              Abstract = false
              BaseType = None
              Interfaces = [ PhpUnion.fSharpUnion; Core.icomparable ]
              }
          ctx.AddType(t) |> PhpType ]
    else
    [ let baseType =
            { Name = info.Entity.CompiledName
              Fields = []
              Methods = []
              Abstract = true 
              BaseType = None
              Interfaces = [PhpUnion.union; PhpUnion.fSharpUnion ]}
      ctx.AddType(baseType) |> PhpType

      for i, case in Seq.indexed info.Entity.UnionCases do
        let t = 
            { Name = caseName case
              Fields = [ for e in case.UnionCaseFields do 
                            { Name = e.Name 
                              Type  = convertType e.FieldType } ]
              Methods = [ { PhpFun.Name = "get_Case";
                            PhpFun.Args = []
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body = 
                                [ PhpStatement.Return(PhpConst(PhpConstString(caseName case)))]
                            } 
                          { PhpFun.Name = "get_FSharpCase";
                            PhpFun.Args = []
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body = 
                                [ PhpStatement.Return(PhpConst(PhpConstString(case.Name)))]
                            } 

                          { PhpFun.Name = "get_Tag"
                            PhpFun.Args = []
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body =
                                [ PhpStatement.Return(PhpConst(PhpConstNumber (float i)))]
                            }
                          { PhpFun.Name = "CompareTo"
                            PhpFun.Args = ["other"]
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body =
                                              [ let cmp = PhpVar(ctx.MakeUniqueVar "cmp",None)
                                                Assign(cmp, 
                                                    PhpTernary( PhpBinaryOp(">", 
                                                                    PhpMethod(PhpVar("this",None), "get_Tag", []),
                                                                    PhpMethod(PhpVar("other", None), "get_Tag", []) ),
                                                                    PhpConst(PhpConstNumber 1.),
                                                                       PhpTernary(
                                                                           PhpBinaryOp("<", 
                                                                               PhpMethod(PhpVar("this",None), "get_Tag", []),
                                                                               PhpMethod(PhpVar("other", None), "get_Tag" , [])),
                                                                               PhpConst(PhpConstNumber -1.), 
                                                                                PhpConst(PhpConstNumber 0.))))
                                                if not case.HasFields then
                                                    PhpStatement.Return(cmp)
                                                else
                                                    If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                        [PhpStatement.Return cmp],
                                                        []
                                                    )
                                                    for e in case.UnionCaseFields do 
                                                        let cmp = PhpVar(ctx.MakeUniqueVar "cmp",None)
                                                        match e.FieldType.TypeDefinition.CompiledName with
                                                        | "int" -> 
                                                            Assign(cmp, 
                                                                PhpTernary( PhpBinaryOp(">", 
                                                                                PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                                PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ),
                                                                                PhpConst(PhpConstNumber 1.),
                                                                                   PhpTernary(
                                                                                       PhpBinaryOp("<", 
                                                                                           PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                                           PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None)),
                                                                                           PhpConst(PhpConstNumber -1.), 
                                                                                            PhpConst(PhpConstNumber 0.)
                                                                                    
                                                                
                                                               ) ) )
                                                        | _ ->
                                                            Assign(cmp, 
                                                                PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                          "CompareTo",
                                                                          [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ])
                                                            
                                                            )
                                                        If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                            [PhpStatement.Return cmp],
                                                            []
                                                        )
                                                    PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                              ]
                            }

                            ]
              Abstract = false
              BaseType = Some baseType
              Interfaces = [ Core.icomparable ] }
        ctx.AddType(t) |> PhpType ]

let convertRecord (ctx: PhpCompiler) (info: Fable.CompilerGeneratedConstructorInfo) = 
    [ let t =
        { Name = info.Entity.CompiledName
          Fields = [ for e in info.Entity.FSharpFields do 
                        { Name = e.Name 
                          Type  = convertType e.FieldType } ]
          Methods = [ 
              { PhpFun.Name = "CompareTo"
                PhpFun.Args = ["other"]
                PhpFun.Matchings = []
                PhpFun.Static = false
                PhpFun.Body =
                                  [ for e in info.Entity.FSharpFields do
                                        let cmp = PhpVar(ctx.MakeUniqueVar "cmp",None)
                                        match e.FieldType.TypeDefinition.CompiledName with
                                        | "int"
                                        | "string" -> 
                                            Assign(cmp, 
                                                PhpTernary( PhpBinaryOp(">", 
                                                                PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ),
                                                                PhpConst(PhpConstNumber 1.),
                                                                   PhpTernary(
                                                                       PhpBinaryOp("<", 
                                                                           PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                                           PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None)),
                                                                           PhpConst(PhpConstNumber -1.), 
                                                                            PhpConst(PhpConstNumber 0.)
                                                                    
                                                
                                               ) ) )
                                        | _ ->
                                            Assign(cmp, 
                                                PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None),
                                                          "CompareTo",
                                                          [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType e.FieldType }, None) ])
                                            
                                            )
                                        If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                            [PhpStatement.Return cmp],
                                            []
                                        )
                                    PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                  ] }
            
          ]
          Abstract = false
          BaseType = None
          Interfaces = [ Core.icomparable ]}
      ctx.AddType(t) |> PhpType ]

type ReturnStrategy =
    | Return
    | Let of string
    | Do
    | Target of string


let convertTest ctx test phpExpr =
    match test with
    | Fable.TestKind.UnionCaseTest(case,_) ->
        let t = Map.find (caseName case) ctx.Types
        PhpIsA(phpExpr, t)
    | Fable.TestKind.ListTest(isCons) ->
        PhpIsA(phpExpr, if isCons then PhpList.cons else PhpList.nil)
    | Fable.OptionTest(isSome) ->
       let isNull = PhpCall(PhpConst (PhpConstString "is_null"), [phpExpr])
       if isSome then
           PhpUnaryOp("!",isNull)
       else
           isNull 


let rec getExprType =
    function
    | PhpVar(_, t) -> t
    | PhpProp(_,_, t) -> t
    | _ -> None

let rec convertExpr (ctx: PhpCompiler) (expr: Fable.Expr) =
    match expr with
    | Fable.Value(value,_) ->
        convertValue ctx value

    | Fable.Operation(Fable.BinaryOperation(op,left,right),t,_) ->
        let opstr =
            match op with
            | BinaryOperator.BinaryMultiply -> "*"
            | BinaryOperator.BinaryPlus ->
                match t with
                | Fable.Type.String -> "."
                | _ -> "+"
            | BinaryOperator.BinaryMinus -> "-"
            | BinaryOperator.BinaryLess -> "<"
            | BinaryOperator.BinaryGreater -> ">"
            | BinaryOperator.BinaryLessOrEqual -> "<="
            | BinaryOperator.BinaryGreaterOrEqual -> ">="
            | BinaryOperator.BinaryAndBitwise -> "&"
            | BinaryOperator.BinaryOrBitwise -> "|"
            | BinaryOperator.BinaryEqual -> "=="
            | BinaryOperator.BinaryEqualStrict -> "==="
            | BinaryOperator.BinaryUnequalStrict -> "!=="
            | BinaryOperator.BinaryModulus -> "%"
            | BinaryOperator.BinaryDivide -> "/"
        PhpBinaryOp(opstr, convertExpr ctx left, convertExpr ctx right)
    | Fable.Operation(Fable.UnaryOperation(op, expr),_,_) ->
        let opStr = 
            match op with
            | UnaryOperator.UnaryNot -> "!"
            | UnaryOperator.UnaryMinus -> "-"
            | UnaryOperator.UnaryPlus -> "+"

        PhpUnaryOp(opStr, convertExpr ctx expr)

    | Fable.Operation(Fable.Call(Fable.StaticCall(Fable.Import(Fable.Value(Fable.StringConstant s, _ ) ,p,k,ty,_)), args),t,_) ->
        match k,p with
        | Fable.ImportKind.Library, Fable.Value(Fable.StringConstant cls,_) ->
            match s with
            | "op_UnaryNegation_Int32" -> PhpUnaryOp("-", convertExpr ctx args.Args.[0])
            | "join" -> PhpCall(PhpConst(PhpConstString "join"), convertArgs ctx args)
            | _ -> 
                let phpCls =
                    match cls with
                    | "List" -> "FSharpList"
                    | "Array" -> "FSharpArray"
                    | _ -> cls


                PhpCall(PhpConst(PhpConstString (phpCls + "::" + fixName s)), convertArgs ctx args)
        | _ -> PhpCall(PhpConst(PhpConstString (fixName s)), convertArgs ctx args)
    | Fable.Operation(Fable.Call(Fable.StaticCall(Fable.Get(Fable.IdentExpr(i),Fable.ExprGet(Fable.Value(Fable.StringConstant(m),_)),_,_)),args),_,_) ->
        let f = 
            match i.Name ,m with
            | "Math", "abs" -> "abs"
            | name, m -> fixName name + "::" + fixName m
        PhpCall(PhpConst(PhpConstString (f)), convertArgs ctx args)
    | Fable.Operation(Fable.Call(Fable.StaticCall(Fable.IdentExpr(i)),args),_,_) ->
        //PhpCall(PhpConst(PhpConstString (fixName i.Name)), convertArgs ctx args)
        let name = fixName i.Name
        ctx.UseVarByRef(name)
        PhpCall(PhpVar(name, None), convertArgs ctx args)


        
    | Fable.Operation(Fable.Call(Fable.InstanceCall( Some (Fable.Value(Fable.StringConstant s, _ ))),({ Args = args; ThisArg = Some this} as argInfo)), _, _) ->
        match s, this.Type with
        | "filter", Fable.Type.Array _  -> PhpCall(PhpConst(PhpConstString ("FSharpArray::" + fixName s)), convertArgsThisLast ctx argInfo)
        | "findIndex", Fable.Type.Array _  -> PhpCall(PhpConst(PhpConstString ("FSharpArray::" + fixName s)), convertArgsThisLast ctx argInfo)
        
        | _ -> PhpMethod(convertExpr ctx this,fixName s, [for arg in args -> convertExpr ctx arg ] )
    | Fable.Operation(Fable.CurriedApply(expr, args),_,_) ->
        PhpCall(convertExpr ctx expr, [for arg in args -> convertExpr ctx arg]) 

    | Fable.Operation(Fable.Emit(macro,args),_,_) ->
        match args with
        | None -> PhpMacro(macro, [])
        | Some args ->
            PhpMacro(macro, [for arg in args.Args -> convertExpr ctx arg])
    | Fable.Get(expr, kind ,t,_) ->
        let phpExpr = convertExpr ctx expr
        match kind with 
        | Fable.UnionField(f,case,_) ->
            let name = caseName case
                
            let t = Map.find name ctx.Types
            let field = t.Fields |> List.tryFind (fun ff -> ff.Name = f.Name)
            match field with
            | Some field ->
                let fieldType = Map.tryFind field.Type ctx.Types
                PhpProp(phpExpr, Field field, fieldType)
            | None -> PhpProp(phpExpr, StrField f.Name, None)
        | Fable.OptionValue ->
            phpExpr
        | Fable.FieldGet(fieldName,_,_) ->
            match getExprType phpExpr with
            | Some phpType ->
                let field = phpType.Fields |> List.find (fun f -> f.Name = fieldName)
                PhpProp(phpExpr, Field field, Map.tryFind field.Type ctx.Types ) 
            | None -> PhpProp(phpExpr, StrField fieldName, None)
         
        | Fable.GetKind.TupleGet(id) ->
            PhpArrayAccess(phpExpr, PhpConst(PhpConstNumber (float id))) 
        | Fable.ExprGet(expr') ->
            let prop = convertExpr ctx expr'
            match prop with
            | PhpConst(PhpConstString "length") ->
                PhpCall(PhpConst(PhpConstString "count"), [phpExpr])
            | _ -> PhpArrayAccess(phpExpr, prop)
        | Fable.ListHead ->
            PhpProp(phpExpr, Field PhpList.value, getExprType phpExpr)
        | Fable.ListTail ->
            PhpProp(phpExpr, Field PhpList.next, getExprType phpExpr)
        | Fable.UnionTag ->
            PhpCall(PhpConst(PhpConstString ("get_class")), [phpExpr])



    | Fable.IdentExpr(id) ->
        let name = fixName id.Name
        ctx.UseVar(name)
        let phpType = 
            match id.Type with
            | Fable.Type.DeclaredType(e,_) ->
                Map.tryFind e.CompiledName ctx.Types

            | _ -> None 
        
        PhpVar(name, phpType)
    | Fable.Import(expr,p,k,t,_) ->
        match convertExpr ctx expr,t with
        | PhpConst (PhpConstString s), Fable.Any  ->
            match p with
            | Fable.Value(Fable.StringConstant p, _) when p <> "." -> PhpConst (PhpConstString ( p + "::" + fixName s))
            | _ -> PhpConst (PhpConstString (fixName s))
        | PhpConst (PhpConstString s), _ -> PhpGlobal (fixName s)
        | exp, _ -> exp

    | Fable.DecisionTree(expr,targets) ->
        let upperTargets = ctx.DecisionTargets
        ctx.DecisionTargets <- targets
        let phpExpr = convertExpr ctx expr
        ctx.DecisionTargets <- upperTargets
        phpExpr

    | Fable.IfThenElse(guard, thenExpr, elseExpr,_) ->
        PhpTernary(convertExpr ctx guard,
                    convertExpr ctx thenExpr,
                    convertExpr ctx elseExpr )
            

    | Fable.Test(expr, test , _ ) ->
        let phpExpr = convertExpr ctx expr
        convertTest ctx test phpExpr
            
        
    | Fable.DecisionTreeSuccess(index,[],_) ->
        let _,target = ctx.DecisionTargets.[index]
        convertExpr ctx target
    | Fable.DecisionTreeSuccess(index,boundValues,_) ->
        let bindings,target = ctx.DecisionTargets.[index]

        let args = List.map (convertExpr ctx) boundValues

        let innerCtx = ctx.NewScope()
        for id in bindings do
            innerCtx.AddLocalVar(fixName id.Name, id.IsMutable)
        let body = convertExprToStatement innerCtx target Return
        for capturedVar in innerCtx.CapturedVars do
            ctx.UseVar(capturedVar)
        PhpCall(
            PhpAnonymousFunc([ for id in bindings -> fixName id.Name ],
                Set.toList innerCtx.CapturedVars, body),
                args )


    | Fable.ObjectExpr(members, t, baseCall) ->
         PhpArray [
            for m in members do
                match m with
                | Fable.ObjectMember(Fable.Value(Fable.StringConstant key,_) ,value,kind) ->
                    PhpArrayString key , convertExpr ctx value
         ]
    | Fable.Function(kind,body,_) ->
        convertFunction ctx kind body

      
    | Fable.Let([], body) ->
        convertExpr ctx body
    | Fable.Let(bindings, body) ->
        let innerCtx = ctx.NewScope()
        for id,_ in bindings do
            innerCtx.AddLocalVar(fixName id.Name, id.IsMutable)
        let body = convertExprToStatement innerCtx expr Return
        for capturedVar in innerCtx.CapturedVars do
            ctx.UseVar(capturedVar)
        PhpCall(PhpAnonymousFunc([], Set.toList innerCtx.CapturedVars , body),[])

    | Fable.Expr.TypeCast(expr, t) ->
        convertExpr ctx expr
    | Fable.Expr.Sequential([Fable.Value(Fable.UnitConstant, _) ; body]) ->
        convertExpr ctx body
        


and convertArgs ctx (args: Fable.ArgInfo) =
    [ match args.ThisArg with
      | Some arg -> convertExpr ctx arg
      | None -> ()
      for arg in args.Args do 
        match arg with
        | Fable.IdentExpr({ Name = "Array"; Kind = Fable.CompilerGenerated }) -> ()
        | _ -> convertExpr ctx arg
    ]
and convertArgsThisLast ctx (args: Fable.ArgInfo) =
       [ 
         for arg in args.Args do 
           match arg with
           | Fable.IdentExpr({ Name = "Array"; Kind = Fable.CompilerGenerated }) -> ()
           | _ -> convertExpr ctx arg
         match args.ThisArg with
         | Some arg -> convertExpr ctx arg
         | None -> ()
       ]
            
        
and convertFunction (ctx: PhpCompiler) kind body =
    let scope = ctx.NewScope()
    let args = 
        match kind with
        | Fable.Lambda(arg) ->
            let argName = fixName arg.Name
            scope.AddLocalVar(argName, arg.IsMutable)
            [argName]
        | Fable.Delegate(args) ->
            [ for arg in args do
                let argName = fixName arg.Name
                scope.AddLocalVar(argName, arg.IsMutable)
                argName ]
 
    let phpBody = convertExprToStatement scope body Return

    for capturedVar in scope.CapturedVars do
        ctx.UseVar(capturedVar)
    PhpAnonymousFunc(args, Set.toList scope.CapturedVars , phpBody ) 

and convertValue (ctx:PhpCompiler) (value: Fable.ValueKind) =
    match value with
    | Fable.NewUnion(args,case,_,_) ->
        let t = Map.find (caseName case) ctx.Types
        PhpNew(t, [for arg in args do convertExpr ctx arg ])
    | Fable.NewTuple(args) ->
        
        PhpArray([for arg in args do (PhpArrayNoIndex, convertExpr ctx arg)])
    | Fable.NewRecord(args, Fable.DeclaredRecord(e), _) ->
        let t = ctx.Types.[e.CompiledName]
        PhpNew(t, [ for arg in args do convertExpr ctx arg ] )
        

    | Fable.NumberConstant(v,_) ->
        PhpConst(PhpConstNumber v)
    | Fable.StringConstant(s) ->
        PhpConst(PhpConstString s)
    | Fable.BoolConstant(b) ->
        PhpConst(PhpConstBool b)
    | Fable.UnitConstant ->
        PhpConst(PhpConstUnit)
    | Fable.Null _ ->
        PhpConst(PhpConstNull)
    | Fable.NewList(Some(head,tail),_) ->
        PhpNew(PhpList.cons, [convertExpr ctx head; convertExpr ctx tail])
    | Fable.NewList(None,_) ->
        PhpCall(PhpConst(PhpConstString("FSharpList::get_Nil")),[])
    | Fable.NewArray(Fable.NewArrayKind.ArrayValues(values),_) ->
        PhpArray([for v in values -> (PhpArrayNoIndex, convertExpr ctx v)])

    | Fable.NewOption(opt,_) ->
        match opt with
        | Some expr -> convertExpr ctx expr
        | None -> PhpConst(PhpConstNull)
    



and canBeCompiledAsSwitch evalExpr tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(caseExpr, Fable.UnionCaseTest(case,e),_), Fable.DecisionTreeSuccess(index,_,_), elseExpr,_) 
        when caseExpr = evalExpr ->
        canBeCompiledAsSwitch evalExpr elseExpr
    | Fable.DecisionTreeSuccess(index, _,_) ->
        true
    | _ -> false

and findCasesNames evalExpr tree =

    [ match tree with
      | Fable.IfThenElse(Fable.Test(caseExpr, Fable.UnionCaseTest(case,e),_), Fable.DecisionTreeSuccess(index,bindings,_), elseExpr,_)
            when caseExpr = evalExpr ->
            Some case, bindings, index
            yield! findCasesNames evalExpr elseExpr
      | Fable.DecisionTreeSuccess(index, bindings,_) ->
            None, bindings, index
      | _ -> ()
    ]

and hasGroupedCases indices tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(_, _, _), Fable.DecisionTreeSuccess(index,_,_), elseExpr,_) ->
        if Set.contains index indices then
            true
        else
            hasGroupedCases (Set.add index indices) elseExpr
    | Fable.DecisionTreeSuccess(index, _, _) ->
        if Set.contains index indices then
            true
        else
            false
    | Fable.IfThenElse(Fable.Test(_, _, _), _,_,_) ->
        false

and getCases cases tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(_, _, _), Fable.DecisionTreeSuccess(index,boundValues,_), elseExpr,_) ->
        getCases (Map.add index boundValues cases) elseExpr
    | Fable.DecisionTreeSuccess(index, boundValues, _) ->
        Map.add index boundValues cases
    | Fable.IfThenElse(Fable.Test(_, _, _), _,_,_) ->
        cases


and convertMatching ctx input guard thenExpr elseExpr expr returnStrategy =
    if (canBeCompiledAsSwitch expr input) then
        let cases = findCasesNames expr input 
        let inputExpr = convertExpr ctx expr
        [ Switch(PhpCall(PhpConst(PhpConstString("get_class")), [inputExpr]),
            [ for case,bindings, i in cases ->
                let idents,target = ctx.DecisionTargets.[i]
                let phpCase =
                    match case with
                    | Some c -> StringCase (caseName c)
                    | None -> DefaultCase


                phpCase, 
                    [ for ident, binding in List.zip idents bindings do
                        ctx.AddLocalVar(fixName ident.Name, ident.IsMutable)
                        Assign(PhpVar(fixName ident.Name, None), convertExpr ctx binding)
                      match returnStrategy with
                      | Target t -> 
                            ctx.AddLocalVar(fixName t, false)
                            Assign(PhpVar(fixName t, None), PhpConst(PhpConstNumber(float i)))
                            Break;
                      | Return _ ->
                            yield! convertExprToStatement ctx target returnStrategy
                      | _ -> 
                            yield! convertExprToStatement ctx target returnStrategy
                            Break
                    ]] 
            )
        
        ]
    else
        [ If(convertExpr ctx guard, convertExprToStatement ctx thenExpr returnStrategy, convertExprToStatement ctx elseExpr returnStrategy) ]

and convertExprToStatement ctx expr returnStrategy =
    match expr with
    | Fable.DecisionTree(input, targets) ->

        let upperTargets = ctx.DecisionTargets 
        ctx.DecisionTargets <- targets
        let phpExpr = convertExprToStatement ctx input returnStrategy
        ctx.DecisionTargets <- upperTargets
        phpExpr
    | Fable.IfThenElse(Fable.Test(expr, Fable.TestKind.UnionCaseTest(case,entity), _) as guard, thenExpr , elseExpr, _) as input ->
        let groupCases = hasGroupedCases Set.empty input
        if groupCases then
            let targetName = ctx.MakeUniqueVar("target")
            let targetVar = PhpVar(targetName, None)
            let switch1 = convertMatching ctx input guard thenExpr elseExpr expr (Target targetName)

            let cases = getCases Map.empty input
            let switch2 =
                Switch(targetVar,
                    [ for i, (idents,expr) in  List.indexed ctx.DecisionTargets do
                        IntCase i, [
                            match Map.tryFind i cases with
                            | Some case ->
                                // Assigns have already been made in switch 1
                                //for id, b in List.zip idents case do
                                //    ctx.AddLocalVar(fixName id.Name)
                                //    Assign(PhpVar(fixName id.Name, None), convertExpr ctx b)
                                yield! convertExprToStatement ctx expr returnStrategy
                            | None -> ()
                            match returnStrategy with
                            | Return _ -> ()
                            | _ -> Break;
                        ]
                    
                    ]
                )
            switch1 @ [ switch2 ]
                
        else
            convertMatching ctx input guard thenExpr elseExpr expr returnStrategy


    | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, _) ->
        let guard = convertExpr ctx guardExpr

        [ If(guard, convertExprToStatement ctx thenExpr returnStrategy,
                    convertExprToStatement ctx elseExpr returnStrategy) ]
    | Fable.DecisionTreeSuccess(index,boundValues,_) ->
        match returnStrategy with
        | Target target -> [ Assign(PhpVar(target,None), PhpConst(PhpConstNumber (float index))) ]
        | _ ->
            let idents,target = ctx.DecisionTargets.[index]
            [ for ident, boundValue in List.zip idents boundValues do
                ctx.AddLocalVar(fixName ident.Name, ident.IsMutable)
                Assign(PhpVar(fixName ident.Name, None), convertExpr ctx boundValue)
              yield! convertExprToStatement ctx target returnStrategy ]

    | Fable.Let(bindings,body) ->
        [ 
          for ident, expr in bindings do 
              let name = fixName ident.Name
              ctx.AddLocalVar(name, ident.IsMutable)
              yield! convertExprToStatement ctx expr (Let name)
          yield! convertExprToStatement ctx body returnStrategy ]

    | Fable.Sequential(exprs) ->
        if List.isEmpty exprs then
            []
        else
            [ for expr in exprs.[0..exprs.Length-2] do
                    yield! convertExprToStatement ctx expr Do
              yield! convertExprToStatement ctx exprs.[exprs.Length-1] returnStrategy
                    ]
    | Fable.Throw(Fable.Operation(Fable.Call(Fable.ConstructorCall(_ ),{ Args = [ Fable.Value(Fable.StringConstant s, _)]}),_,_) ,_,_) ->
        [ Throw(s) ]

    | Fable.Set(expr,kind,value,_) ->
        let left = convertExpr ctx expr
        match left with
        | PhpVar(v,_) -> 
            ctx.AddLocalVar(v, true)
        | _ -> ()
        [ Assign(left, convertExpr ctx value)]
            

    | _ ->
        match returnStrategy with
        | Return -> [ PhpStatement.Return (convertExpr ctx expr) ]
        | Let(var) -> 
            ctx.AddLocalVar(var, false)
            [ Assign(PhpVar(var,None), convertExpr ctx expr) ]
        | Do -> [ PhpStatement.Do (convertExpr ctx expr) ]
        | Target _ -> failwithf "Target should be assigned by decisiontree success"

let convertDecl ctx decl =
    match decl with
    | Fable.Declaration.ConstructorDeclaration(Fable.UnionConstructor(info),_) -> 
        convertUnion ctx info
    | Fable.Declaration.ConstructorDeclaration(Fable.CompilerGeneratedConstructor(info),_) -> 
        convertRecord ctx info
    | Fable.Declaration.ValueDeclaration(Fable.Function(Fable.FunctionKind.Delegate(args), body, Some name),decl) ->
       [{ PhpFun.Name = fixName name
          Args = [ for arg in args do 
                    fixName arg.Name ]
          Matchings = []
          Body = convertExprToStatement ctx body Return 
          Static = false } |> PhpFun ]
    | Fable.Declaration.ValueDeclaration(expr , decl) ->
        [ PhpDeclValue(fixName decl.Name, convertExpr ctx expr) ]
    | Fable.Declaration.ActionDeclaration(expr) ->
        [ PhpDeclAction(convertExpr ctx expr)]
    | _ -> [] 

