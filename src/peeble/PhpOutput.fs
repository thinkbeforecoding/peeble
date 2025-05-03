﻿module Peeble.PhpOutput

open System
open System.IO
open PhpAst
/// Context to output the PhpAst as Php text
type Writer =
    { Writer: TextWriter
      Indent: int
      Precedence: int
      EmitPhpMark: bool
      EmitDeclNumber: bool}

    static member create(w, ?emitPhpMark, ?emitDeclNumber) =
        { Writer = w
          Indent = 0
          Precedence = Int32.MaxValue 
          EmitPhpMark = defaultArg emitPhpMark true
          EmitDeclNumber = defaultArg emitDeclNumber false
          }

/// increment indent
let indent ctx =
    { ctx with Indent = ctx.Indent + 1}

/// write spaces for context indent
let writeIndent  ctx =
    for _ in 1 .. ctx.Indent do
        ctx.Writer.Write("    ")

/// write text to context
let write ctx txt =
    ctx.Writer.Write(txt: string)

/// write text followed by a newline
let writeln ctx txt =
     ctx.Writer.WriteLine(txt: string)

/// write given text with indent
let writei ctx txt =
    writeIndent ctx
    write ctx txt

/// write text with index and newline
let writeiln ctx txt =
    writeIndent ctx
    writeln ctx txt
  
/// write a list of $ prefixed variable separated by commas
let writeVarList ctx vars =
    let mutable first = true
    for var in vars do
        if first then
            first <- false
        else
            write ctx ", "
        write ctx "$"
        write ctx var

/// write variables separated by commas in a 'use' specification
let writeUseList ctx vars =
    let mutable first = true
    for var in vars do
        if first then
            first <- false
        else
            write ctx ", "
        match var with
        | ByValue v ->
            write ctx "$"
            write ctx v
        | ByRef v ->
            write ctx "&$"
            write ctx v
         
module Precedence =
    let binary =
        function
        | "*" | "/" | "%"         -> 3
        | "+" | "-" | "."         -> 4
        | "<<" | ">>"             -> 5
        | "<" | "<=" | ">=" | ">" -> 7
        | "==" | "!=" | "===" 
        | "!==" | "<>" | "<=>"    -> 7
        | "&" -> 8
        | "^" -> 9
        | "|" -> 10
        | "&&" -> 11
        | "||" -> 12
        | "??" -> 13
        | op -> failwithf "Unknown binary operator %s" op


    let unary =
        function
        | "!" -> 2
        | "-" -> 4
        | "&" -> 8
        | op -> failwithf "Unknown unary operator %s" op

    let _new = 0 
    let instanceOf = 1
    let ternary = 14 
    let assign = 15
        

    let clear ctx = { ctx with Precedence = Int32.MaxValue} 

/// write given code using specified precedence. Adds enclosing parens if needed.
let withPrecedence ctx prec f =
    let useParens = prec > ctx.Precedence || (prec = 14 && ctx.Precedence = 14)
    let subCtx = { ctx with Precedence = prec }
    if useParens then
        write subCtx "("

    f subCtx

    if useParens then
        write subCtx ")"

/// write a PhpExp to context recursively
let rec writeExpr ctx expr =
    match expr with
    | PhpBinaryOp(op, left, right) ->
        withPrecedence ctx (Precedence.binary op)
            (fun subCtx ->
                writeExpr subCtx left
                write subCtx " "
                write subCtx op
                write subCtx " "
                writeExpr subCtx right)

    | PhpUnaryOp(op, expr) ->
        withPrecedence ctx (Precedence.unary op)
            (fun subCtx ->
                write subCtx op
                writeExpr subCtx expr )
    | PhpConst cst -> 
        match cst with
        | PhpConstNumber n -> write ctx (string n)
        | PhpConstString s -> 
            write ctx "'"
            write ctx (s.Replace("'",@"\'"))
            write ctx "'"
        | PhpConstBool true -> write ctx "true"
        | PhpConstBool false -> write ctx "false"
        | PhpConstNull -> write ctx "NULL"
        | PhpConstUnit -> write ctx "NULL"
    | PhpVar (v,_) -> 
        write ctx "$"
        write ctx v
    | PhpGlobal v -> 
        write ctx "$GLOBALS['"
        write ctx v
        write ctx "']"
    | PhpProp(l,r, _) ->
        writeExpr ctx l
        write ctx "->"
        match r with
        | Field r -> write ctx r.Name
        | StrField r -> write ctx r
    | PhpNew(t,args) ->
        withPrecedence ctx (Precedence._new)
            (fun subCtx ->
                write subCtx "new "
                write subCtx t.Name
                write subCtx "("
                writeArgs subCtx args
                write subCtx ")")
    | PhpArray(args) ->
        write ctx "[ "
        let mutable first = true
        for key,value in args do
            if first then
                first <- false
            else
                write ctx ", "
            writeArrayIndex ctx key
            writeExpr ctx value
        write ctx "]"
    | PhpArrayAccess(array, index) ->
        writeExpr ctx array
        write ctx "["
        writeExpr ctx index
        write ctx "]"

    | PhpCall(f,args) ->
        let anonymous = match f with PhpAnonymousFunc _ -> true | _ -> false
        if anonymous then
            write ctx "("
        match f with
        | PhpConst (PhpConstString f) ->
            write ctx f
        | PhpVar (name, _) ->
            write ctx name
        | _ -> writeExpr ctx f
        if anonymous then
            write ctx ")"
        write ctx "("
        writeArgs ctx args
        write ctx ")"
    | PhpMethod(this,f,args) ->
        writeExpr ctx this
        write ctx "->"
        write ctx f
        write ctx "("
        writeArgs ctx args
        write ctx ")"
    | PhpTernary (guard, thenExpr, elseExpr) ->
        withPrecedence ctx (Precedence.ternary)
            (fun ctx ->
                writeExpr ctx guard
                write ctx " ? "
                writeExpr ctx thenExpr
                write ctx " : "
                writeExpr ctx elseExpr)
    | PhpIsA (expr, t) ->
        withPrecedence ctx (Precedence.instanceOf)
            (fun ctx ->
                writeExpr ctx expr
                write ctx " instanceof "
                write ctx t.Name)
    | PhpAnonymousFunc(args, uses, body) ->
        write ctx "function ("
        writeVarList ctx args
        write ctx ")"
        match uses with
        | [] -> ()
        | _ ->
            write ctx " use ("
            writeUseList ctx uses
            write ctx ")"
        
        write ctx " { "
        let multiline = body.Length > 1 
        let bodyCtx =
            if multiline then
                writeln ctx ""
                indent ctx
            else
                ctx
        for st in  body do
            writeStatement bodyCtx st
        if multiline then
            writei ctx "}"
        else
            write ctx " }"
    | PhpMacro(macro, args) ->
        // this is used for functions marked with the
        // Emit attribute
        let regex = System.Text.RegularExpressions.Regex("\$(?<n>\d)(?<s>\.\.\.)?")
        let matches = regex.Matches(macro)
        let mutable pos = 0
        for m in matches do
            let n = int m.Groups.["n"].Value
            write ctx (macro.Substring(pos,m.Index-pos))
            if m.Groups.["s"].Success then
                match args.[n] with
                | PhpArray items ->
                   let mutable first = true
                   for _,value in items do
                       if first then
                           first <- false
                       else
                           write ctx ", "
                       writeExpr ctx value 


                | _ -> failwith "Splice param should be a array"

            else
                writeExpr ctx args.[n]
            pos <- m.Index + m.Length
        write ctx (macro.Substring(pos))


and writeArgs ctx args =
    let mutable first = true
    for arg in args do
        if first then
            first <- false
        else
            write ctx ", "
        writeExpr ctx arg

and writeArrayIndex ctx index =
    match index with
    | PhpArrayString s  ->
        write ctx "'"
        write ctx s
        write ctx "' => "
    | PhpArrayInt n  ->
        write ctx (string n)
        write ctx " => "
    | PhpArrayNoIndex ->
        ()

/// Writes a Php Statement
and writeStatement ctx st =
    match st with
    | PhpStatement.Return expr ->
        writei ctx "return "
        writeExpr (Precedence.clear ctx) expr
        writeln ctx ";"
    | Expr expr ->
        writei ctx ""
        writeExpr (Precedence.clear ctx) expr
        writeln ctx ";"
    | Assign(name, expr) ->
        writei ctx ""
        writeExpr (Precedence.clear ctx)  name
        write ctx " = "
        writeExpr (Precedence.clear ctx)  expr
        writeln ctx ";"
    | Switch(expr, cases) ->
        writei ctx "switch ("
        writeExpr (Precedence.clear ctx)  expr
        writeln ctx ")"
        writeiln ctx "{"
        let casesCtx = indent ctx
        let caseCtx = indent casesCtx
        for case,sts in cases do
            match case with
            | IntCase i -> 
                writei casesCtx "case "
                write casesCtx (string i)
            | StringCase s -> 
                writei casesCtx "case '"
                write casesCtx s
                write casesCtx "'"
            | DefaultCase ->
                writei casesCtx "default"
            writeln casesCtx ":"
            for st in sts do
                writeStatement caseCtx st

        writeiln ctx "}"
    | Break ->
        writeiln ctx "break;"
    | If(guard, thenCase, elseCase) ->
        writei ctx "if ("
        writeExpr (Precedence.clear ctx) guard
        writeln ctx ") {"
        let body = indent ctx
        for st in thenCase do
            writeStatement body st
        writei ctx "}"
        if List.isEmpty elseCase then
            writeiln ctx ""
        else
            writeiln ctx " else {"
            for st in elseCase do
                writeStatement body st
            writeiln ctx "}"
    | Throw s ->
        writei ctx "throw new Exception('"
        write ctx s
        writeln ctx "');"
    | PhpStatement.Do (PhpConst PhpConstUnit)-> ()
    | PhpStatement.Do (expr) ->
        writei ctx ""
        writeExpr (Precedence.clear ctx) expr
        writeln ctx ";"


/// write a Php function
let writeFunc ctx (f: PhpFun) =
    writei ctx ""
    if f.Static then
        write ctx "static "
    
    write ctx "function "
    write ctx f.Name
    write ctx "("
    let mutable first = true
    for arg in f.Args do
        if first then
            first <- false
        else
            write ctx ", "
        write ctx "$"
        write ctx arg
    writeln ctx ") {"
    let bodyCtx = indent ctx
    for s in f.Matchings do
        writeStatement bodyCtx s

    for s in f.Body do
        writeStatement bodyCtx s
    writeiln ctx "}"
        
let writeField ctx (m: PhpField) =
    writei ctx "public $"
    write ctx m.Name
    writeln ctx ";"

let writeCtor ctx (t: PhpType) =
    writei ctx "function __construct("
    let mutable first = true
    for p in t.Fields do
        if first then
            first <- false
        else
            write ctx ", "
        //write ctx p.Type
        write ctx "$"
        write ctx p.Name
    writeln ctx ") {"
    let bodyctx = indent ctx
    for p in t.Fields do
        writei bodyctx "$this->"
        write bodyctx p.Name
        write bodyctx " = $"
        write bodyctx p.Name
        writeln bodyctx ";"

    writeiln ctx "}"

let writeType ctx (t: PhpType) =
    writei ctx ""
    if t.Abstract then
        write ctx "abstract "
    write ctx "class "
    write ctx t.Name
    match t.BaseType with
    | Some t ->
        write ctx " extends "
        write ctx t.Name
    | None -> ()

    if t.Interfaces <> [] then 
        write ctx " implements "
        let mutable first = true
        for itf in t.Interfaces do
            if first then
                first <- false
            else
                write ctx ", "
            write ctx itf.Name

    writeln ctx " {" 
    let mbctx = indent ctx
    for m in t.Fields do
        writeField mbctx m

    if not t.Abstract then
        writeCtor mbctx t

    for m in t.Methods do
        writeFunc mbctx m

    writeiln ctx "}"


let writeAssign ctx n expr =
    writei ctx "$GLOBALS['"
    write ctx n
    write ctx "'] = "
    writeExpr ctx expr
    writeln ctx ";"

let writeAction ctx expr =
    writeExpr ctx expr
    writeln ctx ";"


let writeDecl ctx d =
    match d with
    | PhpType t -> writeType ctx t
    | PhpFun t -> writeFunc ctx t
    | PhpDeclValue(n,expr) -> writeAssign ctx n expr
    | PhpDeclAction(expr) -> writeAction ctx expr


let writeFile ctx (file: PhpFile) =
    if ctx.EmitPhpMark then
        writeln ctx "<?php"
    for i,d in file.Decls do
        if ctx.EmitDeclNumber then
            writeln ctx ( "#" + string i)
        writeDecl ctx d
        writeln ctx ""




