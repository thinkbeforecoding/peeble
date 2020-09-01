module Peeble.PhpAst



/// A php constant
type PhpConst =
    | PhpConstNumber of float
    | PhpConstString of string
    | PhpConstBool of bool
    | PhpConstNull
    | PhpConstUnit  // this is emited by f# and output as NULL

/// A php array index to access an Array.
/// Can be either an int or a string
type PhpArrayIndex =
    | PhpArrayNoIndex       // reserved for inserting at the end of an array
    | PhpArrayInt of int
    | PhpArrayString of string

/// A php class field for read or write access
type PhpField =
    { Name: string 
      Type: string }

/// A php var capture for anonymous functions
/// In php vars are not captures by default in closures and have to
/// be explicitely declared with the 'use()' keyword.
type Capture =
    | ByValue of string     // the var is captured by value and will not modify the source one
    | ByRef of string       // the var is captured by ref and changes to it will modify the source one

/// A php property
type Prop =
    | Field of PhpField     // the type is known
    | StrField of string    // only name is known

/// A php expression
and PhpExpr =
      ///Php var usage
    | PhpVar of string * typ: PhpType option
      // Global var usage (using $GLOBAL[])
    | PhpGlobal of string           
      // Php constant
    | PhpConst of PhpConst
      /// Php unary operation
    | PhpUnaryOp of string * PhpExpr
      /// Php binary operation
    | PhpBinaryOp of string *PhpExpr * PhpExpr 
      /// Php property access
    | PhpProp of obj:PhpExpr * Prop * typ: PhpType option // the property is accessed on the result of an expr
      /// Php array access
    | PhpArrayAccess of array:PhpExpr * index:PhpExpr 
      /// Php object instanciation
    | PhpNew of ty:PhpType * args:PhpExpr list
      /// Php array definition
    | PhpArray of args: (PhpArrayIndex * PhpExpr) list
      /// Php function call
    | PhpCall of f: PhpExpr * args: PhpExpr list
      /// Php method call
    | PhpMethod of this: PhpExpr * func:string * args: PhpExpr list
      /// Php ternary operator construct
    | PhpTernary of gard: PhpExpr * thenExpr: PhpExpr * elseExpr: PhpExpr
      /// Php is_a operation
    | PhpIsA of expr: PhpExpr * PhpType
      /// Php anonymous function definition.
      /// the uses capture list indicates which variables are imported from external context.
    | PhpAnonymousFunc of args: string list * uses: Capture list * body: PhpStatement list
      /// Php macro used for Emit interop.
    | PhpMacro of macro: string * args: PhpExpr list
   
/// Php statement
and PhpStatement =
      /// return keyword
    | Return of PhpExpr
      /// simple expression evalation
    | Expr of PhpExpr
      /// switch control flow
    | Switch of PhpExpr * (PhpCase * PhpStatement list) list
      /// break keyword (required in php).
    | Break
      /// Assign target variable to expression value
    | Assign of target:PhpExpr * value:PhpExpr
      /// if control flow
    | If of guard: PhpExpr * thenCase: PhpStatement list * elseCase: PhpStatement list
      /// throw exception
    | Throw of string
      /// do (don't use return)
    | Do of PhpExpr
/// Php case for switch statement
and PhpCase =
      /// int Php switch case
    | IntCase of int
      /// string Php switch case
    | StringCase of string
      /// default final case
    | DefaultCase

/// Php function declaration
and PhpFun = 
    { /// function name
      Name: string
      /// function arguments
      Args: string list
      /// argument deconstruction at the begining of the function
      Matchings: PhpStatement list
      /// function body
      Body: PhpStatement list
      /// indicates whether the function is static
      Static: bool
    }

/// Php type definition
and PhpType =
    { /// Type name
      Name: string
      /// fields definitions
      Fields: PhpField list
      /// methods definitions
      Methods: PhpFun list
      /// indicates whether type is abstract
      Abstract: bool
      /// the type base type if any
      BaseType: PhpType option
      /// defined interfaces
      Interfaces: PhpType list
    }

/// Php declaration
type PhpDecl =
      /// Php function declaration
    | PhpFun of PhpFun
      /// Php value declaration
    | PhpDeclValue of name:string * PhpExpr
      /// Php type declaration
    | PhpType of PhpType
      /// Simple php action
    | PhpDeclAction of PhpExpr

/// Php file content
type PhpFile =
    { Decls: (int * PhpDecl) list }


