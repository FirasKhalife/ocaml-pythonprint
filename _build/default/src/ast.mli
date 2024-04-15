
type py_id = string

(* dotted name * alias *)
type import = py_id list * py_id option

type typed_id = py_id * py_type option

and py_type = 
  | TypeId of py_id
  | TypeGeneric of py_id * type_params

and type_params = py_type list

type params = typed_id list

type branch = expr * block

and block = stmt list

and assigned_expr =
  | AssignedExpr of expr
  | AssignedYield of yield

and yield =
  (* yield from expr *)
  | YieldFrom of expr
  (* yield | yield exp1, exp2 *)
  | YieldRaw of expr list

and class_block = class_stmt list

and class_stmt =
  | ClassPass
  | ClassDecorator of py_id
  | ClassVarDef of typed_id * expr
  | InstanceVarDef of py_id * py_type
  | MethodDef of py_id * type_params * params * block

and stmt =
  (* import: dotted_name list *)
  | Import of (py_id list) list
  (* from import: module * (dotted_name list * alias) list *)
  | FromImport of py_id * (py_id list * py_id option) list
  (* from import all: module *)
  | FromImportAll of py_id
  (* expression: expression *)
  | Expression of expr
  (* decorator: expression *)
  | Decorator of py_id
  (* yield: yield expression - only in function *)
  | Yield of yield
  (* return an expression - only in function *)
  | Return of expr
  (* assignment (let-in): typed_id * body *)
  | Assign of typed_id * assigned_expr
  (* type alias * existing type *)
  | TypeAlias of py_type * py_type
  (* type definition: py_id * var string *)
  | TypeDef of py_id * string
  (* raise exception: exception name * message * from expression *)
  | Raise of py_id * string * py_id option
  (* function definition: name * type params * parameters * body *)
  | FunDef of py_id * type_params * params * block
  (* class definition: name * type params * parent classes * body *)
  | ClassDef of py_id * type_params * py_type list * class_block
  (* pattern matching: expression * list of branches *)
  | Match of expr * branch list
  (* if statement: condition * elif block list * then block * else block *)
  | IfStmt of expr * block * (expr * block) list * block
  (* while loop: condition * body *)
  | While of expr * block
  (* for loop: variable * iterable * body *)
  | For of py_id * expr * block
  (* try-catch: try block * (exception name * target * catch block) list * finally block *)
  | Try of block * (py_id * py_id * block) list * block
  (* with: expression * target * body *)
  | With of expr * py_id * block
  (* print: expression *)
  | Print of expr list
  (* assert: expression, error message *)
  | Assert of expr * expr option
  (* delete: expr list *)
  | Delete of expr list
  (* global: py_id list *)
  | Global of py_id list
  (* nonlocal: py_id list *)
  | Nonlocal of py_id list
  (* comment *)
  | Comment of string
  (* pass *)
  | Pass
  (* break *)
  | Break
  (* continue *)
  | Continue

and expr =
  (* identifier - is not a statement! *)
  | Id of py_id
  (* typed identifier: py_id * py_type *)
  | TypedId of typed_id
  (* class field access, dotted in practice; e.g. (exprReturningClassInstance).prop1.prop2 *)
  | Attribute of expr * py_id list
  (* function application: expression * args *)
  | App of expr * expr list
  (* lambda abstraction: parameters * body *)
  | Lam of py_id list * expr
  (* if expression: cond then else *)
  | IfExpr of expr * expr * expr
  (* tuple *)
  | Tuple of expr list
  (* list *)
  | List of expr list
  (* set *)
  | Set of expr list
  (* dictionary *)
  | Dict of (expr * expr) list
  (* subscript *)
  | Subscript of expr * expr
  (* slice *)
  | Slice of expr option * expr option * expr option
  (* list comprehension, [expr for expr in expr if expr] *)
  | ListComp of expr * expr * expr list * expr option
  (* set comprehension, {expr for expr in expr if expr} *)
  | SetComp of expr * expr * expr list * expr option
  (* dictionary comprehension, {expr: expr for expr in expr if expr} *)
  | DictComp of expr * expr * expr * expr list * expr option
  (* generator expression, (expr for expr in expr if expr) *)
  | GenExpr of expr * expr * expr list * expr option
  (* string *)
  | String of string
  (* int *)
  | Int of int
  (* float *)
  | Float of float
  (* boolean *)
  | Bool of bool
  (* None *)
  | PyNone
  (* operators*)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | MatrixMul of expr * expr
  | Div of expr * expr
  | FloorDiv of expr * expr
  | Mod of expr * expr
  | Pow of expr * expr
  | LShift of expr * expr
  | RShift of expr * expr
  | BitOr of expr * expr
  | BitAnd of expr * expr
  | BitXor of expr * expr
  | BitNot of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Pos of expr
  | Neg of expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | LtE of expr * expr
  | Gt of expr * expr
  | GtE of expr * expr
  | Is of expr * expr
  | IsNot of expr * expr
  | In of expr * expr
  | NotIn of expr * expr