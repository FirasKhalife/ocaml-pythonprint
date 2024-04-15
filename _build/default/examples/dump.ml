
open Python_pp.Ast
open Python_pp.Public
open Python_pp.Print
open Python_pp.Utils

;;

print_block [Assign (("app", None), AssignedExpr (Lam (["f"; "x"], App (Id "f", [Id "x"]))))];;
brkln ();;

let fields = 
  combine_ln 
  [typed_id ("speed", Some (TypeId "int")); 
   typed_id ("model", Some (TypeId "M"))]
in
println_combine 
  [decorator "dataclass";
   class_def "Car" [TypeId "M"] [TypeGeneric ("Vehicle", [TypeId "M"])] fields];;
brkln ();;

println (for_stmt "i" (app (py_id "range") [of_int 10]) (print (py_id "i")));;
brkln ();;

println (while_stmt (lt $ py_id "i" $ py_int 10) (var_def ("i", None) (add $ py_id "i" $ py_int 1)));;
brkln ();;

let list_type = TypeGeneric ("List", [TypeId "A"])

let list_class = 
  [Decorator "dataclass"; 
   ClassDef ("List", [TypeId "A"], [], [ClassPass])]
let nil_class = 
  [Decorator "dataclass"; 
   ClassDef ("Nil", [TypeId "A"], [list_type], [ClassPass])]
let cons_class = 
  [Decorator "dataclass"; 
   ClassDef ("Cons", [TypeId "A"], [list_type], 
      [InstanceVarDef ("head", TypeId "A"); 
       InstanceVarDef ("tl", list_type)])]
;;

print_block $
  [FromImport ("dataclasses", [(["dataclass"], None)]);
   FromImport ("typing", [(["TypeVar"], None); (["Callable"], None)])] @
  list_class @ 
  nil_class @ 
  cons_class @
  [FunDef (
    "app", 
    [TypeId "A"], 
    ["l1", Some (TypeGeneric ("List", [TypeId "A"])); 
     "l2", Some (TypeGeneric ("List", [TypeId "A"]));],
    [Match (Id "l1", 
      [App (Id "Nil", []), 
        [Return (Id "l2")];
       App (Id "Cons", [Id "x"; Id "xs"]), 
        [Return (App (Id "Cons", [Id "x"; App (Id "app", [Id "xs"; Id "l2"])]))]
      ])]
  )] @
  [Expression (
    Mul (
      Add (App (Lam (["x";"y"], Id "x"), [Int 1; Int 2]), Int 10),
      IfExpr (
        Lt (Add (Int 1, Int 2), Int 3), 
        App (Lam (["x"], Sub (Id "x", Int 1)), [Int 2]),
        App (Id "Nil", []))))
  ];;
brkln ();;

disable_verification ();;
(* successful *)
print_stmt 
  (Return (Div (Int 1, IfExpr (Eq (Int 2, Int 2), Add (Int 5, Neg (Int 10)), Sub (Int 79, Int 21)))));;
brkln ();;

enable_verification ();;
(* Fatal error: exception Invalid_argument("Return statement outside of function") *)
print_stmt 
(Return (Div (Int 1, IfExpr (Eq (Int 2, Int 2), Add (Int 5, Neg (Int 10)), Sub (Int 79, Int 21)))));;
