
open Format
open Utils
open Ast

let decorator = dprintf "@%s"

let py_id = str 

let py_none = str "None"

let attribute obj = function
  | [] -> obj
  | fl -> dprintf "%t.%t" obj (concat_sep "." identity fl)

let rec py_type = function
  | TypeId id -> str id
  | TypeGeneric (id, params) -> dprintf "%s%t" id (type_params params)

and type_params = function
  | [] -> mt
  | l -> brak_pat $ concat_map_comma_pp py_type l

let typed_id (name, t_opt) =
  dprintf "%s%t" name (get_or_mt_map (fun t -> dprintf ": %t" $ py_type t) t_opt)

let equal_ids id1 id2 = dprintf "%t = %t" id1 id2
let equal_typed_ids id1 id2 = dprintf "%t = %s" id1 id2 

let dict_item k v = dprintf "%t: %t" k v

let py_str s = dprintf "\'%s\'" s
let py_int = of_int
let py_float = of_float
let py_bool = of_bool

let lambda_def ?(nl = false) args body =
  dprintf "lambda%t: %t"
    (if args = [] then mt
     else (dprintf " %t" (concat_sep ", " identity args)))
    (if nl then dprintf "@[<v 4>(\n%t@]@\n)" body else body)

let dotted = concat_sep "." identity

let import names =
  let pp =
    match names with
    | [] -> raise (Invalid_argument "Empty import")
    | _ -> concat_map_comma_pp dotted names
  in
  dprintf "import %t" pp

let from_import modul names =
  let pp =
    match names with
    | [] -> raise (Invalid_argument "Empty import")
    | _ -> 
      concat_map_comma_pp (fun (id, alias) ->
        let id = dotted id in
        match alias with None -> id | Some a -> dprintf "%t as %s" id a) 
      names
  in
  dprintf "from %s import %t" modul pp

let from_import_all modul = dprintf "from %s import *" modul

let if_stmt cond then_block elifs else_block =
  let if_cond = dprintf "@[<v 4>if %t:@\n%t@]" cond then_block in
  let elifs =
    concat_sep_pp fnl 
      (fun (cond, body) -> dprintf "@\n@[<v 4>elif %t:@\n%t@]" cond body) 
      elifs 
  in
  let else_block = 
    if ismt (asprintf "%t" else_block) then mt
    else dprintf "@\n@[<v 4>else:@\n%t@]" else_block
  in
  dprintf "%t%t%t" if_cond elifs else_block

let while_stmt cond body =
  dprintf "@[<v 4>while %t:@\n%t@]" cond body

let for_stmt id iterable body =
  dprintf "@[<v 4>for %s in %t:@\n%t@]" id iterable body

let except exn_name target exn_body =
  dprintf "@[<v 4>except %s as %s:@\n%t@]" exn_name target exn_body

let try_except try_block exns finally_block =
  let try_block = dprintf "@[<v 4>try:@\n%t@]" try_block in
  let exns = 
    concat_sep_pp fnl (fun (name, target, body) -> except name target body) exns 
  in
  let finally_block = dprintf "@[<v 4>finally:@\n%t@]" finally_block 
  in
  dprintf "%t%t%t" try_block exns finally_block

let with_stmt expr target body =
  dprintf "@[<v 4>with %t as %s:@\n%t@]" expr target body

let comment_one_line = dprintf "# %s"

let comment = dprintf "''' @[<hv 0>%s@] '''"

let pass = str "pass"
let break = str "break"
let continue = str "continue"
let return = dprintf "return %t"
let delete = dprintf "del %t"
let global l = dprintf "global %t" (concat_comma l)
let nonlocal l = dprintf "nonlocal %t" (concat_comma l)
let print = dprintf "print(%t)"

let py_raise exn msg from = 
  if ismt from then dprintf "raise %s('%s')" exn from
  else dprintf "raise %s('%s') from %s" exn msg from

let py_assert expr expr_opt = 
  dprintf "assert %t%t" expr $
    get_or_mt_map (dprintf ", %t") expr_opt

let yield_from exp = dprintf "yield from %t" exp
let yield_raw l =
  match l with
  | [] -> str "yield"
  | _ -> dprintf "yield %t" $ concat_map_comma_pp identity l

(* TODO: DEFAULT PARAMETERS *)
let py_args args =
  par_pat $ concat_map_comma_pp typed_id args

let fun_def name params args body =
  dprintf "@[<v 4>def %s%t%t:@\n%t@]" 
    name (type_params params) (py_args args) body

let self_field = "self", None

let class_fields = function
  | [] -> mt
  | fields -> 
      concat_sep_pp fnl (fun (name, typ) -> 
        typed_id (name, (match typ with None -> Some (TypeId "Any") | Some _ -> typ)))
      fields

let match_args fields = 
  dprintf "__match_args__ = (%t)" (concat_map_comma (asprintf "'%s'") fields)

let init fields =
  let body =
    match fields with
    | [] -> pass
    | fields -> concat_sep_pp fnl (fun (name, _) -> dprintf "self.%s = %s" name name) fields
  in
  fun_def "__init__" [] (self_field :: fields) body

let getter ?(gettername = "") field : formatter -> unit =
  let fun_name =
    if ismt gettername then asprintf "get_%s" (fst field)
    else gettername
  in
  fun_def fun_name [] [self_field] (return $ attribute (py_id "self") [fst field])

let getters fields = concat_sep_pp fnl (fun (name, _) -> getter name) fields

let setter ?(settername = "") field =
  let fun_name =
    if ismt settername then asprintf "set_%s" (fst field)
    else settername
  in
  fun_def fun_name [] [self_field; field] (return $ attribute (py_id "self") [fst field])

let setters fields = concat_sep_pp fnl (fun field -> setter field) fields

let __str__ fields =
  let body =
    match fields with
    | [] -> return $ str "()"
    | fields -> 
        let list = concat_sep_pp (str ", ") (fun (name, _) -> dprintf "{%s}" name) fields in
        return $ dprintf "f'(%t)'" list
  in
  fun_def "__str__" [] [self_field] body

let class_def name pl parents body =
  dprintf "@[<v 4>class %s%t%t:@\n%t@]"
    name (type_params pl) 
    (par_if_not_mt parents $ concat_map_comma_pp py_type parents)
    body

let type_alias t1 t2 = dprintf "type %t = %t" (py_type t1) (py_type t2)

let type_alias_custom t1 input = dprintf "type %t = %s" (py_type t1) input

let type_def id name = dprintf "%s = TypeVar('%s')" id name

let var_def id expr = dprintf "%t = %t" (typed_id id) expr

(* TODO: NAMED PARAMETERS *)
let app f args = dprintf "%t(%t)" f (concat_map_comma_pp identity args)

let py_match head branches =
  dprintf "@[<v 4>match %t:@\n%t@]" head $
    concat_sep_pp fnl (fun (pat, body) -> dprintf "@[<v 4>case %t:@\n%t@]" pat body) branches

let py_match_custom head branches user_input =
  dprintf "@[<v 4>%s@\n(%t%t)" user_input
    (concat_sep_pp (dprintf "") (fun br -> dprintf "%t,@\n" br) branches)
    head

let pp_ifexpr cond pp_then pp_else =
  dprintf "%t if %t else %t" pp_then cond pp_else

let of_un_op_pp op_str e = combine_sep spc [str op_str; e] 
let of_bin_op_pp op_str e1 e2 = combine_sep spc [e1; str op_str; e2]

let pos e = of_un_op_pp "+" e
let neg e = of_un_op_pp "-" e
let not_ e = of_un_op_pp "not" e
let bit_not e = of_un_op_pp "~" e
let add e1 e2 = of_bin_op_pp "+" e1 e2
let sub e1 e2 = of_bin_op_pp "-" e1 e2
let mul e1 e2 = of_bin_op_pp "*" e1 e2
let matrix_mul e1 e2 = of_bin_op_pp "@" e1 e2
let div e1 e2 = of_bin_op_pp "/" e1 e2
let floor_div e1 e2 = of_bin_op_pp "//" e1 e2
let mod_ e1 e2 = of_bin_op_pp "%" e1 e2
let pow e1 e2 = of_bin_op_pp "**" e1 e2
let lshift e1 e2 = of_bin_op_pp "<<" e1 e2
let rshift e1 e2 = of_bin_op_pp ">>" e1 e2
let bit_or e1 e2 = of_bin_op_pp "|" e1 e2
let bit_xor e1 e2 = of_bin_op_pp "^" e1 e2
let bit_and e1 e2 = of_bin_op_pp "&" e1 e2
let eq e1 e2 = of_bin_op_pp "==" e1 e2
let neq e1 e2 = of_bin_op_pp "!=" e1 e2
let lt e1 e2 = of_bin_op_pp "<" e1 e2
let lte e1 e2 = of_bin_op_pp "<=" e1 e2
let gt e1 e2 = of_bin_op_pp ">" e1 e2
let gte e1 e2 = of_bin_op_pp ">=" e1 e2
let is_ e1 e2 = of_bin_op_pp "is" e1 e2
let is_not e1 e2 = of_bin_op_pp "is not" e1 e2
let in_ e1 e2 = of_bin_op_pp "in" e1 e2
let not_in e1 e2 = of_bin_op_pp "not in" e1 e2
