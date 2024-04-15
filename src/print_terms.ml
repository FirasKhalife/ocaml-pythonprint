
open Ast
open Print
open Utils

(** based on Python reference - operator precedence
    https://docs.python.org/3/reference/expressions.html#operator-precedence *)
let p_of_expr = function
  | Id _ | TypedId _ | Attribute _ -> 0
  | String _ | Int _ | Float _ | Bool _ | PyNone -> 0
  | App _ | Tuple _ | List _ | Set _ | Dict _ -> 0
  | Subscript _ | Slice _ | ListComp _ | SetComp _ | DictComp _ | GenExpr _ -> 0
  | Pow _ -> 10
  | Pos _ | Neg _ | BitNot _ -> 15
  | Mul _ | MatrixMul _ | Div _ | FloorDiv _ | Mod _ -> 20
  | Add _ | Sub _ -> 25
  | LShift _ | RShift _ -> 30
  | BitAnd _ -> 35
  | BitXor _ -> 40
  | BitOr _ -> 50
  | In _ | NotIn _ | Is _ | IsNot _
    | Lt _ | LtE _ | Gt _ | GtE _ | Neq _ | Eq _ -> 55
  | Not _ -> 60
  | And _ -> 65
  | Or _ -> 70
  | IfExpr _ -> 85
  | Lam _ -> 90

let rec of_assigned_expr ?(in_fun = false) ?(verify = true) ?(env = empty_env) = function
  | AssignedExpr e -> fst $ of_expr ~verify ~env e
  | AssignedYield y -> 
      if !verif && not in_fun then raise (Invalid_argument "Yield statement outside of function")
      else of_yield ~verify ~env y

and of_yield ?(in_fun = false) ?(verify = true) ?(env = empty_env) exp =
  if !verif && not in_fun then raise (Invalid_argument "Yield statement outside of function")
  else
    match exp with
    | YieldRaw exps -> 
        yield_raw (List.map (fun exp -> fst $ of_expr ?p:None ~verify ~env exp) exps)
    | YieldFrom exp -> 
        yield_from (fst $ of_expr ~verify ~env exp)

and of_class_block stmts =
  let rec fold_rec acc = function
  | [] -> acc
  | [stmt] -> Format.dprintf "%t%t" acc (of_class_stmt ~next:None stmt)
  | stmt :: next :: stmts ->
      fold_rec (Format.dprintf "%t%t" acc (of_class_stmt ~next:(Some next) stmt)) (next::stmts)
  in
  fold_rec mt stmts

and sep_inclass next_opt =
  let sep next =
    match next with
    | ClassVarDef _ | InstanceVarDef _ -> fnl
    | ClassDecorator _ -> fnl2
    | MethodDef _ -> fnl2
    | ClassPass -> mt
  in
  Option.map sep next_opt

and of_class_stmt ?(next = None) stmt =
  let stmt, sep =
    match stmt with
    | ClassPass -> pass, Some mt
    | ClassDecorator id -> decorator id, Some fnl
    | ClassVarDef (id, expr) -> var_def id (fst $ of_expr ~verify:false expr), sep_inclass next
    | InstanceVarDef (id, t) -> typed_id (id, Some t), sep_inclass next
    | MethodDef (id, params, args, body) -> 
        fun_def id params args (of_block ~verify:false ~in_fun:true body), Some fnl2
  in
  let stmt_str = Format.asprintf "%t" stmt in
  if ismt stmt_str then stmt
  else (if Option.is_some next then Format.dprintf "%t%t" stmt (Option.get sep) else stmt)

and of_block ?(in_fun = false) ?(verify = true) ?(env = empty_env) stmts =
  let rec fold_rec acc env = function
  | [] -> acc
  | [stmt] -> 
      let stmt, _ = of_stmt ~verify ~env ~in_fun ~next:None stmt in
      Format.dprintf "%t%t" acc stmt
  | stmt :: next :: stmts ->
      let stmt, env = of_stmt ~verify ~env ~in_fun ~next:(Some next) stmt in
      fold_rec (Format.dprintf "%t%t" acc stmt) env (next::stmts)
  in
  fold_rec mt env stmts

and of_global_block ?(in_fun = false) ?(verify = true) ?(env = empty_env) stmts =
  let rec fold_rec acc env = function
  | [] -> acc, env
  | [stmt] -> 
      let stmt, env' = of_stmt ~verify ~env ~in_fun ~next:None stmt in
      Format.dprintf "%t%t" acc stmt, env'
  | stmt :: next :: stmts ->
      let stmt, env = of_stmt ~verify ~env ~in_fun ~next:(Some next) stmt in
      fold_rec (Format.dprintf "%t%t" acc stmt) env (next::stmts)
  in
  fold_rec mt env stmts

and module_stmt_sep = function
  | None -> mt
  | Some next -> 
      match next with
      | Import _ | FromImport _ | FromImportAll _ -> fnl
      | _ -> fnl2

and of_stmt ?(in_fun = false) ?(next = None) ?(verify = true) ?(env = empty_env) stmt =
  let stmt, sep, env =
    match stmt with
    | Import names ->
        import names, module_stmt_sep next,
          push_id_list (List.map (fun name -> List.hd $ List.rev name) names) env
    | FromImport (modul, names) -> 
        from_import modul names, module_stmt_sep next,
          push_id_list (List.map (fun (name, _) -> List.hd $ List.rev name) names) env
    | FromImportAll modul -> 
        from_import_all modul, module_stmt_sep next, env
    | Expression exp -> fst $ of_expr ~verify ~env exp, fnl, env
    | Decorator id -> if !verif && verify then verify_id id env; decorator id, fnl, env
    | Yield yield -> of_yield ~in_fun ~env yield, fnl, env
    | Return e ->
        if !verif && not in_fun then raise (Invalid_argument "Return statement outside of function")
        else return $ fst (of_expr ~verify ~env e), fnl, env
    | Assign (id, expr) ->
        var_def id (of_assigned_expr ~in_fun ~verify ~env expr), fnl, push_id (fst id) env
    | TypeAlias (t1, t2) -> type_alias t1 t2, fnl, env
    | TypeDef (id, s) -> type_def id s, fnl, env
    | Raise (exn, msg, from) -> 
        let from = 
          match from with 
          | None -> "" 
          | Some id -> if !verif && verify then verify_id id env; id 
        in
        py_raise exn msg from, fnl, env
    | FunDef (id, params, args, body) -> 
        let arg_ids = List.map (fun (id, _) -> id) args in
        let env' = push_id_list (id :: arg_ids) env in
        fun_def id params args (of_block ~env:env' ~in_fun:true body), fnl2,
          push_id id env
    | ClassDef (id, type_params, parents, body) ->
        class_def id type_params parents (of_class_block body), fnl2,
          push_id id env
    | Match (head, cases) ->
        let branches = 
          List.map (fun (pat, stmts) -> 
            let pat, env' = 
              match pat with
              | App (id, args) -> 
                  let id, _ = of_expr ~verify ~env id in
                  let args, env' = fold_of_expr true false env args in
                  app id args, env'
              | _ -> of_expr ~verify:false ~assign:true ~env pat
            in
            pat, of_block ~env:env' ~in_fun stmts) 
          cases
        in
        py_match (fst $ of_expr ~verify ~env head) branches, fnl, env
    | IfStmt (cond, then_, elifs, else_) ->
        let elifs = 
          List.map (fun (cond, block) -> 
            fst $ of_expr ~verify ~env cond, 
            of_block ~env ~in_fun block) 
          elifs
        in
        if_stmt (fst $ of_expr ~verify ~env cond) 
          (of_block ~env ~in_fun then_) 
          elifs (of_block ~env ~in_fun else_), fnl, env
    | While (cond, body) -> 
        while_stmt (fst $ of_expr ~verify ~env cond) (of_block ~env ~in_fun body), fnl, env
    | For (id, exp, body) ->
        let env' = push_id id env in
        for_stmt id (fst $ of_expr ~verify ~env exp) (of_block ~env:env' ~in_fun body), fnl, env
    | Try (body, excepts, finally) ->
        let excepts = 
          List.map (fun (exn, id, block) ->
            let env' = push_id id env in 
            exn, id, of_block ~env:env' ~in_fun block) 
          excepts
        in
        try_except (of_block ~in_fun body) 
          excepts (of_block ~in_fun finally), fnl, env
    | With (exp, id, body) -> 
        let env' = push_id id env in
        with_stmt (fst $ of_expr ~verify ~env exp) id (of_block ~env:env' ~in_fun body), fnl, env
    | Print exps -> print $ concat_expr exps, fnl, env
    | Assert (exp, msg) -> 
        py_assert (fst $ of_expr ~verify ~env exp)
        (Option.map fst $ Option.map (of_expr ~verify ~env) msg), fnl, env
    | Delete exps -> delete $ concat_expr exps, fnl, env
    | Global ids -> global ids, fnl, env
    | Nonlocal ids -> nonlocal ids, fnl, env
    | Comment s -> comment_one_line s, fnl, env
    | Pass -> pass, fnl, env
    | Break -> break, fnl, env
    | Continue -> continue, fnl, env
  in
  let stmt_str = Format.asprintf "%t" stmt in
  if ismt stmt_str then stmt, env
  else (if Option.is_some next then Format.dprintf "%t%t" stmt sep else stmt), env

and concat_expr ?(p = 100) ?(verify = true) ?(env = empty_env) exps = 
  concat_map_comma_pp (fun exp -> fst $ of_expr ~p ~verify ~env exp) exps
and of_un_op ?(p = 100) ?(verify = true) ?(env = empty_env) ?(sep_spc = false) op_str e =
  let sep = if sep_spc then spc else mt in
  combine_sep sep [str op_str; fst $ of_expr ~p ~verify ~env e]
and of_bin_op ?(p = 100) ?(verify = true) ?(env = empty_env) op_str e1 e2 = 
  combine_sep spc [fst $ of_expr ~p ~verify ~env e1; str op_str; fst $ of_expr ~p ~verify ~env e2]

and fold_of_expr assign verify env args =
  let rec fold_rec env acc = function
  | [] -> List.rev acc, env
  | arg :: args ->
      let arg, env' = of_expr ~assign ~verify ~env arg in
      fold_rec env' (arg :: acc) args
  in
  fold_rec env [] args

(** [p] stands for the current precedence level
    [assign] specifies whether identifiers should be added to the environment
    [verify] specifies whether to verify identifiers; relevant only when [assign] is false. *)
and of_expr ?(p = 100) ?(assign = false) ?(verify = true) ?(env = empty_env) expr =
  let parent_p = p in
  let p = p_of_expr expr in
  let (pp: Format.formatter -> unit), (env: env) =
    match expr with
    | Id id -> 
        if assign then py_id id, push_id id env
        else
            (if !verif && verify then verify_id id env; py_id id, env)
    (* typed id can be used only in certain cases... distinguish between typed id and id *)
    | TypedId (s, t) -> 
        if assign then typed_id (s, t), push_id s env
        else typed_id (s, t), env
    | Attribute (obj, ids) -> 
        attribute (fst $ of_expr ~p ~verify ~env obj) ids, env
    | App (f, args) -> 
        let f, _ = of_expr ~p ~verify ~env f in
        let args, env' = fold_of_expr assign verify env args in
        if assign then app f args, env' else app f args, env
    | Lam (ids, body) ->
        let env' = push_id_list ids env in
        let nl = match body with Lam _ -> true | _ -> false in
        lambda_def ~nl ids (fst (of_expr ~p ~verify ~env:env' body)), env
    | IfExpr (cond, then_, else_) ->
        pp_ifexpr 
          (fst $ of_expr ~p ~verify ~env cond) 
          (fst $ of_expr ~p ~verify ~env then_) 
          (fst $ of_expr ~p ~verify ~env else_), env
    | Tuple args ->
        let args, env' = fold_of_expr assign verify env args in
        par_pat $ concat_comma_pp args, env'
    | List args ->
        let args, env' = fold_of_expr assign verify env args in
        brak_pat $ concat_comma_pp args, env'
    | Set args ->
        let args, env' = fold_of_expr assign verify env args in
        curly_pat $ concat_comma_pp args, env'
    | Dict args ->
        curly_pat $ concat_map_comma_pp (fun (k, v) -> 
          dict_item (fst $ of_expr ~verify ~env k) (fst $ of_expr ~verify ~env v)) args, env
    | Subscript _ -> failwith "Subscript not supported"
    | Slice _ -> failwith "Slice not supported"
    | ListComp _ -> failwith "ListComp not supported"
    | SetComp _ -> failwith "SetComp not supported"
    | DictComp _ -> failwith "DictComp not supported"
    | GenExpr _ -> failwith "GenExpr not supported"
    | String s -> if assign then py_str s, push_id s env else py_str s, env
    | Int i -> py_int i, env
    | Float f -> py_float f, env
    | Bool b -> py_bool b, env
    | PyNone -> py_none, env
    | Pos e              -> of_un_op ~p ~verify ~env "+" e, env
    | Neg e              -> of_un_op ~p ~verify ~env "-" e, env
    | Not e              -> of_un_op ~p ~verify ~env "not" e ~sep_spc:true, env
    | BitNot e           -> of_un_op ~p ~verify ~env "~" e, env
    | Add (e1, e2)       -> of_bin_op ~p ~verify ~env "+" e1 e2, env
    | Sub (e1, e2)       -> of_bin_op ~p ~verify ~env "-" e1 e2, env
    | Mul (e1, e2)       -> of_bin_op ~p ~verify ~env "*" e1 e2, env
    | MatrixMul (e1, e2) -> of_bin_op ~p ~verify ~env "@" e1 e2, env
    | Div (e1, e2)       -> of_bin_op ~p ~verify ~env "/" e1 e2, env
    | FloorDiv (e1, e2)  -> of_bin_op ~p ~verify ~env "//" e1 e2, env
    | Mod (e1, e2)       -> of_bin_op ~p ~verify ~env "%" e1 e2, env
    | Pow (e1, e2)       -> of_bin_op ~p ~verify ~env "**" e1 e2, env
    | LShift (e1, e2)    -> of_bin_op ~p ~verify ~env "<<" e1 e2, env
    | RShift (e1, e2)    -> of_bin_op ~p ~verify ~env "<<" e1 e2, env
    | BitOr (e1, e2)     -> of_bin_op ~p ~verify ~env "|" e1 e2, env
    | BitAnd (e1, e2)    -> of_bin_op ~p ~verify ~env "&" e1 e2, env
    | BitXor (e1, e2)    -> of_bin_op ~p ~verify ~env "^" e1 e2, env
    | And (e1, e2)       -> of_bin_op ~p ~verify ~env "and" e1 e2, env
    | Or (e1, e2)        -> of_bin_op ~p ~verify ~env "or" e1 e2, env
    | Eq (e1, e2)        -> of_bin_op ~p ~verify ~env "==" e1 e2, env
    | Neq (e1, e2)       -> of_bin_op ~p ~verify ~env "!=" e1 e2, env
    | Lt (e1, e2)        -> of_bin_op ~p ~verify ~env "<" e1 e2, env
    | LtE (e1, e2)       -> of_bin_op ~p ~verify ~env "<=" e1 e2, env
    | Gt (e1, e2)        -> of_bin_op ~p ~verify ~env ">" e1 e2, env
    | GtE (e1, e2)       -> of_bin_op ~p ~verify ~env ">=" e1 e2, env
    | Is (e1, e2)        -> of_bin_op ~p ~verify ~env "is" e1 e2, env
    | IsNot (e1, e2)     -> of_bin_op ~p ~verify ~env "is not" e1 e2, env
    | In (e1, e2)        -> of_bin_op ~p ~verify ~env "in" e1 e2, env
    | NotIn (e1, e2)     -> of_bin_op ~p ~verify ~env "not in" e1 e2, env
  in
  (if p > parent_p then par_pat pp else pp), env