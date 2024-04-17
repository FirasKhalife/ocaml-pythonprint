
open Ast
open Print
open Utils

(** based on Python reference - operator precedence
    https://docs.python.org/3/reference/expressions.html#operator-precedence *)
let p_of_expr = function
  | Id _ | Attribute _ | Starred _ -> 0
  | String _ | Img _ | Int _ | Float _ | Bool _ | PyNone -> 0
  | App _ | PyTuple _ | PyList _ | PySet _ | Dict _ -> 0
  | Subscription _ | Slicing _ | ListComp _ | SetComp _ | DictComp _ | GenExpr _ -> 5
  | Await _ -> 10
  | Pow _ -> 15
  | Pos _ | Neg _ | BitNot _ -> 20
  | Mul _ | MatrixMul _ | Div _ | FloorDiv _ | Mod _ -> 25
  | Add _ | Sub _ -> 30
  | LShift _ | RShift _ -> 35
  | BitAnd _ -> 40
  | BitXor _ -> 45
  | BitOr _ -> 50
  | In _ | NotIn _ | Is _ | IsNot _
    | Lt _ | LtE _ | Gt _ | GtE _ | Neq _ | Eq _ -> 55
  | Not _ -> 60
  | And _ -> 65
  | Or _ -> 70
  | IfExpr _ -> 75
  | Lam _ -> 80
  | AssignExpr _ -> 85
  | _ -> 100

let get_global_env stmts = 
  let rec fold_rec env = function
  | [] -> env
  | stmt :: stmts ->
      let ids =
        match stmt with
        | Import names -> List.map (fun name -> List.hd $ List.rev name) names 
        | FromImport (_, names) -> List.map (fun (name, _) -> List.hd $ List.rev name) names
        | AssignStmt (ids,_) -> List.map fst ids 
        | FunDef (id,_,_,_) | ClassDef (id,_,_,_) -> [id]
        | _ -> []
      in
      fold_rec (push_id_list ids env) stmts
  in
  fold_rec empty_env stmts

let is_literal = function
  | String _ | Int _ | Float _ | Img _ -> true
  | _ -> false

let is_atom = function
  | Id _ -> true
  | exp when is_literal exp -> true
  | PyList _ | PySet _ | Dict _ | Yield _ -> true
  | ListComp _ | SetComp _ | DictComp _ | GenExpr _ -> true
  | _ -> false

let is_primary = function
  | Attribute _ | Subscription _ | Slicing _ | App _ -> true
  | exp when is_atom exp -> true 
  | _ -> false

let is_op = function
  | Pos _ | Neg _ | Not _ | BitNot _ -> true
  | Add _ | Sub _ | Mul _ | MatrixMul _ | Div _ | FloorDiv _ | Mod _ -> true
  | Pow _ | LShift _ | RShift _ -> true
  | BitAnd _ | BitXor _ | BitOr _ -> true
  | And _ | Or _ -> true
  | Eq _ | Neq _ | Lt _ | LtE _ | Gt _ | GtE _ -> true
  | Is _ | IsNot _ | In _ | NotIn _ -> true
  | exp when is_primary exp -> true
  | _ -> false

let is_expr = function
  | Lam _ -> true
  | IfExpr _-> true
  | _ -> false

let rec is_target = function
  | Id _ | Attribute _ | Subscription _ | Slicing _ | Starred _ -> true
  | PyList exprs -> List.for_all is_target exprs
  | PyTuple exprs -> List.for_all is_target exprs
  | _ -> false

let is_target_list = List.for_all is_target

let rec of_assigned_expr ?(in_fun = false) ?(env = empty_env) = function
  | AssignedExpr e -> fst $ of_expr ~env e
  | AssignedYield y -> 
      if !verif && not in_fun then raise (Invalid_argument "Yield statement outside of function")
      else of_yield ~env y

and of_yield ?(in_fun = false) ?(env = empty_env) exp =
  if !verif && not in_fun then raise (Invalid_argument "Yield statement outside of function")
  else
    match exp with
    | YieldRaw exps -> 
        yield_raw (List.map (fun exp -> fst $ of_expr ?p:None ~env exp) exps)
    | YieldFrom exp -> 
        yield_from (fst $ of_expr ~env exp)

and of_class_block ?(env = empty_env) stmts =
  let env = StringSet.union (StringSet.singleton "self") env in
  let rec fold_rec acc = function
  | [] -> acc
  | [stmt] -> Format.dprintf "%t%t" acc (of_class_stmt ~next:None ~env stmt)
  | stmt :: next :: stmts ->
      fold_rec (Format.dprintf "%t%t" acc (of_class_stmt ~next:(Some next) ~env stmt)) (next::stmts)
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

and of_class_stmt ?(next = None) ?(env = empty_env) stmt =
  let stmt, sep =
    match stmt with
    | ClassPass -> pass, Some mt
    | ClassDecorator id -> decorator id, Some fnl
    | ClassVarDef (ids, expr) -> var_def ids (fst $ of_expr ~env expr), sep_inclass next
    | InstanceVarDef (id, t) -> typed_id (id, Some t), sep_inclass next
    | MethodDef (id, params, args, body) -> 
        fun_def id params args (of_block ~in_fun:true ~env body), Some fnl2
  in
  let stmt_str = Format.asprintf "%t" stmt in
  if ismt stmt_str then stmt
  else (if Option.is_some next then Format.dprintf "%t%t" stmt (Option.get sep) else stmt)

and of_block ?(in_fun = false) ?(env = empty_env) stmts =
  let env' = StringSet.union (get_global_env stmts) env in
  let rec fold_rec acc env = function
  | [] -> acc
  | [stmt] -> 
      let stmt, _ = of_stmt ~env ~in_fun ~next:None stmt in
      Format.dprintf "%t%t" acc stmt
  | stmt :: next :: stmts ->
      let env = match stmt with FunDef _ -> env' | _ -> env in
      let stmt, env = of_stmt ~env ~in_fun ~next:(Some next) stmt in
      fold_rec (Format.dprintf "%t%t" acc stmt) env (next::stmts)
  in
  fold_rec mt env stmts

and module_stmt_sep = function
  | None -> mt
  | Some next -> 
      match next with
      | Import _ | FromImport _ | FromImportAll _ -> fnl
      | _ -> fnl2

and of_stmt ?(in_fun = false) ?(next = None) ?(env = empty_env) stmt =
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
    | Expression exp -> 
        begin 
          match exp with 
          | Id _ -> raise (Invalid_argument "Statement cannot be a simple identifier")
          | _ -> fst $ of_expr ~env exp, fnl, env
        end
    | Decorator id -> if !verif then verify_id id env; decorator id, fnl, env
    | Return e ->
        if !verif && not in_fun then raise (Invalid_argument "Return statement outside of function")
        else return $ fst (of_expr ~env e), fnl, env
    | TypedId (s, t) -> typed_id (s, t), fnl, push_id s env
    | AssignStmt (ids, expr) -> 
        var_def ids (of_assigned_expr ~in_fun ~env expr), fnl, 
        push_id_list (List.map fst ids) env
    | TypeAlias (t1, t2) -> type_alias t1 t2, fnl, env
    | TypeDef (id, s) -> type_def id s, fnl, env
    | Raise (exn, msg, from) -> 
        let from = 
          match from with 
          | None -> "" 
          | Some id -> if !verif then verify_id id env; id 
        in
        py_raise exn msg from, fnl, env
    | FunDef (id, params, args, body) -> 
        let arg_ids = List.map (fun (id, _) -> id) args in
        let env' = push_id_list (id :: arg_ids) env in
        fun_def id params args (of_block ~env:env' ~in_fun:true body), fnl2,
          push_id id env
    | ClassDef (id, type_params, parents, body) ->
        class_def id type_params parents (of_class_block ~env body), fnl2,
          push_id id env
    | Match (head, cases) ->
        let branches = 
          List.map (fun (pat, stmts) -> 
            let pat, env' = 
              match pat with
              | App (id, args) -> 
                  let id, _ = of_expr ~env id in
                  let args, env' = fold_of_expr true env args in
                  app id args, env'
              | _ -> of_expr ~assign:true ~env pat
            in
            pat, of_block ~env:env' ~in_fun stmts) 
          cases
        in
        py_match (fst $ of_expr ~env head) branches, fnl, env
    | IfStmt (cond, then_, elifs, else_) ->
        let elifs = 
          List.map (fun (cond, block) -> 
            fst $ of_expr ~env cond, 
            of_block ~env ~in_fun block) 
          elifs
        in
        if_stmt (fst $ of_expr ~env cond) 
          (of_block ~env ~in_fun then_) 
          elifs (of_block ~env ~in_fun else_), fnl, env
    | While (cond, body) -> 
        while_stmt (fst $ of_expr ~env cond) (of_block ~env ~in_fun body), fnl, env
    | For (id, exp, body) ->
        let env' = push_id id env in
        for_stmt id (fst $ of_expr ~env exp) (of_block ~env:env' ~in_fun body), fnl, env
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
        with_stmt (fst $ of_expr ~env exp) id (of_block ~env:env' ~in_fun body), fnl, env
    | Print exps -> print $ concat_expr exps, fnl, env
    | Assert (exp, msg) -> 
        py_assert (fst $ of_expr ~env exp)
        (Option.map fst $ Option.map (of_expr ~env) msg), fnl, env
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

and concat_expr ?(p = 100) ?(env = empty_env) exps = 
  concat_map_comma_pp (fun exp -> fst $ of_expr ~p ~env exp) exps
and of_un_op ?(p = 100) ?(env = empty_env) ?(sep_spc = false) op_str e =
  let sep = if sep_spc then spc else mt in
  combine_sep sep [str op_str; fst $ of_expr ~p ~env e]
and of_bin_op ?(p = 100) ?(env = empty_env) op_str e1 e2 = 
  combine_sep spc [fst $ of_expr ~p ~env e1; str op_str; fst $ of_expr ~p ~env e2]

and fold_of_expr assign env args =
  let rec fold_rec env acc = function
  | [] -> List.rev acc, env
  | arg :: args ->
      let arg, env' = of_expr ~assign ~env arg in
      fold_rec env' (arg :: acc) args
  in
  fold_rec env [] args

and of_slice env p assign = function
  | ProperSlice (lower, upper, stride) ->
      if not (List.for_all (fun e -> is_expr $ Option.get e) 
          (List.filter Option.is_some [lower; upper; stride]))
        then raise (Invalid_argument "Proper slice arguments should be expressions")
        else
          let lower = Option.map (fun e -> fst $ of_expr ~env ~p ~assign e) lower in
          let upper = Option.map (fun e -> fst $ of_expr ~env ~p ~assign e) upper in
          let stride = Option.map (fun e -> fst $ of_expr ~env ~p ~assign e) stride in
          proper_slice lower upper stride
  | SliceExpr exp -> fst $ of_expr ~env exp

and of_comp_iter env = function
  | CompFor (idx, iterable) ->
      let idx, env' = of_expr ~env idx in
      let iterable, _ = of_expr ~env iterable in
      comp_for idx iterable, env'
  | CompIf expr -> 
      let expr, _ = of_expr ~env expr in
      comp_if expr, env 

and of_comp env (expr, is_async, comp_iter_l) =
  match comp_iter_l with
  | [] -> raise (Invalid_argument "Empty comprehension")
  | CompIf _ :: _ -> raise (Invalid_argument "First comprehension should be a CompFor")
  | _ ->
      let clauses, env =
        List.fold_left 
          (fun (acc, env) iter -> let comp, env = of_comp_iter env iter in comp :: acc, env) 
          ([], env) comp_iter_l
      in
      let expr, _ = of_expr ~env expr in
      comprehension expr is_async (List.rev clauses)

(** [p] stands for the current precedence level
    [assign] specifies whether identifiers should be added to the environment *)
and of_expr ?(p = 100) ?(assign = false) ?(env = empty_env) expr =
  let parent_p = p in
  let p = p_of_expr expr in
  let pp, env =
    match expr with
    | Id id -> 
        if assign then py_id id, push_id id env
        else (if !verif then verify_id id env; py_id id, env)
    | AssignExpr (id, expr) ->
        let env = push_id id env in 
        let expr, env = of_expr ~env expr in
        assign_expr id expr, env
    | Starred e -> py_star (fst $ of_expr ~p ~env e), env
    | Await e -> await (fst $ of_expr ~p ~env e), env
    | Attribute (obj, ids) -> 
        attribute (fst $ of_expr ~p ~env obj) ids, env
    | App (f, args) -> 
        let f, _ = of_expr ~p ~env f in
        let args, env' = fold_of_expr assign env args in
        if assign then app f args, env' else app f args, env
    | Lam (ids, body) ->
        let env' = push_id_list ids env in
        let nl = match body with Lam _ -> true | _ -> false in
        lambda_def ~nl ids (fst (of_expr ~p ~env:env' body)), env
    (* TODO: it was considered as a stmt, 
       has to be considered as an expression and detect whether it is in a function or not *)
    | Yield yield -> of_yield ~in_fun: false ~env yield, env
    | IfExpr (cond, then_, else_) ->
        pp_ifexpr 
          (fst $ of_expr ~p ~env cond) 
          (fst $ of_expr ~p ~env then_) 
          (fst $ of_expr ~p ~env else_), env
    | PyTuple args ->
        let args, env' = fold_of_expr assign env args in
        par_pat $ concat_comma_pp args, env'
    | PyList args ->
        let args, env' = fold_of_expr assign env args in
        brak_pat $ concat_comma_pp args, env'
    | PySet args ->
        let args, env' = fold_of_expr assign env args in
        curly_pat $ concat_comma_pp args, env'
    | Dict args ->
        curly_pat $ concat_map_comma_pp (fun (k, v) -> 
          dict_item (fst $ of_expr ~env k) (fst $ of_expr ~env v)) args, env
    | Subscription _ -> failwith "Subscript not supported"
    | Slicing (e, slices) -> 
        if not (is_primary e) then raise (Invalid_argument "Slicing first arg should be a primary")
        else
          let e, _ = of_expr ~p ~env e in
          slicing e (List.map (of_slice env p assign) slices), env
    | ListComp e -> of_comp env e, env 
    | SetComp e -> of_comp env e, env 
    | DictComp e -> of_comp env e, env 
    | GenExpr e -> of_comp env e, env 
    | String s -> if assign then py_str s, push_id s env else py_str s, env
    | Img i -> py_img i, env
    | Int i -> py_int i, env
    | Float f -> py_float f, env
    | Bool b -> py_bool b, env
    | PyNone -> py_none, env
    | Pos e              -> of_un_op ~p ~env "+" e, env
    | Neg e              -> of_un_op ~p ~env "-" e, env
    | Not e              -> of_un_op ~p ~env "not" e ~sep_spc:true, env
    | BitNot e           -> of_un_op ~p ~env "~" e, env
    | Add (e1, e2)       -> of_bin_op ~p ~env "+" e1 e2, env
    | Sub (e1, e2)       -> of_bin_op ~p ~env "-" e1 e2, env
    | Mul (e1, e2)       -> of_bin_op ~p ~env "*" e1 e2, env
    | MatrixMul (e1, e2) -> of_bin_op ~p ~env "@" e1 e2, env
    | Div (e1, e2)       -> of_bin_op ~p ~env "/" e1 e2, env
    | FloorDiv (e1, e2)  -> of_bin_op ~p ~env "//" e1 e2, env
    | Mod (e1, e2)       -> of_bin_op ~p ~env "%" e1 e2, env
    | Pow (e1, e2)       -> of_bin_op ~p ~env "**" e1 e2, env
    | LShift (e1, e2)    -> of_bin_op ~p ~env "<<" e1 e2, env
    | RShift (e1, e2)    -> of_bin_op ~p ~env "<<" e1 e2, env
    | BitOr (e1, e2)     -> of_bin_op ~p ~env "|" e1 e2, env
    | BitAnd (e1, e2)    -> of_bin_op ~p ~env "&" e1 e2, env
    | BitXor (e1, e2)    -> of_bin_op ~p ~env "^" e1 e2, env
    | And (e1, e2)       -> of_bin_op ~p ~env "and" e1 e2, env
    | Or (e1, e2)        -> of_bin_op ~p ~env "or" e1 e2, env
    | Eq (e1, e2)        -> of_bin_op ~p ~env "==" e1 e2, env
    | Neq (e1, e2)       -> of_bin_op ~p ~env "!=" e1 e2, env
    | Lt (e1, e2)        -> of_bin_op ~p ~env "<" e1 e2, env
    | LtE (e1, e2)       -> of_bin_op ~p ~env "<=" e1 e2, env
    | Gt (e1, e2)        -> of_bin_op ~p ~env ">" e1 e2, env
    | GtE (e1, e2)       -> of_bin_op ~p ~env ">=" e1 e2, env
    | Is (e1, e2)        -> of_bin_op ~p ~env "is" e1 e2, env
    | IsNot (e1, e2)     -> of_bin_op ~p ~env "is not" e1 e2, env
    | In (e1, e2)        -> of_bin_op ~p ~env "in" e1 e2, env
    | NotIn (e1, e2)     -> of_bin_op ~p ~env "not in" e1 e2, env
  in
  (if p > parent_p then par_pat pp else pp), env