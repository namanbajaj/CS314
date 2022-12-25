open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

let rec find env value = match env with	
  | (a, b) :: (t) -> if a = value then b else (find t value) 
  | [] -> raise UndefinedVar

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  | Number n -> (Int_Val n)  

  | True -> Bool_Val true
 
  | False -> Bool_Val false

  | Var n -> find env n

  | Plus (e1, e2) -> 
    ( 
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Int_Val (one + two)
        | _ -> raise TypeError
    )

  | Minus (e1, e2) ->
    ( 
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Int_Val (one - two)
        | _ -> raise TypeError
    )

  | Times (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Int_Val (one * two)
        | _ -> raise TypeError
    )

  | Div (e1, e2) ->
    ( 
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> if(two <> 0) then (Int_Val (one / two)) else raise DivByZeroError
        | _ -> raise TypeError
    )

  | Mod (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> if(two <> 0) then (Int_Val (one mod two)) else raise DivByZeroError
        | _ -> raise TypeError
    )

  | Or (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Bool_Val one, Bool_Val two -> Bool_Val (one || two)
        | _ -> raise TypeError
    )

  | And (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Bool_Val one, Bool_Val two -> Bool_Val (one && two)
        | _ -> raise TypeError
    )
  
  | Not (e1) ->
    (
      match (eval_expr e1 env) with
        | Bool_Val one -> Bool_Val (not one)
        | _ -> raise TypeError
    )

  | Lt (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Bool_Val (one < two)
        | _ -> raise TypeError
    )

  | Leq (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Bool_Val (one <= two)
        | _ -> raise TypeError
    )

  | Eq (e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val one, Int_Val two -> Bool_Val (one = two)
        | Bool_Val one, Bool_Val two -> Bool_Val (one = two)
        | _ -> raise TypeError
    )

  | Fun (e1, e2) -> Closure (env, e1, e2)

  | App (e1, e2) -> 
  (
      match eval_expr e1 env with
        | Closure (first, a, last) -> eval_expr (last) ([ (a, eval_expr e2 env) ] @ first)
        | _ -> raise TypeError
  )

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  
  | Skip -> env
  
  | Comp (c1, c2) ->  eval_command c2 (eval_command c1 env)
  
  | Declare (t, x) -> 
    (
      match t with
      | Int_Type -> (x, (Int_Val(0))) :: env
      | Bool_Type -> (x, (Bool_Val(false))) :: env
      | Lambda_Type -> (x, (Closure(env, x, Var(x) ) ) ) :: env
    )


  | Assg (id, e) -> 
    (
      match (eval_expr (Var id) env) with
        | Bool_Val x -> 
          (
            match (eval_expr e env) with
              | Int_Val y -> (id, (eval_expr e env)) :: env
              | _ -> raise TypeError
          )

        | Int_Val x -> 
          (
            match (eval_expr e env) with
              | Int_Val y -> (id, (eval_expr e env)) :: env
              | _ -> raise TypeError
          )
        
        | Closure (a,b,c) -> 
          (
            match (eval_expr e env) with
              | Closure (_,_,_) -> (id, (eval_expr e env)) :: env
              | _ -> raise TypeError
          )
    )
  
  | Cond (e, c1, c2) -> 
    (
      match (eval_expr e env) with
      | Bool_Val x -> if(x = true) then (eval_command c1 env) else (eval_command c2 env) 
      | _ -> raise TypeError
    )

  | While (e, c) -> 
    (
      match eval_expr e env with
      | Bool_Val false -> env
      | Bool_Val true -> eval_command (While (e, c)) (eval_command c env)
      | _ -> raise TypeError
    )

  | For (e, c) -> 
    (
      match eval_expr e env with
      | Int_Val x -> 
        (
          match eval_expr (Lt (Number 0, Number x)) [] with
          | Bool_Val true -> eval_command (For (Number (x - 1), c)) (eval_command c env)
          | _ -> env
        )
      | _ -> raise TypeError
    )

