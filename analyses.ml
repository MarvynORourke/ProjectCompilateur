(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

let rec stmt_returns = function
	(Skip)->false
	|(Assign(_,_,_))->false
	|(Seq(stmt1,stmt2))->(stmt_returns stmt1) || (stmt_returns stmt2)
	|(Cond(_,stmt1,stmt2))->(stmt_returns stmt1) || (stmt_returns stmt2)
	|(While(_,stmt))->(stmt_returns stmt)
	|(CallC(_,_))->false
	|(Return(_))->true;;



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)



let rec stack_depth_e = function
	(Const(_,_))->1
	|(VarE(_,_))->1
	|(BinOp(_,_,expr1,expr2))->1+(stack_depth_e expr1) +(stack_depth_e expr2)
	|(IfThenElse(_,expr1,expr2,expr3))->5+(stack_depth_e expr1)+(stack_depth_e expr2) +(stack_depth_e expr3) 
	|(CallE(_,_,list_expr))-> let rec aux = function
							  (expr::c)-> (stack_depth_e expr) + (aux c)
							  |_->0 in aux(list_expr)+1;;
let rec stack_depth_c = function
	(Skip)->1
	|(Assign(_,_,expr))->1 + (stack_depth_e expr)
	|(Seq(stmt,stmt2))->(stack_depth_c stmt) + (stack_depth_c stmt2)
	|(Cond(expr,stmt,stmt2))->5+(stack_depth_e expr) + (stack_depth_c stmt) + (stack_depth_c stmt2)
	|(While(expr,stmt))->5+(stack_depth_e expr) + (stack_depth_c stmt)
	|(CallC(_,list_expr))->let rec aux = function
							  (expr::c)-> (stack_depth_e expr) + (aux c)
							  |_->0 in aux(list_expr)+1
	|(Return(expr))->1 +(stack_depth_e expr);;

(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet = 
  Set.Make
    (struct type t = string 
	    let compare = Pervasives.compare 
     end)

let rec defassign_e vs = function
	(Const(_,_))->true
	|(VarE(_,Var(binding,nom)))->(StringSet.mem nom vs)
	|(BinOp(_,_,expr1,expr2))->(defassign_e vs expr1) && (defassign_e vs expr2)
	|(IfThenElse(_,expr1,expr2,expr3))->(defassign_e vs expr1) && (defassign_e vs expr2) && (defassign_e vs expr3)
	|(CallE(_,_,list_expr))-> let rec aux = function
							  (expr::c)-> (defassign_e vs expr) && (aux c)
							  |_->true in aux(list_expr);;

exception NoVariable;;
							  
let rec defassign_c vs = function
	(Skip)->vs
	|(Assign(_,Var(_,nom),expr))->if defassign_e vs expr then (StringSet.add nom vs) else raise NoVariable
	|(Seq(stmt,stmt2))->(StringSet.union (defassign_c vs stmt) (defassign_c vs stmt2))
	|(Cond(expr,stmt,stmt2))->if defassign_e vs expr then(StringSet.union (defassign_c vs stmt) (defassign_c vs stmt2)) else raise NoVariable
	|(While(expr,stmt))-> if defassign_e vs expr then defassign_c vs stmt else raise NoVariable
	|(CallC(_,list_expr))->let rec aux = function
							  (expr::c)-> defassign_e vs expr && (aux c)
							  |_->true in if aux list_expr then vs else raise NoVariable
	|(Return(expr))->if defassign_e vs expr then vs else raise NoVariable;;
