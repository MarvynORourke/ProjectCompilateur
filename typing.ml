(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;
exception VariableInconnu;

exception TypageImpossible;

let rec exist_variable var = function
	((a::c),l)-> if var = a then c
				 else exist_variable l
	|_->raise VariableInconnu;

let rec tp_expr env = function 
(* Onr egarde le type de la constante *)
	(Const (_,i)) -> (match i with 
						(BoolV b -> Const(BoolV, b)
						IntV i -> Const(IntV, i)
						VoidV -> VoidV))
	(*Onr egarde si la variable est dans l'environnement, si elle l'est, ont lui donne son type sinon on lève un exception*)
	|(VarE (_,nom)) -> try (VarE (tp_expr nom env.localvar),Var(Local,nom)) with (* Local en attendant de savoir sic 'est global ou non *)
												VariableInconnu -> raise TypageImpossible
	(*On regarde le type d'une opération, on vérifie si les deux expressions sont bien du même type et ensuite on regarde si elle corresponde au genre d'opération effectué *)
	|(BinOp(_,bin,expr1,expr2))->let a = tp_expr env expr1 and b = tp_expr env expr2 in (if type_of_expr a =  type_of_expr b then match bin with(
																							BArith _ -> if type_of_expr a = IntV then BinOp(IntV,bin,a,b) else raise TypageImpossible
																							_ -> BinOp(BoolT,bin,a,b ))
																						else raise TypageImpossible)
	(*On s'occupe de l'expression IfThenElse, si la première expression n'est pas un expression bool on retourne une exception et si les deux valeurs retourné ne sont pas identique *)
	|(IfThenElse(_,expr1,expr2,expr3))-> let a = tp_expr env expr1 and b = tp_expr env expr2 and c = tp_expr env expr3 in ( if type_of_expr b != type_of_expr c or type_of_expr a != BoolT then raise TypageImpossible
																															else IfThenElse(type_of_expr b,a,b,c));
								