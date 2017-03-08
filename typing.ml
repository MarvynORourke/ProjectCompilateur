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
exception VariableInconnu;;

exception TypageImpossible;;

exception FonctionInconnu;;

(*Fonction qui regarde si la variable existe dans l'environnement*)
let rec exist_variable var = function
	((a,c)::l)-> if var = a then c
				 else exist_variable var l
	|_->raise VariableInconnu;;



(*Fonction qui regarde si la fonction est dans l'environnement*)					
let rec get_fundecl nom = function
	((Fundecl(tp,n,l))::c)-> if nom = n then (tp,l)
				else get_fundecl nom c
	|_-> raise FonctionInconnu;;

let rec tp_expr env = function 
(* On regarde le type de la constante *)
	(Const (_,i)) -> (match i with
						(BoolV b) -> (Const(BoolT, BoolV b))
						|(IntV i)-> (Const(IntT, IntV i))
						|(VoidV)-> (Const(VoidT,VoidV)))
	(*On regarde si la variable est dans l'environnement, si elle l'est, ont lui donne son type sinon on lève un exception*)
	|(VarE(_,Var(_,nom))) -> (try VarE((exist_variable nom env.localvar),Var(Local,nom)) with VariableInconnu -> raise TypageImpossible)
	(*On regarde le type d'une opération, on vérifie si les deux expressions sont bien du même type et ensuite on regarde si elle correspond au genre d'opération effectué *)
	|(BinOp(_,bin,expr1,expr2)) ->let a = (tp_expr env expr1) and b = (tp_expr env expr2) in (if (tp_of_expr a) = (tp_of_expr b) then (match bin with
																																	(BArith _)-> if (tp_of_expr a) = IntT then BinOp(IntT,bin,a,b)
																																				else raise TypageImpossible
																																	|_-> BinOp(BoolT,bin,a,b))
																							else raise TypageImpossible)
	(*On s'occupe de l'expression IfThenElse, si la première expression n'est pas un expression bool on retourne une exception et si les deux valeurs retourné ne sont pas identique *)
	|(IfThenElse(_,expr1,expr2,expr3))-> let a = tp_expr env expr1 and b = tp_expr env expr2 and c = tp_expr env expr3 in (if tp_of_expr b != tp_of_expr c || tp_of_expr a != BoolT then raise TypageImpossible
																														  else IfThenElse(tp_of_expr b,a,b,c))
	(*Pour finir CallE qui est un appel de fonction, il vérfie si le typage des variables correspond et si la fonction est dans l'environnement*)
	|(CallE(_,name,l1)) ->try (let (type_fun,liste_env) = (get_fundecl name env.funbind) in 
																						let rec aux = function
																							((a::c),(Vardecl(t,n)::d))->let tp = tp_expr env a in (if tp_of_expr tp = t then tp :: aux(c,d)
																																										else raise TypageImpossible)
																							|([],[])->[] 
																							|_-> raise TypageImpossible
																								in CallE(type_fun,name,aux(l1,liste_env))) with _ -> raise TypageImpossible;;