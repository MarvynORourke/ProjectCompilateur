(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)
  
type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}

let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t
  | CallE (t, _, _) -> t;;
  
 let tp_of_vardecl (Vardecl (t, _)) = t;;
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
	(((a,c)::l),(b,d)::l2)-> if var = a  then c
				 else if var = b then d else exist_variable var (l,l2)
	|([],(b,d)::l2)-> if var = b then d else exist_variable var ([],l2)
	|((a,c)::l,[])-> if var = a  then c else exist_variable var (l,[])
	|_->raise VariableInconnu;;



(*Fonction qui regarde si la fonction est dans l'environnement*)					
let rec get_fundecl nom = function
	((Fundecl(tp,n,l))::c)-> if nom = n then (tp,l)
				else get_fundecl nom c
	|_-> raise FonctionInconnu;;

let rec tp_expr env = function 
(* On regarde le type de la constante *)
	(Const ((_:int),i)) -> (match i with
						(BoolV b) -> (Const(BoolT, BoolV b))
						|(IntV i)-> (Const(IntT, IntV i))
						|(VoidV)-> (Const(VoidT,VoidV)))
	(*On regarde si la variable est dans l'environnement, si elle l'est, ont lui donne son type sinon on lève un exception*)
	|(VarE(_,Var(_,nom))) -> (try VarE((exist_variable nom (env.localvar,env.globalvar)),Var(Local,nom)) with VariableInconnu ->  failwith nom)
	(*On regarde le type d'une opération, on vérifie si les deux expressions sont bien du même type et ensuite on regarde si elle correspond au genre d'opération effectué *)
	|(BinOp(_,bin,expr1,expr2)) ->let a = (tp_expr env expr1) and b = (tp_expr env expr2) in (if (tp_of_expr a) = (tp_of_expr b) then (match bin with
																																	(BArith _)-> if (tp_of_expr a) = IntT then BinOp(IntT,bin,a,b)
																																				else failwith "1"
																																	|(BLogic _)->if (tp_of_expr a) = BoolT then BinOp(BoolT,bin,a,b)
																																				else  failwith "2"
																																	|_->BinOp(BoolT,bin,a,b))
																							else failwith "4")
	(*On s'occupe de l'expression IfThenElse, si la première expression n'est pas un expression bool on retourne une exception et si les deux valeurs retourné ne sont pas identique *)
	|(IfThenElse(_,expr1,expr2,expr3))-> let a = tp_expr env expr1 and b = tp_expr env expr2 and c = tp_expr env expr3 in (if tp_of_expr b != tp_of_expr c || tp_of_expr a != BoolT then  failwith "3"
																														  else IfThenElse(tp_of_expr b,a,b,c))
	(*Pour finir CallE qui est un appel de fonction, il vérfie si le typage des variables correspond et si la fonction est dans l'environnement*)
	|(CallE(_,name,l1)) ->try (let (type_fun,liste_env) = (get_fundecl name env.funbind) in 
																						let rec aux = function
																							((a::c),(Vardecl(t,n)::d))->let tp = tp_expr env a in (if tp_of_expr tp = t then tp :: aux(c,d)
																																										else  failwith "5")
																							|([],[])->[] 
																							|_->failwith "6"																							in CallE(type_fun,name,aux(l1,liste_env))) with _ -> failwith "6";;
																							
(*Fonction qui type toutes les expressions d'une liste *)																																														
let rec tp_expr_list env = function
		(expr1::c)->(tp_expr env expr1)::(tp_expr_list env c)
		|_->[];;
		
(*Fonction qui type les stmt*)		
let rec tp_stmt env = function
	(Skip)->Skip
	|(Assign(_,var,expr))->(Assign(VoidT,var,tp_expr env expr))
	|(Seq(stmt1,stmt2))->(Seq(tp_stmt env stmt1,tp_stmt env stmt2))
	|(Cond(expr,stmt1,stmt2))->(Cond(tp_expr env expr, tp_stmt env stmt1,tp_stmt env stmt2))
	|(While(expr,stmt))->(While(tp_expr env expr, tp_stmt env stmt))
	|(CallC(fname,expr_list))->(CallC(fname,tp_expr_list env expr_list))
	|(Return(expr))->(Return(tp_expr env expr));;

(*Liste des mots inutilisables*)
let unusable_name = ["auto";"break";"case";"char";"const";"continue";"default";"do";"double";"else";"enum";"extern";"float";"for";"goto";"if";"int";"long";"register";"return";"short";"signed";"sizeof";"static";"struct";"switch";"typedef";"union";"unsigned";"void";"volatile";"while"];;

(*Fonction qui regarde si les éléments d'une liste 1 sont dans une liste 2 et si le type de la variable est pas de type VoidT*)
let rec elmt_list_in_list l1 = function
	(elmt::c)->if List.mem (name_of_vardecl elmt) l1 && (tp_of_vardecl elmt) = VoidT then false 
				else elmt_list_in_list l1 c
	|_->true;;
(*Conversion de VarDecl list en (tp*name) list*)
let rec vardecl_list_to_list = function
(vardecl::c)->(name_of_vardecl vardecl,tp_of_vardecl vardecl)::(vardecl_list_to_list c)
|_->[];; 

exception ErrorFundefn;;
	
let tp_fdefn env = function 
	Fundefn(Fundecl(tp,fname,var_liste),var_liste2,stmt)-> if elmt_list_in_list unusable_name var_liste2 && elmt_list_in_list unusable_name var_liste 
														   then let env2 = {localvar = vardecl_list_to_list var_liste2;globalvar =vardecl_list_to_list var_liste @ env.globalvar; returntp = tp; funbind = Fundecl(tp,fname,var_liste)::env.funbind } 
																in Fundefn(Fundecl(tp,fname,var_liste),var_liste2,tp_stmt env2 stmt)
														   else raise ErrorFundefn;;

			
let tp_prog = function 
	Prog(vardecl_list,fundef_list)-> let env = {localvar =[]; globalvar = vardecl_list_to_list vardecl_list; returntp = VoidT; funbind = [] } in Prog(vardecl_list,List.map (tp_fdefn env) fundef_list);;
