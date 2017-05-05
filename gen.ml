(* Compilation functions *)
open Lang
open Analyses
open Instrs
open Typing

exception VarInexistante;;					  

let b2i = function
| BoolT -> IntT
| t -> t
;;
					  
(*Fonction qui permet de donner la position de l'élément e dans la liste*)
let rec position e = function
	(a::c)-> if a = e then 0 else 1 + position e c
	|_->raise VarInexistante;; 
	
(*Fonction qui prend une liste d'expression et qui retourne la liste inversé *)
let rec inverse = function
	(a::c)->(inverse c)@[a]
	|_->[];;

(*Fonction qui donne retourne une lsite des types qu'il y a dans une liste *)
let rec types_inlist = function 
	(a::c)->(tp_of_expr a)::(types_inlist c)
	|_->[];;

(*Fonction qui génère le type qui prépare du bytecode*) 
let rec gen_expr liste_var etiquette = function
	(Const(tp,c))->[Loadc(tp,c)]
	| (Const (BoolT, BoolV b)) -> let i = if b then 1 else 0 in [Loadc (IntT, IntV i)]
	| (Const _) -> failwith "Erreur"

	|(VarE(tp,Var(_,nom)))->[Loadv(tp,(position nom liste_var))]
	| (BinOp (tp, binop, exp1, exp2)) -> 
		let pa = (gen_expr liste_var)
		in let l1 = pa etiquette exp1 
		and l2 = pa etiquette exp2 
		in begin (match binop with 
		| BArith _ | BLogic _ -> l1 @ l2 @ [Bininst (IntT, binop)]
		| BCompar cmp_operator ->
			let lbl_true = etiquette @ [0] 
			and lbl_fin = etiquette @ [1]
			in l1 @ l2 @ If (cmp_operator, lbl_true) 
				:: Loadc (IntT, IntV 0) 
				:: Goto lbl_fin 
				:: Label lbl_true 
				:: Loadc (IntT, IntV 1) 
				:: [Label lbl_fin]
		) end	
	|(IfThenElse(tp,expr1,expr2,expr3))->(gen_expr liste_var (1::etiquette) expr1)@[Loadc(IntT,IntV 0);If(BCeq,(2::etiquette))]@(gen_expr liste_var (2::etiquette) expr2)@[Goto(3::etiquette);Label(2::etiquette)]@(gen_expr liste_var (3::etiquette) expr3)@[Label(3::etiquette)]
	|(CallE(tp,name,liste_expr))->let list_expr_inverse = inverse liste_expr in 
																			let rec aux = function 
																				(a::c)->(gen_expr liste_var etiquette a)@aux(c)
																				|_->[] in aux list_expr_inverse@[Invoke(tp,name,types_inlist list_expr_inverse)];;

																				
let rec gen_stmt liste_var etiquette = function
	(Skip)->[Nop]
	|(Assign(tp,var,expr))->(gen_expr liste_var etiquette expr) @[Storev(tp,(List.length liste_var)+1)]
	|(Seq(stmt,stmt1))->(gen_stmt liste_var (1::etiquette) stmt)@(gen_stmt liste_var (2::etiquette) stmt1)
	|(Cond(expr,stmt,stmt2))->(gen_expr liste_var (1::etiquette) expr)@[Loadc(IntT,IntV 0);If(BCeq,(2::etiquette))]@(gen_stmt liste_var (2::etiquette) stmt)@[Goto(3::etiquette);Label(2::etiquette)]@(gen_stmt liste_var (3::etiquette) stmt)@[Label(3::etiquette)]
	|(While(expr,stmt))->[Label(1::etiquette)]@(gen_expr liste_var (1::etiquette) expr)@[Loadc(IntT,IntV 0);If(BCeq,(2::etiquette))]@(gen_stmt liste_var (2::etiquette) stmt)@[Goto(1::etiquette);Label(2::etiquette)]
	|(CallC(name,liste_expr))->let list_expr_inverse = inverse liste_expr in
																			let rec aux = function 
																				(a::c)->(gen_expr liste_var etiquette a)@aux(c)
																				|_->[] in aux list_expr_inverse@[Invoke(VoidT,name,types_inlist list_expr_inverse)]
	| Return rExpr -> [ReturnI (b2i (Lang.tp_of_expr rExpr))];;

let rec type_list = function
	((Vardecl(tp,_))::c)->tp::(type_list c)
	|_->[];;
	
let rec name_list = function
	((Vardecl(_,name))::c)->name::(name_list c)
	|_->[];;
	
let gen_fundefn = function 
	(Fundefn(Fundecl(tp,name,parameters),local_var,stmt))->Methdefn(Methdecl(b2i tp,name,type_list parameters),Methinfo((List.length local_var),(stack_depth_c stmt) +1),(gen_stmt ((name_list parameters)@(name_list local_var)) [0] stmt));;
	
let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([],List.map gen_fundefn fdfs);;