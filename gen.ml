(* Compilation functions *)

open Lang
open Analyses
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)



exception VarInexistante;;					  
					  
(*Fonction qui permet de donner la position de l'élément e dans la liste*)
let rec position e = function
	(a::c)-> if a = e then 0 else 1 + position e c
	|_->raise VarInexistante;; 

(*Fonction qui génère le type qui prépare du bytecode*) 
let rec gen_expr liste_var = function
	(Const(tp,c))->[Loadc(tp,c)]
	|(VarE(tp,Var(_,nom)))->[Loadv(tp,(position nom liste_var))]
	|(BinOp(tp,bin,expr1,expr2))->(gen_expr liste_var expr1)@(gen_expr liste_var expr2)@[Bininst(tp,bin)];;
	

let env = {localvar = [("n",IntT)]; globalvar = []; returntp = VoidT; funbind = []};;

let expr = BinOp(0,BArith BAadd,(BinOp(0,BArith BAadd,(Const(0,IntV 5)),(VarE(0,Var(Local,"n"))))),(Const(0,IntV 5)));;
	
let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      ((gen_expr (["n"]) (tp_expr env expr))@[ReturnI IntT]))]);;