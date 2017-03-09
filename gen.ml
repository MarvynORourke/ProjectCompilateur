(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])

(*Fonction qui permet de donner la position de l'élément e dans la liste*)
let rec position e = function
	(a::c)-> if a = e then position e [] else 1 + position e c
	|_->0;; 

(*Fonction qui génère le type qui prépare du bytecode*) 
let rec gen_expr env = function
	(Const(tp,c))->[LoadC(tp,c)]
	|(VarE(tp,Var(_,nom)))->[LoadV(tp,position((nom,tp),env.localvar))]
	|(BinOp(tp,bin,expr1,expr2))->(gen_expr env expr1)@(gen_expr env expr2)@[Binist(tp,bin)];;