#load "lang.cmo";;
#load "typing.cmo";;
#load "gen.cmo";;
#load "analyses.cmo";;
open Lang
open Analyses
open Instrs
open Typing
open Gen;;

  
print_string("coucou");;
 (*Test unitaire*)
	(*Const*)
if ((gen_expr ["x";"y";"z"] [0] (Const(IntT,IntV(5)))) = ([Loadc(IntT,IntV(5))])) then print_string("ok")else print_string("nope");;
	(*Var*)
if (gen_expr ["x";"y";"z"] [0] (VarE(IntT,Var(Local,"x"))) = ([Loadv(IntT,0)])) then print_string("ok")else print_string("nope");;
	(*BinOp , false*)
if (gen_expr ["x";"y";"z"] [0] (BinOp(BoolT,BCompar BCeq,Const(IntT,IntV(5)),Const(IntT,IntV(5))))) = ([Loadc(IntT,IntV(5))]) then print_string("ok") else print_string("nope");;
	(*BinOp,true*)
if (gen_expr ["x";"y";"z"] [0] (BinOp(BoolT,BCompar BCeq,Const(IntT,IntV(5)),Const(IntT,IntV(5))))) = ([Loadc(IntT,IntV(5));Loadc(IntT,IntV(5));Bininst(BoolT,BCompar BCeq)]) then print_string("ok") else print_string("nope");;
	(*IfThenElse*)
if (gen_expr ["x";"y";"z"] [0] (IfThenElse(IntT,Const(IntT,IntV(5)),Const(IntT,IntV(0)),Const(IntT,IntV(1))))) = ([Loadc(IntT,IntV(5));Loadc(IntT,IntV 0);If(BCeq,[2;0]);Loadc(IntT,IntV(0));Goto([3;0]);Label([2;0]);Loadc(IntT,IntV(1));Label([3;0])]) then print_string("ok")else print_string("nope");;
	(*CallE*)
if gen_expr ["x";"y";"z"] [0] (CallE(IntT,"f",[Const(IntT,IntV(5))])) = ([Loadc (IntT, IntV 5); Invoke (IntT, "f", [IntT])]) then print_string("ok")else print_string("nope");;


(*Tests Analyses*)

stack_depth_e(BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,VarE(IntT,Var(Local,"n")),Const(IntT,IntV(1))),Const(IntT,IntV(2))),Const(IntT,IntV(3))),Const(IntT,IntV(3))));;

stack_depth_e(BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,BinOp(IntT,BArith BAadd,Const(IntT,IntV(3)),Const(IntT,IntV(1))),Const(IntT,IntV(2))),Const(IntT,IntV(3))),VarE(IntT,Var(Local,"n"))));;
