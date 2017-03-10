.class MyClass
.super java/lang/Object


.method static even(I)I
  .limit stack 3
  .limit locals 1
     sipush 5
     iload 0
     iadd
     sipush 5
     iadd
     ireturn
.end method

