PhD thesis code 
===

## SPAD language type checker

### Objectives

The project aims to deliver a new type checker for SPAD language. Several improvements over current type checker are planned:

 * introduce better type inference,
 * introduce modern language constructs,
 * produce understandable diagnostic messages (currently the compiler dumps AST as s-expression if it encounters an error),
 * eliminate well known bugs in the type system,
 * find new type errors in FriCAS library code.

Techniques employed will involve bottom-up tree walking, graph-like structures for type information management, aggressive type pruning.

### Deliverables

The project is going to deliver a partial implementation of type checker for Spad language. It'll be a set of source files with accompanying documentation. The code will have a single entry point - type checker function - which will accept parsed source file and infer types as needed. The result of the type checker will be a type annotated AST (with "$" and "@" operators) ready to be fed into existing Spad compiler, or an error message stating (as clearly as possible) why the type checking failed. The new type checker should:

 * work on existing Spad code in FriCAS library without massive source changes,
 * handle most commonly used program structures,
 * detect as many type errors as possible before passing the code to original compiler,
 * catch errors that go undetected with current type checker,
 * show that in some cases it doesn't require user delivered annotations, that are otherwise necessary,
 * make the compilation process faster.

### How to compile?

1. Download [FriCAS](http://fricas.sourceforge.net/download.html) sources and compile them.
2. Copy `src/interp/ncomp.boot` to cloned repository directory.
3. Run `patch < ncomp.boot.diff` shell command to patch *FriCAS* interpreter.
4. Run *FriCAS* in the directory.
5. Inside *FriCAS* issue command `)read initial.input`.

If all steps were successful you should be in a mode that let's you run some test the new type checker. To do so issue FriCAS command `)compile Test1.spad`.

### Output description

The following message in FriCAS output marks the start of execution of new type checker:

```
>>>> POST-PARSE HOOK BEGIN <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
```

Original abstract syntax tree dump - this is how the program looks after being parsed:

```
   (DEF  (Test1)  ((CATEGORY package (SIGNATURE foo ((Integer) (Integer))) (SIGNATURE foo ((Float) (String))) (SIGNATURE foo ((String) (Float) (Integer)))))
    (())

     (CAPSULE  (: bar (Mapping (Integer) (Integer) (Integer)))
      (DEF (foo a) (() (Integer)) (() ()) (SEQ (LET a2 (* a a)) (LET ma (- a)) (exit 1 (IF (>= a (Zero)) ma a2))))
      (: baz (Mapping (String) (NonNegativeInteger) (Integer))))
     )
```

Type checker AST representation dump:

```
   Test1() : {foo : Integer -> Integer,foo : String -> Float,foo : (Float,Integer) -> String} == ? add

       bar : (Integer,Integer) -> Integer
       foo(a : Integer) ==
         a2 := (a * a)
         ma := -(a)
         if (a >= Zero()) then ma else a2

       baz : (NonNegativeInteger,Integer) -> String
```

Converted back to compiler's AST:

```
   (DEF  (Test1)  ((CATEGORY package (SIGNATURE foo ((Integer) (Integer))) (SIGNATURE foo ((Float) (String))) (SIGNATURE foo ((String) (Float) (Integer)))))
    (())

     (CAPSULE  (: bar (Mapping (Integer) (Integer) (Integer)))  (DEF (foo a) (() (Integer)) (() ()) SEQ (LET a2 (* a a)) (LET ma (- a)) (IF (>= a (Zero)) ma a2))
      (: baz (Mapping (String) (NonNegativeInteger) (Integer))))
     )
```

Now construction of type checker blackboard begins. In this phase the environment is created and modified, as well as queried for specified symbols.

```
   [*] Processing functor : Test1 : () -> {foo : Integer -> Integer,foo : String -> Float,foo : (Float,Integer) -> String}
   EnvAddMode(Test1,() -> {foo : Integer -> Integer,foo : String -> Float,foo : (Float,Integer) -> String})
   EnvAddMode($,{foo : Integer -> Integer,foo : String -> Float,foo : (Float,Integer) -> String})
   EnvAddModemapsFromCategory($,{foo : Integer -> Integer,foo : String -> Float,foo : (Float,Integer) -> String})
   [*] Found sequence of 3 expressions.
   [*] Expression bar has type (Integer,Integer) -> Integer
   EnvAddMode(bar,(Integer,Integer) -> Integer)
   [*] Processing function "foo"
   Found "foo/1" with signatures : [(Float,Integer) -> String,String -> Float,Integer -> Integer]
   Signature associated with function definition : Integer -> ?
   Eligible signatures : [Integer -> Integer]
   [*] Assume foo/1 has type : Integer -> Integer
   EnvAddDomain(Integer)
   EnvAddDomain(Integer)
   EnvAddMode(a,Integer)
   [*] Found sequence of 3 expressions.
   [*] Processing fresh assignment "a2"
   EnvAddMode(a2,%5)
   [*] Symbol lookup for "a2"
   Found "a2" with type : [%5]
   [*] Processing function application.
   [*] Symbol lookup for "*"
   Found "*" with type : [(Integer,Integer) -> Integer,(NonNegativeInteger,Integer) -> Integer,(PositiveInteger,Integer) -> Integer]
   [*] Symbol lookup for "a"
   Found "a" with type : [Integer]
   [*] Symbol lookup for "a"
   Found "a" with type : [Integer]
   [*] Processing fresh assignment "ma"
   EnvAddMode(ma,%9)
   [*] Symbol lookup for "ma"
   Found "ma" with type : [%9]
   [*] Processing function application.
   [*] Symbol lookup for "-"
   Found "-" with type : [(Integer,Integer) -> Integer,Integer -> Integer]
   [*] Symbol lookup for "a"
   Found "a" with type : [Integer]
   [*] Processing conditional expression.
   [*] Processing function application.
   [*] Symbol lookup for ">="
   Found ">=" with type : [(Integer,Integer) -> Boolean]
   [*] Symbol lookup for "a"
   Found "a" with type : [Integer]
   [*] Processing function application.
   [*] Symbol lookup for "Zero"
   Found "Zero" with type : [() -> Integer]
   [*] Symbol lookup for "ma"
   Found "ma" with type : [%9]
   [*] Symbol lookup for "a2"
   Found "a2" with type : [%5]
```

Now the blackboard is dumped. Note that type fields are filled partially and not all nodes are marked with plus sign - these are the nodes to be processed by type inference algorithm:

```
   [*] Tree ready for unification :
   Nodes :
   (-)  #1 <- {#2} is Integer
   (-)  #2 <- {#14} is %3
   (-)  #3 <- {#4 := #5} is %4
   (-)  #4 is %5
   (-)  #5 <- {#6(#7,#8)} is %6
   (-)  #6 is %7
   (-)  #7 is Integer
   (-)  #8 is Integer
   (-)  #9 <- {#10 := #11} is %8
   (-) #10 is %9
   (-) #11 <- {#12(#13)} is %10
   (-) #12 is %11
   (-) #13 is Integer
   (-) #14 <- {if #15 then #20 else #21} is %12
   (-) #15 <- {#16(#17,#18)} is %13
   (-) #16 is (Integer,Integer) -> Boolean
   (-) #17 is Integer
   (-) #18 <- {#19()} is %14
   (-) #19 is () -> Integer
   (-) #20 is %9
   (-) #21 is %5
```

And type inference algorithm kicks in:

```
   [*] Run type inference for #18 of %14 type.
   [*] Modyfing %14 type variable with [Integer]
   [*] Type inferred for node #18 : Integer
   [*] Rewrote #18 with {%14 => Integer}
   [*] Processing node #18 completed!
   [*] Run type inference for #15 of %13 type.
   [*] Modyfing %13 type variable with [Boolean]
   [*] Type inferred for node #15 : Boolean
   [*] Rewrote #15 with {%13 => Boolean}
   [*] Processing node #15 completed!
   [*] Run type inference for #14 of %12 type.
   [*] Modyfing %9 type variable with [%5]
   [*] Modyfing %12 type variable with [%9,%5]
   [*] Run type inference for #11 of %10 type.
   [*] Modyfing %10 type variable with [Integer]
   [*] Type inferred for node #11 : Integer
   [*] Rewrote #11 with {%10 => Integer}
   [*] Run type inference for #9 of %8 type.
   [*] Modyfing %5 type variable with [Integer]
   [*] Modyfing %9 type variable with [%8]
   [*] Modyfing %8 type variable with [Integer]
   [*] Run type inference for #5 of %6 type.
   [*] Modyfing %6 type variable with [Integer]
   [*] Type inferred for node #5 : Integer
   [*] Rewrote #5 with {%6 => Integer}
   [*] Run type inference for #3 of %4 type.
   [*] Modyfing %5 type variable with [Integer]
   [*] Modyfing %5 type variable with [%4]
   [*] Modyfing %4 type variable with [Integer]
   [*] Run type inference for #2 of %3 type.
   [*] Modyfing %3 type variable with [Integer]
   [*] Modyfing %8 type variable with [%3]
   [*] Run type inference for #1 of Integer type.
   [*] Modyfing %3 type variable with [Integer]
   [*] Type inferred for node #21 : Integer
   [*] Rewrote #4 with {%5 => Integer}
   [*] Rewrote #21 with {%5 => Integer}
   [*] Processing node #21 completed!
   [*] Type inferred for node #20 : Integer
   [*] Rewrote #10 with {%9 => Integer}
   [*] Rewrote #20 with {%9 => Integer}
   [*] Processing node #20 completed!
   [*] Run type inference for #14 of %12 type.
   [*] Type inferred for node #14 : Integer
   [*] Rewrote #14 with {%12 => Integer}
   [*] Processing node #14 completed!
   [*] Type inferred for node #12 : Integer -> Integer
   [*] Rewrote #12 with {%11 => Integer -> Integer}
   [*] Processing node #12 completed!
   [*] Run type inference for #11 of Integer type.
   [*] Processing node #11 completed!
   [*] Run type inference for #9 of %8 type.
   [*] Modyfing %8 type variable with [Integer]
   [*] Type inferred for node #9 : Integer
   [*] Rewrote #9 with {%8 => Integer}
   [*] Processing node #9 completed!
   [*] Type inferred for node #6 : (Integer,Integer) -> Integer
   [*] Rewrote #6 with {%7 => (Integer,Integer) -> Integer}
   [*] Processing node #6 completed!
   [*] Run type inference for #5 of Integer type.
   [*] Processing node #5 completed!
   [*] Run type inference for #3 of %4 type.
   [*] Modyfing %4 type variable with [Integer]
   [*] Type inferred for node #3 : Integer
   [*] Rewrote #3 with {%4 => Integer}
   [*] Processing node #3 completed!
   [*] Run type inference for #2 of %3 type.
   [*] Type inferred for node #2 : Integer
   [*] Rewrote #2 with {%3 => Integer}
   [*] Processing node #2 completed!
   [*] Run type inference for #1 of Integer type.
   [*] Processing node #1 completed!
```

The algorithm finished successfully and dumps the blackboard, but with all type fields filled in.

```
   Nodes :
   (+)  #1 <- {#2} is Integer
   (+)  #2 <- {#14} is Integer
   (+)  #3 <- {#4 := #5} is Integer
   (+)  #4 is Integer
   (+)  #5 <- {#6(#7,#8)} is Integer
   (+)  #6 is (Integer,Integer) -> Integer
   (+)  #7 is Integer
   (+)  #8 is Integer
   (+)  #9 <- {#10 := #11} is Integer
   (+) #10 is Integer
   (+) #11 <- {#12(#13)} is Integer
   (+) #12 is Integer -> Integer
   (+) #13 is Integer
   (+) #14 <- {if #15 then #20 else #21} is Integer
   (+) #15 <- {#16(#17,#18)} is Boolean
   (+) #16 is (Integer,Integer) -> Boolean
   (+) #17 is Integer
   (+) #18 <- {#19()} is Integer
   (+) #19 is () -> Integer
   (+) #20 is Integer
   (+) #21 is Integer
```

Following line ends the algorithm:

```
>>>> POST-PARSE HOOK END <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
```
