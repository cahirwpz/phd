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

### Logging

During type checking various messages are printed out. Each message consist of three fields:

```
(0.001) [Main] A very important message.
 ^       ^     ^
 |       |     |
 |       |     the beginning of a message (possibly multiline)
 |       component name, colour means logging level
 time in seconds from the moment when compiler entered new type checker code
```

There are 5 logging levels which roughly correspond in the meaning to unix `syslog` levels. Component names are coloured to mark importance of message:

| level  | colour  |
|:------:|:-------:|
| debug  | magenta | 
| info   | cyan    |
| notice | green   |
| warn   | yellow  |
| fail   | red     |

Logger mechanism is configurable. You can specify for each component which messages will be logged, for instance (snippet from `SpadCompilerTest.spad`):

```
loggerDefaultLevel "debug"
loggerLevel('Unify, "info")
loggerLevel('Type, "info")
loggerLevel('Parser, "notice")
loggerLevel('Main, "notice")
```

### Components

| name     | responsiblities |
|:--------:|:----------------|
| `Daase`  | Communicates and extracts type information from the database of original compiler. |
| `Env`    | Populates and accesses old compiler environment data structure. Makes possible to read all function signatures associated with given type. |
| `Parser` | Translates between original (*SExpression*) and type checker internal (*SpadNode*) representation of abstract syntax trees. Performs additional syntax checks. |
| `AST`    | Abstract syntax tree analyzer, walks over AST and constructs type facts in form of a "blackboard" data structure. |
| `Unify` | Performs type unification algorithm and various variations of it. Additionaly may check if one type is subtype of another type. |
| `Type` | Type checker algorithm. Operates on "blackboard" data structure. Infers or verifies types present in a function. |

### Interpreting output messages

##### Source AST in internal representation

This should contain the same information as input file, but the printout should be perfomed by internal code. After that an abstract syntax tree analyzer will be run, which in turn will construct a blackboard with type facts.

```
(0.001) [Main] Tree ready for type checking:
  Test1 () :
      foo : Integer -> Integer
      foo : Float -> Float
      bar : (Integer, Integer) -> Integer
    == ? add
      import Float
      import PositiveInteger
      import NonNegativeInteger
      foo (a : ?) ==
        a2 := (a * a)
        ma := -(a)
        if (a >= 0)
        then
          (ma * 2)
        else
          (a2 + 2)
      baz : (Integer, Integer) -> Integer
      bar (x : ?, y : ?) ==
        nx : Integer
        if (x > 0)
        then
          y := -(10)
          nx := baz(x, y)
        else
          nx := -(x)
        (nx + y)
      baz (x : Integer, y : Integer) : Integer ==
        (x * y)
```

##### Blackboard state before type checker run

These two data structures represented by *TypeCheckerNodeArray* domain express all information that `AST` phase was able to gather about the function or functor under question (`foo : Integer -> Integer` in this case).

Nodes printout represents AST with type information and typing rules. 

1. `(-)` or `(+)` indicate if type checker finished inferring type for this node,
2. `#n` is node number,
3. there are two possiblities:
   - `symbol : Type` : a leaf - usually a symbol,
   - `{...} is Type` : a node with typing rule attached.

Note that some nodes have a type named `%n`. These are type variables stored in second data structure. If a given node there're several alternatives available `[T1, T2, ...]` or the type is unknown `?`, a type variable will be assigned for it. Though there're cases where artificial nodes are generated - they don't correspond to any node from AST.

Last things that requires some explanation is rule format. Actually each node can be assigned several typing rules (that happens sometimes - e.g. for `::` or implict `elt` use aka aggregate access). Each rule consist of several components.

Below is current grammar for rules (subject to change):

* `{r1, r2, ...}` : single rule composed of rule components,
* `{...} | {...} | ...` : alternative rules,
* `#n` - node *n* must be visited before proceeding,
* `#n <: #m` : type assigned to node *n* is in subtyping relation with node *m*,
* `#n := #m` : node *n* will have a (super)type of node *m*, same for current node.
* `#n(#x, #y, ...)` : function application rule, *n* has a function type, (*x*, *y*, ...) will be subtypes of what can be applied to the function,
* `#n $ Type` : function types assigned to *n* must originate from `Type` domain.

There're some special type variables:

* `%any` - represents any type (aka top type) - used to handle `error` statement,
* `%undef` - undefined type (aka bottom type) assigned to each statement (e.g. *if-then* without *else* branch).

```
Nodes :
  (-)  #1 {#2, #3} is Integer -> Integer
  (-)  #2 a : Integer
  (-)  #3 {#4, #4 <: #3} is Integer
  (-)  #4 {#5, #11, #16, #16 <: #4} is %3
  (-)  #5 {#6, #7, #6 := #7} is %4
  (-)  #6 a2 : %5
  (-)  #7 {#8, #9, #10, #8(#9, #10)} is %6
  (-)  #8 * : %7
  (-)  #9 a : Integer
  (-) #10 a : Integer
  (-) #11 {#12, #13, #12 := #13} is %10
  (-) #12 ma : %11
  (-) #13 {#14, #15, #14(#15)} is %12
  (-) #14 - : %13
  (-) #15 a : Integer
  (-) #16 {#17, #22, #26, if #17 then #22 else #26} is %24
  (-) #17 {#18, #19, #20, #18(#19, #20)} is %15
  (-) #18 >= : (Integer, Integer) -> Boolean $ Integer
  (-) #19 a : Integer
  (-) #20 {#21, #21()} is %18
  (-) #21 Zero : () -> Integer $ Integer
  (-) #22 {#23, #24, #25, #23(#24, #25)} is %20
  (-) #23 * : %21
  (-) #24 ma : %11
  (-) #25 2 : Integer
  (-) #26 {#27, #28, #29, #27(#28, #29)} is %22
  (-) #27 + : (Integer, Integer) -> Integer $ Integer
  (-) #28 a2 : %5
  (-) #29 2 : Integer
Type variables :
    %3 : ?
    %4 : ?
    %5 : ?
    %6 : ?
    %7 : [(Integer, Integer) -> Integer $ Integer, (NonNegativeInteger, Integer) -> Integer $ Integer, (PositiveInteger, Integer) -> Integer $ Integer]
   %10 : ?
   %11 : ?
   %12 : ?
   %13 : [(Integer, Integer) -> Integer $ Integer, Integer -> Integer $ Integer]
   %15 : ?
   %18 : ?
   %20 : ?
   %21 : [(Integer, Integer) -> Integer $ Integer, (NonNegativeInteger, Integer) -> Integer $ Integer, (PositiveInteger, Integer) -> Integer $ Integer]
   %22 : ?
   %24 : [%20, %22, Union(%20, %22), Union(%22, %20)]
```

##### Blackboard state after type checker run

This is how a complete type checked tree should look like. For your convenience `(+)` are coloured in green and `(-)` in red.

This example doesn't show that, but final tree must not have any rule alternatives in any of nodes. 

```
  (+)  #1 {#2, #3} is Integer -> Integer
  (+)  #2 a : Integer
  (+)  #3 {#4, #4 <: #3} is Integer
  (+)  #4 {#5, #11, #16, #16 <: #4} is Integer
  (+)  #5 {#6, #7, #6 := #7} is Integer
  (+)  #6 a2 : Integer
  (+)  #7 {#8, #9, #10, #8(#9, #10)} is Integer
  (+)  #8 * : (Integer, Integer) -> Integer $ Integer
  (+)  #9 a : Integer
  (+) #10 a : Integer
  (+) #11 {#12, #13, #12 := #13} is Integer
  (+) #12 ma : Integer
  (+) #13 {#14, #15, #14(#15)} is Integer
  (+) #14 - : Integer -> Integer $ Integer
  (+) #15 a : Integer
  (+) #16 {#17, #22, #26, if #17 then #22 else #26} is Integer
  (+) #17 {#18, #19, #20, #18(#19, #20)} is Boolean
  (+) #18 >= : (Integer, Integer) -> Boolean $ Integer
  (+) #19 a : Integer
  (+) #20 {#21, #21()} is Integer
  (+) #21 Zero : () -> Integer $ Integer
  (+) #22 {#23, #24, #25, #23(#24, #25)} is Integer
  (+) #23 * : (Integer, Integer) -> Integer $ Integer
  (+) #24 ma : Integer
  (+) #25 2 : Integer
  (+) #26 {#27, #28, #29, #27(#28, #29)} is Integer
  (+) #27 + : (Integer, Integer) -> Integer $ Integer
  (+) #28 a2 : Integer
  (+) #29 2 : Integer
```

##### Type annotated tree passed to original compiler

Original tree passed to new type checker will be rewritten and type annotations will be added.

```
  Test1 () :
      foo : Integer -> Integer
      foo : Float -> Float
      bar : (Integer, Integer) -> Integer
    == Type add
      foo (a : Integer) : Integer ==
        a2 : Integer := (* $ Integer)(a, a)
        ma : Integer := (- $ Integer)(a)
        if (>= $ Integer)(a, Zero $ Integer)
        then
          (* $ Integer)(ma, 2)
        else
          (+ $ Integer)(a2, 2)
      baz : (Integer, Integer) -> Integer
      bar (x : Integer, y : Integer) : Integer ==
        nx : Integer
        if (> $ Integer)(x, Zero $ Integer)
        then
          y := (- $ Integer)(10)
          nx := baz(x, y)
        else
          nx := (- $ Integer)(x)
        (+ $ Integer)(nx, y)
      baz (x : Integer, y : Integer) : Integer ==
        (* $ Integer)(x, y)
```