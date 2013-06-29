
# Types and data structures

## Type representation

 * `data_type`: representation of builtin types

		data_type = ['Bool, 'Int, 'Float, 'String, 'Any]

	**Note:** Strings are immutable!

 * `fun_type`: function type
 
		type fun_type = { ft_args: data_type list,
		                  ft_result: data_type option }

    If the function returns `Void` type second element of pair is `None` (look at [Option] type in OCaml library).

[Option]: http://ocaml-lib.sourceforge.net/doc/Option.html

 * `the_type`:
 
		the_type = FunType of fun_type
		         | DataType of data_type
		         | UnknownType
 
 * `symbol`:

		symbol = { s_name : string, s_type : the_type }
		
	Eventually, we'll need to add symbol attributes here like `inline`, `global`, etc.
 
### Environment

	(symbol list) stack

# Abstract Syntax Trees

## Constants

	Char of char
	Int of int
	Float of float
	String of string

## Variables

### Variable introduction

	Let of (VarSet.t * tree)

represents

	let
	  [attr] name1 : type1
	  [attr] name2 : type2
	   … 
	  [attr] nameN : typeN
	in
	  expr

which makes `name1`, `name2`, …, `nameN` visible in the scope of expression execution. The space for these variables is allocated, but their value is undefined.

`[attr]` is a list of variable attributes, which may be:

 * `local`: allocated on local stack 
 * `global`: global variable

Note that `VarSet.t` is not allowed to be empty!

### Variable assignment

	Assign of (symbol * tree)
	
represents:

	name := expr

where variable `name` has already been defined. We assume that the type of `expr` matches of this declared for `name` in the environment.

### Variable dereference

	Symbol of string

represents dereference of a variable - i.e. obtains its value.

**Q**: What to do about uninitialized variables?

### Global variables

	Global of typed_sym * tree option

represents (at the global level):

	var name : type

or:

	var name : type := expr
	
… if optional expression was defined.

## Functions

### Function application

	Apply of (tree * tree list)

### Function call

	Call of (symbol * tree list)

represents:

	fun (expr1, expr2, …, exprN)

where function `fun` has been already defined. Usual type conformance 

### Block definition

Block is a series of expressions, evaluated one by one. It has the type and value of last expression that was evaluated.

	Block of tree list
	
represents:

	  begin name
	    expr1
	    expr2
	     … 
	    exprN
	  end

### Lambda expression

	Lambda of string list * tree

### Return from function

	Return of tree

## Control structures

### `if` statement

Note that `if` is statement and not expression, thus it has no value.

	IfThen of tree * tree

represents:

	if b-expr
	  then expr

where `b-expr` **must** return a boolean value.

### `if-then-else` expression

	IfThenElse of tree * tree * tree

represents:

	if b-expr
	  then expr1
	  else expr2

where `b-expr` **must** return a boolean value and `expr1` and `expr2` have to be of same type.

This is an expression so it has the type and value of respective branch.

### `while` statement

	while of tree * tree

## Others - WTF?

### List constructor

`Cons` is a constructor for polymorphic lists.

	Cons of tree * tree

represents:

	x::xs
	
where `x` is a value, and `xs` is a list. 

### No idea at all

	Value of string
