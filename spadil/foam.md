FOAM: FriCAS Open Abstract Machine
---

The Abstract machine employed by FriCAS is a subset of [Common Lisp][cl] instructions. At final compilations stages it is fed into one of following implementations of Common Lisp:

* [Steel Bank Common Lisp][sbcl]
* [GNU CLISP][clisp]

[cl]: http://clhs.lisp.se/Front/index.htm
[sbcl]: http://sbcl.org/
[clisp]: http://www.clisp.org/

The result of `${source}.spad` compilation can be found in `${build_dir}/src/algebra/${source}.NRLIB/${source}.lsp`.


Imported from Common Lisp
---

#### Constants

[NIL], [T]

[NIL]: http://clhs.lisp.se/Body/v_nil.htm
[T]: http://clhs.lisp.se/Body/v_t.htm#t

#### Operators

* Arithmetic: [+] (add), [-] (sub, neg), [*] (mul), [/] (div)
* Logic: [OR], [AND], [NOT]
* Compare: [<] (lt), [>] (gt), [<=] (ge), [>=] (le), [=] (eq)

[+]: http://clhs.lisp.se/Body/f_pl.htm
[-]: http://clhs.lisp.se/Body/f__.htm
[*]: http://clhs.lisp.se/Body/f_st.htm
[/]: http://clhs.lisp.se/Body/f_sl.htm
[OR]: http://clhs.lisp.se/Body/a_or.htm#or
[AND]: http://clhs.lisp.se/Body/m_and.htm
[NOT]: http://clhs.lisp.se/Body/f_not.htm
[>]: http://clhs.lisp.se/Body/f_eq_sle.htm#GT
[<]: http://clhs.lisp.se/Body/f_eq_sle.htm#LT
[>=]: http://clhs.lisp.se/Body/f_eq_sle.htm#GE
[<=]: http://clhs.lisp.se/Body/f_eq_sle.htm#LE
[=]: http://clhs.lisp.se/Body/f_eq_sle.htm#EQ

#### Lists

[CAR], [CDR], [CONS], [LIST], [LENGTH], [NULL]

[CAR]: http://clhs.lisp.se/Body/f_car_c.htm#car
[CDR]: http://clhs.lisp.se/Body/f_car_c.htm#cdr
[CONS]: http://clhs.lisp.se/Body/f_cons.htm
[LIST]: http://clhs.lisp.se/Body/a_list.htm#list
[LENGTH]: http://clhs.lisp.se/Body/f_length.htm#length
[NULL]: http://clhs.lisp.se/Body/f_null.htm

#### Others

[ATOM], [EQL], [APPLY]

[ATOM]: http://clhs.lisp.se/Body/f_atom.htm
[EQL]: http://clhs.lisp.se/Body/f_eql.htm
[APPLY]: http://clhs.lisp.se/Body/f_apply.htm#apply
[APPEND]: http://clhs.lisp.se/Body/f_append.htm#append

List of FOAM instructions
---

### `LETT`: variable assignment

	(LETT Name Value DebugInfo?)
	
Transformed to [SETQ], which is an expression.

	(SETQ Name Value)

[SETQ]: http://clhs.lisp.se/Body/s_setq.htm#setq

### `PROGN`: group of forms

[PROGN] has no [BLOCK] associated with it, so [RETURN] will skip it.

[PROGN]: http://clhs.lisp.se/Body/s_progn.htm#progn
[BLOCK]: http://clhs.lisp.se/Body/s_block.htm#block
[RETURN]: http://clhs.lisp.se/Body/m_return.htm#return
[RETURN-FROM]: http://clhs.lisp.se/Body/s_ret_fr.htm#return-from

### `PROG` & `SPROG`: local variables introduction

**Q:** Is variable shadowing allowed?

[PROG] implicitly establishes a block named `nil`.

[PROG]: http://clhs.lisp.se/Body/m_prog_.htm#prog

##### Typed version:

`SPROG` is a macro introduced to handle introduction of typed variables.

	(SPROG ((Var1 VarType1) (Var2 VarType2) … (VarN VarTypeN))
	  BODY)

Is transformed to:

	?

##### Untyped version:

	(PROG (Var1 Var2 … VarN)
	  BODY)

### `RETURN`: leave `nil` block

[RETURN] leaves the nearest block established by [PROG], [PROG1] or [PROG2] with certain value.

	(RETURN Value)

Which is equivalent to:

	(RETURN-FROM nil Value)

### `COND`: conditional execution

Makes use of [COND]. Transformed to [IF].

[IF]: http://clhs.lisp.se/Body/s_if.htm#if
[COND]: http://clhs.lisp.se/Body/m_cond.htm#cond

	(COND (Pred1 Action1)
    	  (Pred2 Action2)
    	   …
    	  (PredN ActionN))
    	  
Transformed to:

1. If only one predicate:

	    (IF Pred1
	      (PROGN Action1))

2. If two predicates and second one equals to `'T`:

	    (IF Pred1
	      (PROGN Action1)
	      (PROGN Action2))

3. Rules above are applied recursively, if more than two predicates.

### `SEQ`: sequence of expressions

`SEQ` expression has the value and type of last expression `ExprN`:

    (SEQ
        Expr1
      Label1
        Expr2
        …
      LabelM
        ExprN)

`SEQ` implicitly establishes a block named `seq`. 

Moreover `SEQ` is defined with [TAGBODY], so labels are allowed in its body. So far only `G190` and `G191` labels were discovered. Code uses [GO] to jump to these labels. This is primarily used to encode different kind of loops.

[TAGBODY]: http://clhs.lisp.se/Body/s_tagbod.htm#tagbody
[GO]: http://clhs.lisp.se/Body/s_go.htm#go

### `EXIT`: leave sequence

`(EXIT Value)` leaves the innermost `SEQ` block.

Translates to:

	(RETURN-FROM seq Value)


### `PROG1` & `PROG2`: special purpose blocks

[PROG1] and [PROG2] evaluate all forms within their body, but their value is determined by first of second form respectively.

[PROG1]: http://clhs.lisp.se/Body/m_prog1c.htm#prog1
[PROG2]: http://clhs.lisp.se/Body/m_prog1c.htm#prog2

	(PROG1
	  Expr1
	  Expr2
	  … 
	  ExprN)

Translates to:

	(LET (#1=(GENSYM))
	  (SETQ #1# Expr1)
	  Expr2
	  … 
	  ExprN
	  #1#)

For [PROG2] the transformation is analogous.

### `SPECIAL`: global variables

Declares `|$global|` named variable to be in *dynamic variables* space.

	(DECLARE (SPECIAL |$global|))

[PROGV]: http://clhs.lisp.se/Body/s_progv.htm#progv

### `DEFUN` & `SDEFUN`: function definition

[DEFUN] implicitly establishes a block of the name of declared function.

[DEFUN]: http://clhs.lisp.se/Body/m_defun.htm#defun

Definition of `FunName: Type1 => Type2 => … => ResultType` function:


	(SDEFUN FunName ((Arg1 Type1)
	                 (Arg2 Type2)
	                  …
	                 (Result ResultType))
	  BODY)

### Dynamic function resolution

##### Call a SPAD function:

	(SPADCALL Arg1 Arg2 … ArgN SpadFunRef)

##### Dereference a field of SPAD object:

	(QREFELT Object FieldNum)

Returns a reference to given field of an object, which usually is a *SPAD function*.
