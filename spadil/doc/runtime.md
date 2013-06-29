# Runtime built-in types, operators and functions

It is crucial here to distinguish between:

* **runtime types** : `Type`, `Any`, `Integer`, `Float`, `String`, `Symbol`, `Cons`, `Nil`, `Vector`. 
* **LLVM types** : `void`, `i1` (aka `bool`), `i8 *` (aka `char *`), `i32` (aka `int`), `double`, etc.

LLVM type system is described [here][LLVM types].

[LLVM types]: http://llvm.org/docs/LangRef.html#type-system

## Built-in types

### Type descriptor (Type)

```c
struct type {
  (struct type *) type;
  (i8 *) name;
} type_s;

const type_s type_t = { &type_t, "type" };

typedef (type_s *) Type;
```

### Generic value (Any)

```c
typedef (Type *) Any;
```

### Machine integer (Integer)

```c
typedef struct {
  Type type;
  i32  value;
} integer_s;

const type_s integer_t = { &type_t, "integer" };

typedef (integer_s *) Integer;
```

### Machine float (Float)

```c
typedef struct {
  Type   type;
  double value;
} float_s;

const type_s float_t = { &type_t, "float" };

typedef (float_s *) Float;
```

### String

```c
typedef struct {
  Type type;
  i32  size;
  i8   data[0];
} string_s;

const type_s string_t = { &type_t, "string" };

typedef (string_s *) String;
```

### Symbol

```c
typedef struct symbol {
  Type type;
  i8*  name;
  Cons properties;
} symbol_s;

const type_s symbol_t = { &type_t, "symbol" };

typedef (symbol_s *) Symbol;
```

### List (Cons)

```c
typedef struct cons {
  Type type;
  Any  fst;
  Any  snd;
} cons_s;

const type_s cons_t = { &type_t, "cons" };

typedef (cons_s *) Cons;
```

### Empty list value (Nil)

```c
const cons_s nil_v = { &cons_t, (Any)&nil, (Any)&nil };

const Cons Nil = &nil_v;
```

### Linear array (Vector)

```c
typedef struct vector {
  Type type;
  i32  size;
  Any  data[0];
} vector_s;

const type_s vector_t = { &type_t, "vector" };

typedef (vector_s *) Vector;
```

## Built-in operators

### Unary (prefix)

Valid for `Integer` and `Float`:
* `-` (inverse)

Valid for `Bool`:
* `NOT` (logical negation)

### Binary (infix)

Valid for `Integer` and `Float`:
* `+` (add)
* `-` (substract)
* `*` (multiply)
* `/` (divide)
* `<` (less than)
* `>` (greater than)
* `<=` (less or equal to)
* `>=` (greater or equal to)
* `=` (equal)
* `/=` (not equal)

Valid for `Bool`:
* `OR` (logical disjunction)
* `AND` (logical conjuction)

## Built-in functions

### Error handling

```
error(str : <i8 *>) : void
```

This function prints out a messages and calls `exit()`, i.e. it does not return.

There's no exception handling right now.

### Printing

```
print(obj : Any) : void
```

### Type handling

```
type_of(obj : Any) : Type
cast(obj : Any, type : Type) : type
```

`cast` check if an object matches the type, if not it calls `error`.

### Type conversion

```	
integer_to_bool(n : i32) : i1
integer_to_float(n : i32) : double
```

```
float_to_bool(f : double) : i1
float_to_integer(f : double) : i32
```

### Boxing and unboxing

The compiler can automatically promote the type of a value in both directions.

```
box_bool(b : i1) : Bool
box_integer(n : i32) : Integer 
box_float(f : double) : Float
```

```
unbox_bool(obj : Bool) : i1
unbox_integer(obj : Integer) : i32
unbox_float(obj : Float) : double
```

### Lists

```
cons(a : Any, b : Any) : Cons
null(list : Cons) : i1
first(list : Cons) : Any
rest(list : Cons) : Any
```

### Arrays

**Q**: What about bounds checking? Should it crash exactly the same way as `cast`?

```
vector(n : i32) : Vector
getelt(vec : Vector, n : i32) : Any
setelt(vec : Vector, n : i32, obj : Any) : void
```
