# Virtual Machine runtime description

## Type representation

### Type descriptor

```
typedef (void *) ANY;
typedef (struct Type) TYPE;

struct Type {
  (TYPE *) type;
  (char *) name;
};

const TYPE type_t = { &type_t, "type" };
```

#### Cons

```
const TYPE cons_t = { &type_t, "cons" };

typedef struct {
  (TYPE *) type;
  ANY      fst;
  ANY      snd;
} cons_s;

const cons_s nil = { &cons_t, (ANY)&nil, (ANY)&nil };
```

#### Vector

```
const TYPE vector_t = { &type_t, "vector" };

typedef struct {
  (TYPE *) type;
  size_t   size;
  ANY      data[0];
} vector_s;
```

#### String

```
const TYPE string_t = { &type_t, "string" };

typedef struct {
  (TYPE *) type;
  size_t   size;
  char     data[0];
} string_s;
```

#### Symbol

```
const TYPE symbol_t  = { &type_t, "symbol" };

typedef struct symbol {
  (TYPE *) type;
  ANY      properties;
  char     name[0];
} symbol_t;
```

#### Float

```
const TYPE float_t = { &type_t, "float" };

typedef struct {
  (TYPE *) type;
  double   value;
} float_s;
```

#### Integer

```
const TYPE integer_t = { &type_t, "integer" };

typedef struct {
  (TYPE *) type;
  long     value;
} integer_s;
```

## Builtin runtime functions

It is crucial here to distinguish between:

* **runtime types** : `Integer`, `Float`, `String`, `Cons`, `Any`, etc. 
* **LLVM types** : `void`, `i1`, `i8 *`, `i32`, `double`, etc.

LLVM type system is described [here][LLVM types].

[LLVM types]: http://llvm.org/docs/LangRef.html#type-system

### Error handling

```
error(str : <i8 *>) : void
```

This function does not return.

### Printing

```
print(obj : Any) : void
```

### Type handling

```
type_of(obj : Any) : Type
```

```
is_cons?(obj : Any) : i1
is_integer?(obj : Any) : i1
is_float?(obj : Any) : i1
is_string?(obj : Any) : i1
is_symbol?(obj : Any) : i1
```

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

```
box_bool(b : i1) : Bool
box_int(n : i32) : Integer 
box_float(f : double) : Float
```

```
unbox_bool(obj : Bool) : i1
unbox_int(obj : Integer) : i32
unbox_float(obj : Float) : double
```

### Lists

```
null(list : Cons) : i1
cons(a : Any, b : Any) : Cons
first(list : Cons) : Any
rest(list : Cons) : Any
```

### Arrays

```
vector(n : i32) : Vector
getelt(vec : Vector, n : i32) : Any
setelt(vec : Vector, n : i32, obj : Any) : void
```