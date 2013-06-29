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

### Error handling

```
error(str): String => ()

```

### Printing

```
print(obj): Any => ()
```

### Type handling

```
type_of(obj): Any => Type

is_cons?(a): Any => Bool
is_int?(a): Any => Bool
is_float?(a): Any => Bool
```

### Type conversion

```	
int_to_bool(i): Int => Bool
int_to_float(i): Int => Float
float_to_bool(f): Float => Bool
float_to_int(f): Float => Int
```

### Boxing and unboxing

```
box_bool(b): Bool => Any
box_int(i): Int => Any 
box_float(f): Float => Any
```

```
unbox_bool(a): Any => Bool
unbox_int(a): Any => Int
unbox_float(a): Any => Float
```

### Lists

```
null(l): Any => Bool
cons(a,b): Any => Any => Any
first(l): Any => Any
rest(l): Any => Any
```

### Arrays

```
vector(n): Int => Any
getelt(arr, n): Any => Int => Any
setelt(arr, n, obj): Any => Int => Any => Any
```