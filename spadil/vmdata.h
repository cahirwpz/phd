#ifndef __VMDATA_H__
#define __VMDATA_H__

typedef void *GEN;
typedef GEN KEY;
typedef long LONG;
typedef int CHAR;

struct type_key {
  KEY descriptor;
  char *name;
};

const GEN key_key;
const GEN vector_1d_key;
const GEN string_key;
const GEN symbol_key;
const GEN integer_key;
const GEN bigint_key;
const GEN dfloat_key;
const GEN cons_key;

typedef struct vector_1d {
  KEY descriptor;
  LONG size;
  GEN data[0];
} vector_1d_t;

typedef struct string {
  KEY descriptor;
  LONG size;
  CHAR data[0];
} string_t;

typedef struct symbol {
  KEY descriptor;
  GEN name;
  GEN properties;
} symbol_t;

typedef struct dfloat {
  KEY descriptor;
  double dval;
} dfloat_t;

typedef struct integer {
  KEY descriptor;
  LONG ival;
} integer_t;

typedef struct bigint {
  KEY descriptor;
  LONG size;
  LONG data[0];
} bigint_t;

typedef struct cons {
  KEY descriptor;
  GEN fst;
  GEN snd;
} cons_t;

const GEN vm_nil;

#endif
