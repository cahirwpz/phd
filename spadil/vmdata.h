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
const GEN bigint_key;
const GEN defloat_key;

struct vector_1d {
  KEY descriptor;
  LONG size;
  GEN data[1];
};

struct string {
  KEY descriptor;
  LONG size;
  CHAR data[1];
};

struct symbol {
  KEY descriptor;
  GEN name;
  GEN properties;
};

struct cons {
  GEN first;
  GEN next;
};

struct dfloat {
  KEY descriptor;
  double dval;
};

struct bigint {
  KEY descriptor;
  LONG size;
  LONG data[1];
};

#endif
