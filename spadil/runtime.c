#define NDEBUG
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <gc.h>

#undef NULL

#include "vmdata.h"

int gc_start() {
  GC_INIT();
  return 1;
}

void error(const char *str) {
  puts(str);
  abort();
}

/* SingleInteger */

GEN box_SI(LONG a) {
  integer_t *val = GC_malloc(sizeof(integer_t));

  val->descriptor = integer_key;
  val->ival = a;

  return (GEN)val;
}

LONG unbox_SI(GEN ptr) {
  integer_t *val = (integer_t *)ptr;

  assert(val->descriptor == integer_key);

  return val->ival;
}

void print_SI(LONG a) {
  printf("%ld\n", a);
}

/* DoubleFloat */

GEN box_DF(double a) {
  dfloat_t *val = GC_malloc(sizeof(dfloat_t));

  val->descriptor = dfloat_key;
  val->dval = a;

  return (GEN)val;
}

double unbox_DF(GEN ptr) {
  dfloat_t *val = (dfloat_t *)ptr;

  assert(val->descriptor == dfloat_key);

  return val->dval;
}

void print_DF(double a) {
  printf("%.15g\n", a);
}

/* Cons */

GEN CONS(GEN fst, GEN snd) {
  cons_t *val = GC_malloc(sizeof(cons_t));

  val->descriptor = cons_key;
  val->fst = fst;
  val->snd = snd;

  return (GEN)val;
}

GEN CAR(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  assert(val->descriptor == cons_key);

  return val->fst;
}

GEN CDR(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  assert(val->descriptor == cons_key);

  return val->snd;
}

bool NULL(GEN ptr) {
  return ptr == vm_nil;
}

bool ATOM(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  return val->descriptor != cons_key;
}

GEN NREVERSE(GEN ptr) {
  /* TODO */
  return ptr;
}

GEN SPADfirst(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  assert(val->descriptor == cons_key);

  if (val->fst == vm_nil)
    error("Cannot take first of an empty list");

  return val->fst;
}

/* Rational numbers */

GEN DIVIDE2(LONG a, LONG b) {
  return CONS(box_SI(a / b), box_SI(a % b));
}

LONG QCAR(GEN ptr) {
  cons_t *cons = (cons_t *)ptr;

  assert(cons->descriptor == cons_key);

  integer_t *integer = (integer_t *)cons->fst;

  assert(integer->descriptor == integer_key);

  return integer->ival;
}

LONG QCDR(GEN ptr) {
  cons_t *cons = (cons_t *)ptr;

  assert(cons->descriptor == cons_key);

  integer_t *integer = (integer_t *)cons->snd;

  assert(integer->descriptor == integer_key);

  return integer->ival;
}

/* Vector 1D */

GEN MAKEARR1(LONG size, LONG init) {
  vector_1d_t *val = GC_malloc(sizeof(vector_1d_t) + sizeof(GEN) * size);

  val->size = size;

  for (int i = 0; i < size; i++)
    val->data[i] = vm_nil;

  return val;
}

GEN QREFELT(GEN ptr, LONG index) {
  vector_1d_t *val = (vector_1d_t *)ptr;

  return val->data[index];
}

GEN *QVREF(GEN ptr) {
  vector_1d_t *val = (vector_1d_t *)ptr;

  assert(val->descriptor == vector_1d_key);

  return val->data;
}

LONG QVSIZE(GEN ptr) {
  vector_1d_t *val = (vector_1d_t *)ptr;

  assert(val->descriptor == vector_1d_key);

  return val->size;
}
