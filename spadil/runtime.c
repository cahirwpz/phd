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

GEN box_SI(int32_t a) {
  integer_t *val = GC_malloc(sizeof(integer_t));

  val->descriptor = integer_key;
  val->ival = a;

  return (GEN)val;
}

GEN box_DF(double a) {
  dfloat_t *val = GC_malloc(sizeof(dfloat_t));

  val->descriptor = dfloat_key;
  val->dval = a;

  return (GEN)val;
}

GEN CONS(GEN fst, GEN snd) {
  cons_t *val = GC_malloc(sizeof(cons_t));

  val->descriptor = cons_key;
  val->fst = fst;
  val->snd = snd;

  return (GEN)val;
}

bool NULL(GEN ptr) {
  return ptr == vm_nil;
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

GEN *QVREF(GEN ptr) {
  vector_1d_t *val = (vector_1d_t *)ptr;

  assert(val->descriptor == vector_1d_key);

  return val->data;
}

int32_t QVSIZE(GEN ptr) {
  vector_1d_t *val = (vector_1d_t *)ptr;

  assert(val->descriptor == vector_1d_key);

  return val->size;
}

GEN SPADfirst(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  assert(val->descriptor == cons_key);

  if (val->fst == vm_nil)
    error("Cannot take first of an empty list");

  return val->fst;
}

int32_t unbox_SI(GEN ptr) {
  integer_t *val = (integer_t *)ptr;

  assert(val->descriptor == integer_key);

  return val->ival;
}

void print_SI(int32_t a) {
  printf("%d\n", a);
}

void print_DF(double a) {
  printf("%.15g\n", a);
}
