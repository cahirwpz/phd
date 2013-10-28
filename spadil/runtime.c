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
