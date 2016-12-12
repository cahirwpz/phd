#define NDEBUG
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <gc.h>

#undef NULL

#include "vmdata.c"

int gc_start() {
  GC_INIT();
  return 1;
}

void error(const char *str) {
  puts(str);
  abort();
}

void print_ANY(GEN any);

void print_eol(void) {
  putchar('\n');
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
  printf("%ld", a);
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
  printf("%.15g", a);
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

GEN NIL() {
  return vm_nil;
}

bool NULL(GEN ptr) {
  return ptr == vm_nil;
}

bool ATOM(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  return val->descriptor != cons_key;
}

GEN NREVERSE(GEN lst) {
  if (NULL(lst))
    return lst;

  GEN rev = vm_nil;

  do {
    rev = CONS(CAR(lst), rev);
    lst = CDR(lst);
  } while (!NULL(lst));

  return rev;
}

GEN SPADfirst(GEN ptr) {
  cons_t *val = (cons_t *)ptr;

  assert(val->descriptor == cons_key);

  if (val->fst == vm_nil)
    error("Cannot take first of an empty list");

  return val->fst;
}

LONG LENGTH(GEN lst) {
  LONG l = 0;

  while (!NULL(lst)) {
    lst = CDR(lst);
    l++;
  }

  return l;
}

void print_CONS(GEN lst) {
  putchar('[');
  while (!NULL(lst)) {
    print_ANY(CAR(lst));
    lst = CDR(lst);
    if (!NULL(lst))
      printf(", ");
  }
  putchar(']');
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

  val->descriptor = vector_1d_key;
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

GEN LIST2VEC(GEN lst) {
  LONG i = 0;
  LONG n = LENGTH(lst);
  GEN vec = MAKEARR1(n, 0);
  GEN *arr = QVREF(vec);

  while (i < n) {
    arr[i++] = CAR(lst);
    lst = CDR(lst);
  }

  return vec;
}

void print_VEC(GEN ptr) {
  vector_1d_t *vec = (vector_1d_t *)ptr;

  LONG i = 0;
  LONG n = vec->size;

  putchar('<');
  while (i < n) {
    print_ANY(vec->data[i++]);
    if (i < n)
      printf(", ");
  }
  putchar('>');
}

/* Any */

void print_ANY(GEN ptr) {
  any_t *any = (any_t *)ptr;
  KEY key = any->descriptor;

  if (key == integer_key)
    print_SI(unbox_SI(ptr));
  else if (key == dfloat_key)
    print_DF(unbox_DF(ptr));
  else if (key == cons_key)
    print_CONS(ptr);
  else if (key == vector_1d_key)
    print_VEC(ptr);
  else
    putchar('?');
}
