#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <gc.h>

#include "vmdata.h"

int gc_start() {
  GC_INIT();
  return 1;
}

GEN box_i32(int32_t a) {
  integer_t *val = GC_malloc(sizeof(integer_t));

  val->descriptor = integer_key;
  val->ival = a;

  return (GEN)val;
}

int32_t unbox_i32(GEN ptr) {
  integer_t *val = (integer_t *)ptr;

  assert(val->descriptor == integer_key);

  return val->ival;
}
