#include "vmdata.h"

const struct type_key key_key_struct = {
  (KEY)&key_key_struct, "key" };
const struct type_key vector_1d_key_struct = {
  (KEY)&key_key_struct, "vector_1d" };
const struct type_key string_key_struct = {
  (KEY)&key_key_struct, "string" };
const struct type_key symbol_key_struct = {
  (KEY)&key_key_struct, "symbol" };
const struct type_key integer_key_struct = {
  (KEY)&key_key_struct, "integer" };
const struct type_key bigint_key_struct = {
  (KEY)&key_key_struct, "bigint" };
const struct type_key dfloat_key_struct = {
  (KEY)&key_key_struct, "dfloat_id" };
const struct type_key cons_key_struct = {
  (KEY)&key_key_struct, "cons" };

const GEN key_key = (GEN)&key_key_struct;
const GEN vector_1d_key = (GEN)&vector_1d_key_struct;
const GEN string_key = (GEN)&string_key_struct;
const GEN symbol_key = (GEN)&symbol_key_struct;
const GEN integer_key = (GEN)&integer_key_struct;
const GEN bigint_key = (GEN)&bigint_key_struct;
const GEN dfloat_key = (GEN)&dfloat_key_struct;
const GEN cons_key = (GEN)&cons_key_struct;

const struct cons vm_nil_struct = {
  (GEN)&cons_key, (GEN)&vm_nil_struct, (GEN)&vm_nil_struct };

const GEN vm_nil = (GEN)&vm_nil_struct;
