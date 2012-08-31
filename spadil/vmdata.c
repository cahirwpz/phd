#include "vmdata.h"

const struct type_key key_key_struct = {
  (KEY)&key_key_struct, "key" };

const GEN key_key = (GEN)&key_key_struct;

const struct type_key vector_1d_key_struct = {
  (KEY)&key_key_struct, "vector_1d" };

const GEN vector_1d_key = (GEN)&vector_1d_key_struct;

const struct type_key string_key_struct = {
  (KEY)&key_key_struct, "string" };

const GEN string_key = (GEN)&string_key_struct;

const struct type_key symbol_key_struct = {
  (KEY)&key_key_struct, "symbol" };

const GEN symbol_key = (GEN)&symbol_key_struct;

const struct type_key bigint_key_struct = {
  (KEY)&key_key_struct, "bigint" };

const GEN bigint_key = (GEN)&bigint_key_struct;

const struct type_key defloat_key_struct = {
  (KEY)&key_key_struct, "defloat" };

const GEN defloat_key = (GEN)&defloat_key_struct;

const struct cons vm_nil_struct = {
  (GEN)&vm_nil_struct, (GEN)&vm_nil_struct };

const GEN vm_nil = (GEN)&vm_nil_struct;
