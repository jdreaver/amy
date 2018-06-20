#include <stdio.h>
#include <stdlib.h>

/* Generic closure struct. The function pointer and environment are both void
 * because they are meant to be casted to the correct types. */
typedef struct Closure {
  int arity;
  void (*func_ptr)();
  union EnvVal* env;
} Closure;

/* All values are casted to this union so environments are generic. */
union EnvVal {
  int as_int;
  double as_double;
  Closure* as_closure;
};

Closure* apply_pap_1(Closure* closure, union EnvVal x1) {
  Closure* (*f1)() = (Closure* (*)(union EnvVal* env, union EnvVal))closure->func_ptr;
  union EnvVal* env = closure->env;
  printf("apply_pap_1, f1: %d, env:, %d, x1: %d\n", (int)f1, (int)env, x1.as_int);
  return f1(env, x1);
}

/* TODO: When we make a closure from an existing closure, we have to properly
   handle the environment! Maybe extend EnvVal to allow Closure*, and then
   make the original closure the first argument. */
Closure* make_pap_1(int original_arity, void (*f)(), union EnvVal x1) {
  printf("make_pap_1, x1: %d\n", x1.as_int);
  union EnvVal* env = (union EnvVal*)malloc(sizeof(union EnvVal) * 1);
  env[0] = x1;

  struct Closure* closure = (struct Closure*)malloc(sizeof(struct Closure*));
  closure->arity = original_arity - 1;
  closure->func_ptr = f;
  closure->env = env;

  return closure;
}

Closure* apply_pap_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  Closure* (*f2)() = (Closure* (*)(union EnvVal* env, union EnvVal, union EnvVal))closure->func_ptr;
  union EnvVal* env = closure->env;
  printf("apply_pap_2, f2: %d, env:, %d, x1: %d, x2: %d\n", (int)f2, (int)env, x1.as_int, x2.as_int);
  return f2(env, x1, x2);
}

Closure* make_pap_2(int original_arity, void (*f)(), union EnvVal x1, union EnvVal x2) {
  printf("make_pap_2, x1: %d, x2: %d\n", x1.as_int, x2.as_int);
  union EnvVal* env = (union EnvVal*)malloc(sizeof(union EnvVal) * 2);
  env[0] = x1;
  env[1] = x2;

  struct Closure* closure = (struct Closure*)malloc(sizeof(struct Closure*));
  closure->arity = original_arity - 2;
  closure->func_ptr = f;
  closure->env = env;

  return closure;
}

Closure* apply_pap_3(Closure* closure, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  printf("apply_pap_3\n");
  Closure* (*f3)() = (Closure* (*)(union EnvVal* env, union EnvVal, union EnvVal))closure->func_ptr;
  union EnvVal* env = closure->env;
  return f3(env, x1, x2, x3);
}

Closure* make_pap_3(int original_arity, void (*f)(), union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  printf("make_pap_3\n");
  union EnvVal* env = (union EnvVal*)malloc(sizeof(union EnvVal) * 3);
  env[0] = x1;
  env[1] = x2;
  env[2] = x3;

  struct Closure* closure = (struct Closure*)malloc(sizeof(struct Closure*));
  closure->arity = original_arity - 3;
  closure->func_ptr = f;
  closure->env = env;

  return closure;
}

Closure* unpack_pap_1(Closure* closure, union EnvVal x1) {
  union EnvVal* env = closure->env;
  return apply_pap_2(env[0].as_closure, env[1], x1);
}

void* select_apply_pap_function(int arity) {
  switch (arity) {
    case 1: return &apply_pap_1;
    case 2: return &apply_pap_2;
	case 3: return &apply_pap_3;
	default:
	  printf("Invalid arity! %d\n", arity);
	  return 0;
  }
}

Closure* call_closure_1(Closure* closure, union EnvVal x1) {
  // Closure* result;
  int arity = closure->arity;
  union EnvVal closure_env_val;

  printf("Inside call_closure_1. arity: %d, x1: %d\n", arity, x1.as_int);

  switch (arity) {
    case 1:
      /* Proper number of args, just call */
      return apply_pap_1(closure, x1);

    default:
      /* Not enough arguments, partial application */
      closure_env_val.as_closure = closure;
      return make_pap_2(arity + 1, (void (*)())select_apply_pap_function(arity - 1), closure_env_val, x1);
  }
}

Closure* call_closure_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  Closure* result;
  int arity = closure->arity;
  union EnvVal closure_env_val;

  printf("Inside call_closure_2. arity: %d, x1: %d, x2: %d\n", arity, x1.as_int, x2.as_int);

  switch (arity) {
    case 1:
      /* Too many arguments, call then call again */
      result = apply_pap_1(closure, x1);
      return call_closure_1(result, x2);

    case 2:
      /* Proper number of args, just call */
      return apply_pap_2(closure, x1, x2);

    default:
      /* Not enough arguments, partial application */
      closure_env_val.as_closure = closure;
      return make_pap_3(arity + 1, (void (*)())select_apply_pap_function(arity - 2), closure_env_val, x1, x2);
  }
}

void my_print(int x, int y, int z) {
  printf("my_print: x: %d, y: %d, z: %d\n", x, y, z);
}

void my_print_wrapper_1(union EnvVal* env, int y, int z) {
  printf("my_print_wrapper_1\n");
  int x = env[0].as_int;
  my_print(x, y, z);
}

struct Closure* make_my_print_closure_1(int x)
{
  union EnvVal xe;
  xe.as_int = x;

  return make_pap_1(3, &my_print_wrapper_1, xe);
}

void my_print_wrapper_2(union EnvVal* env, int z) {
  printf("my_print_wrapper_2\n");
  int x = env[0].as_int;
  int y = env[1].as_int;
  my_print(x, y, z);
}

struct Closure* make_my_print_closure_2(int x, int y)
{
  union EnvVal xe, ye;
  xe.as_int = x;
  ye.as_int = y;

  return make_pap_2(3, &my_print_wrapper_2, xe, ye);
}

void my_other(int x, int y, double a, int z) {
  printf("my_other: x: %d, y: %d, a: %f, z: %d\n", x, y, a, z);
}

void my_other_wrapper(union EnvVal* env, int z) {
  printf("my_other_wrapper_1\n");
  int x = env[0].as_int;
  int y = env[1].as_int;
  double a = env[2].as_double;
  my_other(x, y, a, z);
}

struct Closure* make_my_other_closure(int x, int y, double a)
{
  union EnvVal xe, ye, ae;
  xe.as_int = x;
  ye.as_int = y;
  ae.as_double = a;

  return make_pap_3(4, &my_other_wrapper, xe, ye, ae);
}

int main() {
  /* Allocate closures */
  Closure* my_print_closure_1 = make_my_print_closure_1(100);
  Closure* my_print_closure_2 = make_my_print_closure_2(5, 1);
  Closure* my_other_closure = make_my_other_closure(-1, -2, -3.45);

  union EnvVal one_thousand, five_hundred;
  five_hundred.as_int = 500;
  one_thousand.as_int = 1000;
  call_closure_2(my_print_closure_1, five_hundred, one_thousand);
  call_closure_1(my_print_closure_2, one_thousand);
  call_closure_1(my_other_closure, one_thousand);

  printf("\nNow we are going nested\n");
  Closure* nested = call_closure_1(my_print_closure_1, five_hundred);
  call_closure_1(nested, one_thousand);
}
