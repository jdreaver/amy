#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Generic closure struct. The function pointer and environment are both void
 * because they are meant to be casted to the correct types. */
typedef struct Closure {
  int arity;
  void (*func_ptr)();
  size_t numargs;
  union EnvVal* env;
} Closure;

/* All values are casted to this union so environments are generic. */
union EnvVal {
  int as_int;
  double as_double;
  Closure* as_closure;
};

union EnvVal* realloc_env(size_t numargs, union EnvVal* env, size_t newargs) {
  union EnvVal* new_env = malloc((numargs + newargs) * sizeof *new_env);
  if (numargs > 0) {
	memcpy(new_env, env, numargs * sizeof *new_env);
  }
  return new_env;
}

union EnvVal* extend_env_1(size_t numargs, union EnvVal* env, union EnvVal x1) {
  union EnvVal* newenv = realloc_env(numargs, env, 1);
  newenv[numargs] = x1;
  return newenv;
}

union EnvVal* extend_env_2(size_t numargs, union EnvVal* env, union EnvVal x1, union EnvVal x2) {
  union EnvVal* newenv = realloc_env(numargs, env, 2);
  newenv[numargs] = x1;
  newenv[numargs + 1] = x2;
  return newenv;
}

union EnvVal* extend_env_3(size_t numargs, union EnvVal* env, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  union EnvVal* newenv = realloc_env(numargs, env, 3);
  newenv[numargs] = x1;
  newenv[numargs + 1] = x2;
  newenv[numargs + 2] = x3;
  return newenv;
}

Closure* make_empty_closure(int arity, void (*f)()) {
  struct Closure* closure = (struct Closure*)malloc(sizeof *closure);
  closure->arity = arity;
  closure->func_ptr = f;
  closure->numargs = 0;
  closure->env = NULL;
  return closure;
}

Closure* copy_closure(Closure* closure) {
  struct Closure* new_closure = (struct Closure*)malloc(sizeof *closure);
  new_closure->arity = closure->arity;
  new_closure->func_ptr = closure->func_ptr;
  new_closure->numargs = closure->numargs;
  new_closure->env = closure->env;
  return new_closure;
}

Closure* extend_closure_1(Closure* closure, union EnvVal x1) {
  struct Closure* new_closure = copy_closure(closure);
  new_closure->env = extend_env_1(new_closure->numargs, new_closure->env, x1);
  new_closure->numargs += 1;
  return new_closure;
}

Closure* extend_closure_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  struct Closure* new_closure = copy_closure(closure);
  new_closure->env = extend_env_2(new_closure->numargs, new_closure->env, x1, x2);
  new_closure->numargs += 2;
  return new_closure;
}

Closure* extend_closure_3(Closure* closure, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  struct Closure* new_closure = copy_closure(closure);
  new_closure->env = extend_env_3(new_closure->numargs, new_closure->env, x1, x2, x3);
  new_closure->numargs += 3;
  return new_closure;
}

Closure* apply_closure_1(Closure* closure, union EnvVal x1) {
  Closure* (*f)() = (Closure* (*)(union EnvVal* env))closure->func_ptr;
  union EnvVal* env = extend_env_1(closure->numargs, closure->env, x1);
  printf("apply_closure_1, x1: %d\n", x1.as_int);
  return f(env);
}

Closure* apply_closure_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  Closure* (*f)() = (Closure* (*)(union EnvVal* env, union EnvVal, union EnvVal))closure->func_ptr;
  union EnvVal* env = extend_env_2(closure->numargs, closure->env, x1, x2);
  printf("apply_closure_2, x1: %d, x2: %d\n", x1.as_int, x2.as_int);
  return f(env);
}

Closure* apply_closure_3(Closure* closure, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  printf("apply_closure_3\n");
  Closure* (*f)() = (Closure* (*)(union EnvVal* env, union EnvVal, union EnvVal))closure->func_ptr;
  union EnvVal* env = extend_env_3(closure->numargs, closure->env, x1, x2, x3);
  return f(env);
}

Closure* call_closure_1(Closure* closure, union EnvVal x1) {
  // Closure* result;
  int arity = closure->arity;
  size_t numargs = closure->numargs;

  printf("call_closure_1, arity: %d, numargs: %zu, x1: %d\n", arity, numargs, x1.as_int);

  switch (arity - numargs) {
    case 1:
      /* Proper number of args, just call */
      return apply_closure_1(closure, x1);

    default:
      /* Not enough arguments, partial application */
      return extend_closure_1(closure, x1);
  }
}

Closure* call_closure_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  Closure* result;
  int arity = closure->arity;
  size_t numargs = closure->numargs;

  printf("call_closure_2, arity: %d, numargs: %zu, x1: %d, x2: %d\n", arity, numargs, x1.as_int, x2.as_int);

  switch (arity - numargs) {
    case 1:
      /* Too many arguments, call then call again */
      result = apply_closure_1(closure, x1);
      return call_closure_1(result, x2);

    case 2:
      /* Proper number of args, just call */
      return apply_closure_2(closure, x1, x2);

    default:
      /* Not enough arguments, partial application */
      return extend_closure_2(closure, x1, x2);
  }
}

Closure* call_closure_3(Closure* closure, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  Closure* result;
  int arity = closure->arity;
  size_t numargs = closure->numargs;

  printf("call_closure_3, arity: %d, numargs: %zu, x1: %d, x2: %d, x3: %d\n", arity, numargs, x1.as_int, x2.as_int, x3.as_int);

  switch (arity - numargs) {
    case 1:
      /* Too many arguments, call then call again */
      result = apply_closure_1(closure, x1);
      return call_closure_2(result, x2, x3);

    case 2:
      /* Too many arguments, call then call again */
      result = apply_closure_2(closure, x1, x2);
      return call_closure_1(result, x3);

    case 3:
      /* Proper number of args, just call */
      return apply_closure_3(closure, x1, x2, x3);

    default:
      /* Not enough arguments, partial application */
      return extend_closure_3(closure, x1, x2, x3);
  }
}

void my_print(int x, int y, int z) {
  printf("my_print: x: %d, y: %d, z: %d\n", x, y, z);
}

void my_print_wrapper_1(union EnvVal* env) {
  printf("my_print_wrapper_1\n");
  int x = env[0].as_int;
  int y = env[1].as_int;
  int z = env[2].as_int;
  my_print(x, y, z);
}

struct Closure* make_my_print_closure_1(int x)
{
  union EnvVal xe;
  xe.as_int = x;

  return extend_closure_1(make_empty_closure(3, &my_print_wrapper_1), xe);
}

void my_print_wrapper_2(union EnvVal* env) {
  printf("my_print_wrapper_2\n");
  int x = env[0].as_int;
  int y = env[1].as_int;
  int z = env[2].as_int;
  my_print(x, y, z);
}

struct Closure* make_my_print_closure_2(int x, int y)
{
  union EnvVal xe, ye;
  xe.as_int = x;
  ye.as_int = y;

  return extend_closure_2(make_empty_closure(3, &my_print_wrapper_2), xe, ye);
}

void my_other(int x, int y, double a, int z) {
  printf("my_other: x: %d, y: %d, a: %f, z: %d\n", x, y, a, z);
}

void my_other_wrapper(union EnvVal* env) {
  printf("my_other_wrapper_1\n");
  int x = env[0].as_int;
  int y = env[1].as_int;
  double a = env[2].as_double;
  int z = env[3].as_int;
  my_other(x, y, a, z);
}

struct Closure* make_my_other_closure(int x, int y, double a)
{
  union EnvVal xe, ye, ae;
  xe.as_int = x;
  ye.as_int = y;
  ae.as_double = a;

  return extend_closure_3(make_empty_closure(4, &my_other_wrapper), xe, ye, ae);
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
  Closure* nested_1 = call_closure_1(my_print_closure_1, five_hundred);
  call_closure_1(nested_1, one_thousand);

  union EnvVal one, two, three, four;
  one.as_int = 1;
  two.as_int = 2;
  three.as_double = 3.3333;
  four.as_int = 4;
  Closure* nested_3 = extend_closure_1(make_empty_closure(4, &my_other_wrapper), one);
  call_closure_3(nested_3, two, three, four);
}
