/* This program demonstrates closures in C, partial application, and
   application of closures. */

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
  char* as_string;
  Closure* as_closure;
};

union EnvVal* realloc_env(size_t numargs, union EnvVal* env, size_t newargs) {
  union EnvVal* new_env = malloc((numargs + newargs) * sizeof *new_env);
  if (numargs > 0) {
	memcpy(new_env, env, numargs * sizeof *new_env);
  }
  return new_env;
}

union EnvVal* extend_env(size_t env1_size, union EnvVal* env1, size_t env2_size, union EnvVal* env2) {
  union EnvVal* newenv = realloc_env(env1_size, env1, env2_size);
  memcpy(newenv + env1_size, env2, env2_size * sizeof *env2);
  return newenv;
}

Closure* make_closure(int arity, void (*f)(), size_t numargs, union EnvVal* env) {
  struct Closure* closure = (struct Closure*)malloc(sizeof *closure);
  closure->arity = arity;
  closure->func_ptr = f;
  closure->numargs = numargs;
  closure->env = realloc_env(numargs, env, 0);
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

Closure* extend_closure(Closure* closure, size_t env_size, union EnvVal* env) {
  struct Closure* new_closure = copy_closure(closure);
  new_closure->env = extend_env(new_closure->numargs, new_closure->env, env_size, env);
  new_closure->numargs += env_size;
  return new_closure;
}

Closure* call_closure(Closure* closure) {
  int arity = closure->arity;
  size_t numargs = closure->numargs;
  Closure* (*f)() = (Closure* (*)(union EnvVal* env, union EnvVal, union EnvVal))closure->func_ptr;
  union EnvVal* env = closure -> env;
  Closure* result;

  size_t arity_diff = (size_t)arity - numargs;
  if (arity_diff < 0) {
    /* Too many arguments, call then call again with extra arguments tacked on */
	result = f(env); // f shouldn't use extra arguments, so we just leave them on
    return call_closure(extend_closure(result, -arity_diff, env - arity_diff));
  } else if (arity_diff == 0) {
	/* Proper number of args, just call */
	return f(env);
  } else {
    /* Not enough arguments, partial application */
	return closure;
  }
}

Closure* call_closure_1(Closure* closure, union EnvVal x1) {
  return call_closure(extend_closure(closure, 1, (union EnvVal[1]){x1}));
}

Closure* call_closure_2(Closure* closure, union EnvVal x1, union EnvVal x2) {
  return call_closure(extend_closure(closure, 2, (union EnvVal[2]){x1, x2}));
}

Closure* call_closure_3(Closure* closure, union EnvVal x1, union EnvVal x2, union EnvVal x3) {
  return call_closure(extend_closure(closure, 3, (union EnvVal[3]){x1, x2, x3}));
}

void my_print(int x, int y, int z) {
  printf("my_print: x: %d, y: %d, z: %d\n", x, y, z);
}

void my_print_wrapper_1(union EnvVal* env) {
  int x = env[0].as_int;
  int y = env[1].as_int;
  int z = env[2].as_int;
  my_print(x, y, z);
}

struct Closure* make_my_print_closure_1(int x)
{
  union EnvVal xe;
  xe.as_int = x;
  union EnvVal env[] = {xe};

  return make_closure(3, &my_print_wrapper_1, 1, env);
}

void my_print_wrapper_2(union EnvVal* env) {
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
  union EnvVal env[] = {xe, ye};

  return make_closure(3, &my_print_wrapper_2, 2, env);
}

void my_other(int x, int y, double a, char* z) {
  printf("my_other: x: %d, y: %d, a: %f, z: %s\n", x, y, a, z);
}

void my_other_wrapper(union EnvVal* env) {
  int x = env[0].as_int;
  int y = env[1].as_int;
  double a = env[2].as_double;
  char* z = env[3].as_string;
  my_other(x, y, a, z);
}

struct Closure* make_my_other_closure(int x, int y, double a)
{
  union EnvVal xe, ye, ae;
  xe.as_int = x;
  ye.as_int = y;
  ae.as_double = a;
  union EnvVal env[] = {xe, ye, ae};

  return make_closure(4, &my_other_wrapper, 3, env);
}

int main() {
  /* Allocate closures */
  Closure* my_print_closure_1 = make_my_print_closure_1(100);
  Closure* my_print_closure_2 = make_my_print_closure_2(5, 1);
  Closure* my_other_closure = make_my_other_closure(-1, -2, -3.45);

  union EnvVal one_thousand, five_hundred, hello;
  five_hundred.as_int = 500;
  one_thousand.as_int = 1000;
  hello.as_string = "hello";
  call_closure_2(my_print_closure_1, five_hundred, one_thousand);
  call_closure_1(my_print_closure_2, one_thousand);
  call_closure_1(my_other_closure, hello);

  printf("\nNow we are going nested\n");
  Closure* nested_1 = call_closure_1(my_print_closure_1, five_hundred);
  call_closure_1(nested_1, one_thousand);

  union EnvVal one, two, three;
  one.as_int = 1;
  two.as_int = 2;
  three.as_double = 3.3333;
  Closure* nested_3 = call_closure_1(make_closure(4, &my_other_wrapper, 0, NULL), one);
  call_closure_3(nested_3, two, three, hello);
}
