#include <stdio.h>
#include <stdlib.h>

/* Generic closure struct. The function pointer and environment are both void
 * because they are meant to be casted to the correct types. */
typedef struct Closure {
  void (*func_ptr)();
  void* env;
} Closure;

/* my_print function and its closure environment MyPrintEnv */
typedef struct MyPrintEnv {
  int x;
  int y;
} MyPrintEnv;

void my_print(int z, MyPrintEnv* env) {
  int x = env->x;
  int y = env->y;
  printf("my_print: x: %d, y: %d, z: %d\n", x, y, z);
}

struct Closure make_my_print_closure(int x, int y)
{
  struct MyPrintEnv env;
  env.x = x;
  env.y = y;

  struct Closure closure;
  closure.env = &env;
  closure.func_ptr = &my_print;

  return closure;
}

/* my_other function and its closure environment MyOtherEnv */
typedef struct MyOtherEnv {
  int x;
  int y;
  int a;
} MyOtherEnv;

void my_other(int z, MyOtherEnv* env) {
  int x = env->x;
  int y = env->y;
  printf("my_other: x: %d, y: %d, z: %d\n", x, y, z);
}

struct Closure make_my_other_closure(int x, int y, int a)
{
  struct MyOtherEnv env;
  env.x = x;
  env.y = y;
  env.a = a;

  struct Closure closure;
  closure.env = &env;
  closure.func_ptr = &my_other;

  return closure;
}

/* Function that doesn't accept a closure, but we will nevertheless pretend it
 * does. */
void print_no_closure(int z) {
  printf("print_no_closure: z: %d\n", z);
}

/* Function to call a generic function. */
void call_closure(void (*func)(int, void*), void* env, int z) {
  printf("Inside call_closure\n");
  func(z, env);
}

int main() {
	/* Allocate closures */
  struct Closure my_print_closure = make_my_print_closure(5, 1);
  struct Closure my_other_closure = make_my_other_closure(-1, -2, -3);

  /* Call a function that accepts closures with the closures. */
  call_closure(my_print_closure.func_ptr, my_print_closure.env, 500);
  call_closure(my_other_closure.func_ptr, my_other_closure.env, 1000);
  /* Note that print_no_closure doesn't accept a closure, so we just pass a
   * void pointer. */
  call_closure((void (*)(int, void *))&print_no_closure, (void*)0, 2000);
}
