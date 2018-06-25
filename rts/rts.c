#include <stdlib.h>
#include <string.h>
#include "gc.h"

/* Generic closure struct. The function pointer and environment are both void
 * because they are meant to be casted to the correct types. */
typedef struct Closure {
  int8_t arity;
  void (*func_ptr)();
  int8_t numargs;
  int64_t* env; // N.B. Using int64_t everywhere probably only works well on 64 bit archs
} Closure;

/* Create a closure. NOTE: env needs to be heap allocated, not stack
   allocated. */
Closure* create_closure(int8_t arity, void (*f)(), int8_t numargs, int64_t* env) {
  struct Closure* closure = (struct Closure*)GC_malloc(sizeof *closure);
  closure->arity = arity;
  closure->func_ptr = f;
  closure->numargs = numargs;
  closure->env = env;
  return closure;
}

/* Extends an environment by concatenating the new environment to the end of
   the old one. */
int64_t* extend_env(int8_t env1_size, int64_t* env1, int8_t env2_size, int64_t* env2) {
  int64_t* newenv = GC_malloc((size_t)(env1_size + env2_size) * sizeof *newenv);
  if (env1_size > 0) {
	memcpy(newenv, env1, (size_t)env1_size * sizeof *newenv);
  }
  memcpy(newenv + env1_size, env2, (size_t)env2_size * sizeof *env2);
  return newenv;
}

Closure* copy_closure(Closure* closure) {
  struct Closure* new_closure = (struct Closure*)GC_malloc(sizeof *closure);
  new_closure->arity = closure->arity;
  new_closure->func_ptr = closure->func_ptr;
  new_closure->numargs = closure->numargs;
  new_closure->env = closure->env;
  return new_closure;
}

Closure* extend_closure(Closure* closure, int8_t env_size, int64_t* env) {
  struct Closure* new_closure = copy_closure(closure);
  new_closure->env = extend_env(new_closure->numargs, new_closure->env, env_size, env);
  new_closure->numargs += env_size;
  return new_closure;
}

/* Recursive function to evaluate a closure and return the result.

   This function uses the eval/apply model of closure application. The closure
   function is first inspected to get its arity and the number of arguments
   applied to it. Then we consider 3 cases: too many arguments, exactly the
   right amount of arguments, and not enough arguments.

   This function has a return type of Closure* so we don't do a bunch of casts
   here, but in reality the final result of a closure application could be any
   arbitrary result if called with the correct number of arguments.
   */
Closure* eval_closure(Closure* closure) {
  int8_t arity = closure->arity;
  int8_t numargs = closure->numargs;
  Closure* (*f)() = (Closure* (*)(int64_t* env, int64_t, int64_t))closure->func_ptr;
  int64_t* env = closure -> env;
  Closure* result;

  int8_t arity_diff = arity - numargs;
  if (arity_diff < 0) {
    /* Too many arguments, call then call again with extra arguments tacked on.

	   N.B.: This relies on f returning a closure. Any time a function is
	   returned from another function, it must be a closure, even if it isn't
	   partially applied. */
	result = f(env); // f shouldn't use extra arguments, so we just leave them on
    return eval_closure(extend_closure(result, -arity_diff, env - arity_diff));
  } else if (arity_diff == 0) {
	/* Proper number of args, just call the function and return the result. */
	return f(env);
  } else {
    /* Not enough arguments. This is a partial application, and we just return
	   the result. */
	return closure;
  }
}

/* Calls a closure with the given array of arguments.

   N.B. We can probably pre-generate lots of tiny call_closure_1,
   call_closure_2 etc. functions with the exact number of arguments to make
   the caller's life easier. */
Closure* call_closure(Closure* closure, int8_t numargs, int64_t* args) {
  return eval_closure(extend_closure(closure, numargs, args));
}
