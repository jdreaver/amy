#include <stdlib.h>
#include <string.h>
#include "gc.h"

/* Closure struct. Packs in a function with known arity and an array of
   arguments to apply (called the environment, or env). */
typedef struct Closure {
  int8_t arity;
  struct Closure* (*func_ptr)(int64_t* env);
  int8_t numargs;
  int64_t* env; // N.B. Using int64_t everywhere probably only works well on 64 bit archs
} Closure;

/* Create an empty closure with the given function pointer. */
Closure* create_closure(int8_t arity, Closure* (*f)(int64_t* env)) {
  struct Closure* closure = (struct Closure*)GC_malloc(sizeof *closure);
  closure->arity = arity;
  closure->func_ptr = f;
  closure->numargs = 0;
  closure->env = NULL;
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

/* Extends a closure's environment with the given environment. */
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
  Closure* (*f)(int64_t* env) = closure->func_ptr;
  int64_t* env = closure->env;
  Closure* result;

  int8_t arity_diff = arity - numargs;
  if (arity_diff < 0) {
    /* Too many arguments. Call, then call result with remaining arguments.

       N.B. This relies on f returning a closure. Any time a function is
       returned from another function, it must be a closure, even if it isn't
       partially applied. */

    result = f(env); // f shouldn't use extra arguments, so we just leave them on

	/* N.B. we offset the environment by the arity because that is how many
	   arguments the previous function call used. */
    return eval_closure(extend_closure(result, -arity_diff, env + arity));

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
