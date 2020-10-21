// copied from https://github.com/hadley/pryr/commit/b396fb600c12b34c451fed7c2afc3873c2b36720
// Author : Hadley Wickham

// rlang_eval is defined here https://github.com/r-lib/rlang/blob/e764a78204216ed1c133b0d1baed9ef518e5ada7/src/eval.c
// it's the function behind eval_bare
// return_from is base around it

#include <R.h>
#include <Rinternals.h>

SEXP is_promise(SEXP name, SEXP env) {
  SEXP object = findVar(name, env);
  SEXP result;

  PROTECT(result = allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = (TYPEOF (object) == PROMSXP);
  UNPROTECT(1);

  return(result);
}

SEXP rlang_eval(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
