// copied from https://github.com/hadley/pryr/commit/b396fb600c12b34c451fed7c2afc3873c2b36720
// Author : Hadley Wickham

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
