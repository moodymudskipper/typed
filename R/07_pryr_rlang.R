# From package pryr by Hadley Wickham
is_promise2 <- function(name, env) {
  .Call("is_promise", name, env)
}

# From package rlang by Lionel Henry
eval_bare <- function (expr, env = parent.frame()) {
  .Call(rlang_eval, expr, env)
}
