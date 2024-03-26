lazy <- function(env, expr) {
  eval(bquote(delayedAssign("x", .(substitute(expr)), assign.env = env)))
}
