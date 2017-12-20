# Compiler
synthetic_syntax <- function() {
  list(
    "is"= "<-",
    "normal[(]" = "rnorm(5000,",
    "poisson[(]" = "rpois(5000,"
  )
}

define <- function(str0) {
  syntax_list <- synthetic_syntax()
  for (i in seq_along(syntax_list)) {
    str0 <- gsub(names(syntax_list)[i], syntax_list[i], str0)
  }
  str0
}

# Unit test
define("x is normal(0,1)")
