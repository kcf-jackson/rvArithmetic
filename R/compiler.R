synthetic_syntax <- function() {
  list(
    "let " = " ",
    "be " = "<-",
    "beta[(]" = "rbeta(5000,",
    "binomial[(]" = "rbinom(5000,",
    "cauchy[(]" = "rcauchy(5000,",
    "chi-square[(]" = "rchisq(5000,",
    "exponential[(]" = "rexp(5000,",
    "f[(]" = "rf(5000,",
    "gamma[(]" = "rgamma(5000,",
    "geometric[(]" = "rgeom(5000,",
    "hypergeometric[(]" = "rhyper(5000,",
    "lognormal[(]" = "rlnorm(5000,",
    "logistic[(]" = "rlogis(5000,",
    "normal[(]" = "rnorm(5000,",
    "poisson[(]" = "rpois(5000,",
    "student[(]" = "rt(5000,",
    "uniform[(]" = "runif(5000,",
    "weibull[(]" = "rweibull(5000,",
    "model[(]" = "function("
  )
}


define <- function(str0) {
  syntax_list <- synthetic_syntax()
  for (i in seq_along(syntax_list)) {
    find <- names(syntax_list)[i]
    replace <- syntax_list[i]
    str0 <- gsub(find, replace, str0)
  }
  str0
}


result <- function(str0) {
  sprintf("hist(%s, 30, prob = T)", str0)
}


infer <- function(str0) {
  cmd <- str0 %>% strsplit(" ") %>% unlist()
  sprintf("abc(%s, %s)", cmd[3], cmd[1])
}
