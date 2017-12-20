dispatch_eval <- function(cmd, env = globalenv()) {
  tokens <- cmd %>% strsplit(" ") %>% unlist()
  keyword <- tokens[1]
  command <- paste(tokens[-1], collapse = ' ')
  
  if (tokens[1] == "let") {
    eval(parse(text = define(command)), env)
  } else if (tokens[1] == "find") {
    eval(parse(text = result(command)), env)
  } else {
    eval(parse(text = cmd), env)  # usual R command
  }
  
  invisible(env)
}


# Compiler
synthetic_syntax <- function() {
  list(
    "be"= "<-",
    "normal[(]" = "rnorm(5000,",
    "poisson[(]" = "rpois(5000,",
    "gamma[(]" = "rgamma(5000,",
    "chi-square[(]" = "rchisq(5000,",
    "uniform[(]" = "runif(5000,",
    "binomial[(]" = "rbinom(5000,"
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

# Unit test
dispatch_eval("let x be normal(0,1)")
dispatch_eval("find x")
