rm(list = ls())
library(magrittr)

# Interpreter
repl <- function() {
  env <- new.env()
  input <- ""
  while (input != "exit()") {
    input <- readline("rv_arithmetic > ")  
    if (input == "exit()") break
    run_script(input, env)
  }
  invisible(env)
}

run_script <- function(str0, env = new.env()) {
  code_lines <- str0 %>% strsplit(";") %>% unlist() %>% purrr::map_chr(clean_up)
  for (cmd in code_lines) {
    dispatch_eval(cmd, env)
  }
  invisible(env)
}

dispatch_eval <- function(cmd, env) {
  tokens <- cmd %>% strsplit(" ") %>% unlist()
  keyword <- tokens[1]
  command <- paste(tokens[-1], collapse = ' ')
  catch_err <- . %>% tryCatch(error = identity, warning = identity)
  has_err <- . %>% {any(c('error', 'warning') %in% class(.))}
  report_err <- function(res, show_value = T) {
    if (has_err(res)) {
      print(res$message)
    } else if (show_value) {
      print(res)
    }
  }
  eval_env <- function(cmd) {
    catch_err(eval(parse(text = cmd), env))
  }
  
  if (tokens[1] == "let") {
    define(command) %>% eval_env() %>% report_err(F)
  } else if (tokens[1] == "find") {
    result(command) %>% eval_env() %>% report_err(F)
  } else {
    cmd %>% eval_env() %>% report_err(T)
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


# Util
clean_up <- function(str0) {
  remove_extra_whitespace <- . %>% {gsub("  ", " ", .)}
  while (str0 != remove_extra_whitespace(str0)) {
    str0 <- remove_extra_whitespace(str0)
  }
  str0 %>% stringr::str_trim()
}


# Examples
run_script("let x be normal(0,1); let y be poisson(5); find x + y")

run_script(
  "let x be normal(0,1); 
  let y be poisson(5); 
  let z be normal(-10, 1); 
  find sin(x - y) * z"
)

run_script(
  "let theta be gamma(3, 2); 
  let y be poisson(3 + 2 * theta);
  find y"
)

run_script(
  "let p be uniform(0.6, 0.8);
  let y be binomial(1, p);
  find y"
)


repl()
