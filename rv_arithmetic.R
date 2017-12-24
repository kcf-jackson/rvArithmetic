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
  code_lines <- str0 %>% codesplit()
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

codesplit <- function(str0) {
  res <- c()
  buffer <- NULL
  open <- F
  codelines <- str0 %>% strsplit(";") %>% unlist()
  for (i in 1:length(codelines)) {
    line <- codelines[i]
    contain_open <- grepl("[{]", line)
    contain_close <- grepl("[}]", line)
    if (contain_open) open <- T
    
    if (open) {
      buffer <- c(buffer, line)
    } else {
      res <- c(res, line)
    }
    
    if (contain_close) {
      open <- F
      res <- c(res, paste(buffer, collapse = ';'))
      buffer <- NULL
    }
  }
  res %>% purrr::map_chr(clean_up)
}


# Compiler
synthetic_syntax <- function() {
  list(
    "let" = "",
    "be" = "<-",
    "normal[(]" = "rnorm(5000,",
    "poisson[(]" = "rpois(5000,",
    "gamma[(]" = "rgamma(5000,",
    "chi-square[(]" = "rchisq(5000,",
    "uniform[(]" = "runif(5000,",
    "binomial[(]" = "rbinom(5000,",
    "model" = "function"
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

normalise <- function(vec0) {vec0 / sum(vec0)}

l2_norm <- function(x, y) {sum((x - y)^2)}

metric <- function(x, y, summary_fun, distance_fun = l2_norm) {
  summary_fun %>% purrr::map_dbl(~distance_fun(.x(x), .x(y))) %>% sum()
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

run_script(
  "let flip be model(p) {
    let y be binomial(1, p);
    return(y);
  };
  find flip(0.4)"
)

# repl()

abc <- function(data0, fun, epsilon = 1e-6, iter = 1000, 
                summary_fun = c(mean, var), distance_fun = l2_norm) {
  eval_fun <- . %>% { do.call(fun, as.list(.)) }
  perf_fun <- function(x, y) { metric(x, y, summary_fun, distance_fun) }
  has_warning_or_error <- function(x) {
    any(c('warning', 'error') %in% class(x))
  }
  param_dim <- length(formalArgs(fun))
  
  count <- 0
  p <- runif(param_dim)
  current_distance <- perf_fun(eval_fun(p), data0)
  while ((current_distance > epsilon) && (count < iter)) {
    count <- count + 1
    new_p <- p + rnorm(param_dim, sd = 0.1)
    new_distance <- tryCatch(
      perf_fun(eval_fun(new_p), data0),
      error = identity, warning = identity
    )
    if (!has_warning_or_error(new_distance)) {
      if (new_distance < current_distance) {
        p <- new_p
        current_distance <- new_distance
        # cat("Improved!", p, current_distance, "\n")
      }
    }
  }
  list(param = p, distance_from_data = current_distance, iter = count)
}
