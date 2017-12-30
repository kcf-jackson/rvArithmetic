#' REPL for the language
#' @export
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


#' Run script
#' @param str0 string; the script to be run.
#' @param env The environment.
#' @export
run_script <- function(str0, env = new.env()) {
  code_lines <- str0 %>% codesplit()
  for (cmd in code_lines) {
    dispatch_eval(cmd, env)
  }
  invisible(env)
}


#' Dispatch evaluation function based on keywords
#' @keywords internal
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
  } else if (tokens[1] == "infer") {
    infer(command) %>% eval_env() %>% report_err(T)
  } else {
    cmd %>% eval_env() %>% report_err(T)
  }

  invisible(env)
}


#' Correctly parse parenthesis
#' @keywords internal
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
