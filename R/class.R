

.DollarNames.R7 <- function(x, pattern = "") {
  x <- environment(x)
  out <- names(x)
  repeat {
    x <- parent.env(x)
    if(identical(x, emptyenv())) break
    out <- c(out, names(x))
  }
  if(pattern == "")
    out
  else
    grep(pattern, out, value = TRUE)
}


new_class_spec <- function(classname, body, parent_env, inherits) {
  as.list.environment(environment())
}

copy_over_env_elements <- function(from, to, skip = "super") {
  for (nm in setdiff(names(from), skip)) {
    if (bindingIsActive(nm, from)) {
      makeActiveBinding(nm, activeBindingFunction(nm, from), to)
    } else
      to[[nm]] <- from[[nm]]
  }
}

.instantiate <- function(cls) {

  self_env <- emptyenv()
  classnames <- character()

  .grow <- function(cls) {
    if(is.null(cls))
      return()

    if(inherits(cls, "R7_generator"))
      cls <- get("spec", environment(cls))
    for(supr_cls in rev(cls$inherits))
      .grow(supr_cls)

    methods_env <- new.env(parent = cls$parent_env)
    methods_env$super <- self_env
    eval(cls$body, methods_env)

    self_env <<- new.env(parent = self_env)
    copy_over_env_elements(from = methods_env, to = self_env,
                           skip = "super")
    append1(classnames) <<-
      attr(self_env, "classname") <- cls$classname
    attr(self_env, "methods_env") <- methods_env
  }

  .grow(cls)
  self_env <- new.env(parent = self_env)

  self <- as.function.default(c(
    alist(... =),
    bquote( .(get)("..call..", envir = .(self_env))(...) )
  ), envir = self_env)

  class(self) <- unique(c(rev(classnames), "R7"))

  local({
    repeat {
      methods_env <- attr(self_env, "methods_env")
      methods_env$self <- self
      self_env <- parent.env(self_env)
      if(identical(self_env, emptyenv()))
        break
    }
  })

  self
}


#' @export
`%class%` <- function(spec, body) {
  spec <- substitute(spec)
  body <- substitute(body)
  parent_env <- parent.frame()

  if (is.call(spec)) {
    classname <- spec[[1L]]
    stopifnot(is.symbol(classname))
    classname <- as.character(classname)
    spec[[1]] <- list
    inherits <- eval(spec, parent_env)

  } else if (is.symbol(spec)) {
    classname <- as.character(spec)
    inherits <- NULL

  } else
    stop("invalid spec")

  spec <- new_class_spec(classname, body, parent_env, inherits)

  instantiate <- function(...) {
    self <- .instantiate(spec)
    init <- get0("..init..", environment(self), mode = "function",
                 ifnotfound = function() {})
    init(...)
    self
  }
  class(instantiate) <- c(paste0(classname, "_generator"),
                          "R7_generator")
  assign(classname, instantiate, envir = parent_env)
  invisible(instantiate)
}

#' @export
`$.R7` <- function(x, name) {
  get0(name, environment(x))
}


#' @export
`$<-.R7` <- function(x, name, value) {
  self_env <- environment(x)
  assign(name, value, envir = self_env,
         inherits = exists(name, envir = self_env))
  x
}

#' @export
`[[<-.R7` <- `$<-.R7`

#' @export
`[[.R7` <- `$.R7`



#' @export
dottr_s3_dispatcher <- function(name) {
  ..name.. <- sprintf("..%s..", name)
  fn <- get(name, mode = "function")
  frmls <- formals(args(fn) %||% function(...) {})

  x_sym <- as.symbol(names(frmls)[[1]])
  if(identical(x_sym, quote(...)))
    x_sym <- quote(..1)

  args <- names(frmls)[-1]
  names(args) <- args
  args <- lapply(args, as.symbol)
  if("..." %in% names(args))
    args[["..."]] <- quote(expr = )

  body <- bquote({
    if (exists(.(..name..), envir = environment(.(x_sym))))
      get(.(..name..), envir = environment(.(x_sym)))(..(args))
    else
      NextMethod()
  }, splice = TRUE)
  as.function(c(frmls, body), parent.frame())
}


#' @export
print.R7 <- function(x, ...) {
  catlf("class instance of type: <%s>",
        paste0(class(x), collapse = ", "))
  x <- environment(x)
  env_name <- "self"
  repeat {
    catlf("<%s>:", env_name)
    xl <- as.list.environment(x, all.names = TRUE, sorted = TRUE)
    active <- vapply(names(xl),
                     function(nm) bindingIsActive(nm, x),
                     FALSE)
    if(any(active))
      names(xl)[active] <- paste(names(xl)[active], "(active)")

    str(xl,
        no.list = TRUE, max.level = 1,
        drop.deparse.attr = TRUE,
        give.attr = FALSE)
    x <- parent.env(x)
    if(identical(x, emptyenv()))
      break
    env_name <- attr(x, "classname")
  }
}
