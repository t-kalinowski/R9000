
#' @export
.DollarNames.R7 <- function(x, pattern = "") {
  out <- names.R7(x)
  if(pattern == "")
    out
  else
    grep(pattern, out, value = TRUE)
}

#' @export
names.R7 <- function(x) {
  x <- environment(x)
  out <- names(x)
  repeat {
    x <- parent.env(x)
    if(identical(x, emptyenv())) break
    out <- c(out, names(x))
  }
  out
}


new_class_spec <- function(classname, body, parent_env, inherits) {
  as.list.environment(environment())
}

copy_over_env_elements <- function(from, to, skip = "super") {
  for (nm in setdiff(names(from), skip)) {
    if (bindingIsActive(nm, from))
      makeActiveBinding(nm, activeBindingFunction(nm, from), to)
    else if (!is.null(val <- from[[nm]]))
      to[[nm]] <- val
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
    methods_env$super <- new_super(self_env)
    eval(cls$body, methods_env)

    self_env <<- new.env(parent = self_env)
    copy_over_env_elements(from = methods_env, to = self_env,
                           skip = "super")
    append1(classnames) <<-
      attr(self_env, "classname") <- cls$classname
    attr(self_env, "methods_env") <- methods_env
  }

  .grow(cls)

  # frontmost self starts empty, it's where non-private attributes set in init()
  # like self$new_foo<- will live
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

  proto_self <- .instantiate(spec)

  if (!exists("..init..", environment(proto_self), mode = "function")) {

    instantiate <- function() .instantiate(spec)

  } else {

    instantiate <- as.function(local({

      init <- get("..init..", environment(proto_self))
      init_frmls <- formals(init)
      init_call <- pass_through_call(
        quote(get("..init..", environment(self))),
        names(init_frmls)
      )

      c(init_frmls, bquote({
        self <- .instantiate(spec)
        .(init_call)
        self
      }))
    }))

  }


  # register dottr s3 methods
  local({
    this_class_methods <-
      parent.env(environment(proto_self)) # not superclasses

    for (..name.. in grep("^\\.\\..+\\.\\.$",
                          names(this_class_methods),
                          value = TRUE)) {
      name <- substr(..name.., 3L, nchar(..name..) - 2L)
      if (name %in% c("init", "call"))
        next

      generic <- get0(name, parent_env, ifnotfound = function(x) {})
      generic_frmls <- formals(args(generic) %||% function(x, ...) {})
      dispatch_sym <- names(generic_frmls)[1]

      if (dispatch_sym == "...") {
        dispatch_sym <- "x"
        generic_frmls <-  alist(x = , ... = )
      }

      dispatch_sym <- as.symbol(dispatch_sym)

      method <- this_class_methods[[..name..]]

      body <- pass_through_call(
        fn_expr = bquote(get(.(..name..), environment( .(dispatch_sym) ))),
        arg_nms = names(formals(method)))
      dispatch_frml <- alist(self = )
      names(dispatch_frml) <- names(generic_frmls)[1]
      s3_method <- as.function.default(
        c(dispatch_frml, formals(method), body),
        parent_env)

      registerS3method(name, classname, s3_method, parent_env)
    }
  })


  # - patch all proto_self$methods() to:
  #   1) take `self` as first argument
  #   2) maybe dispatch to `self$method` if self is of the appropriate class
  # - make all private attributes into active bindings that throw errors
  local({
    proto_self_env <- environment(proto_self)
    repeat {
      for (nm in names(proto_self_env)) {
        if (bindingIsActive(nm, proto_self_env)) {
          atr <- activeBindingFunction(nm, proto_self_env)
          rm(list = nm, envir = proto_self_env)
        } else {
          atr <- proto_self_env[[nm]]
        }
        if (is.function(atr)) {
          fn <- atr
          if(!exists(nm, proto_self_env, inherits = FALSE)) {
            # active binding
            arg_sym <- as.symbol(names(formals(fn))[1])
            dispatch_expr <- bquote(
              if(missing(.(arg_sym)))
                get(.(nm), super(.(classname), self), inherits = FALSE)
              else
                assign(.(nm), .(arg_sym), super(.(classname), self))
            )
          } else {
            # standard method
            dispatch_expr <- pass_through_call(
              bquote(super(.(classname), self)[[.(nm)]]),
              names(formals(fn)))
          }
          maybe_dispatch_expr <-  bquote(
            if(!missing(self) && inherits(self, .(classname)))
              return(.(dispatch_expr))
          )
          body(fn) <- bquote({
            .(maybe_dispatch_expr)
            .(body(fn))
          })
          formals(fn) <- c(alist(self = ), formals(fn))
          if(!exists(nm, proto_self_env, inherits = FALSE))
            class(fn) <- "R7_active_property_prototype"
          proto_self_env[[nm]] <- fn
        } else if (is.null(atr)) {
          msg <- sprintf(
            "private binding '%s' can can only be accessed from a class instance",
            nm)
          fn <- as.function.default(c(alist(x=), bquote(stop(.(msg))), baseenv()))
          makeActiveBinding(name, fn, proto_self_env)
        }
      }

      proto_self_env <- parent.env(proto_self_env)
      if (identical(proto_self_env, emptyenv()))
        break
    }
  })


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
`$.R7_generator` <- function(x, name) {
  get(name, environment(get("proto_self", environment(x))))
}

#' @export
`[[<-.R7` <- `$<-.R7`

#' @export
`[[.R7` <- `$.R7`

get_super_env <- function(self_env, classname, self, private) {
  if(!is.null(self))
    self_env <- environment(self)
  if (!is.null(classname)) {
    stopifnot(is_string(classname))
    repeat {
      if (identical(classname, attr(self_env, "classname")))
        break
      if (identical(self_env, emptyenv()))
        stop("Class does not inherit from ", classname)
      self_env <- parent.env(self_env)
    }
  }
  if (private)
    attr(self_env, "methods_env")
  else
    self_env
}

new_super <- function(self_env) {
  super <- eval(substitute({
    function(classname = NULL, self = NULL, ..., private = FALSE)
      get_super_env(self_env, classname, self, private, ...)
  }, list(get_super_env = get_super_env, self_env = self_env)))
  class(super) <- "R7_super"
  environment(super) <- self_env
  super
}


#' @export
`$.R7_super` <- `$.R7`



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

pass_through_call <- function(fn_expr, arg_nms) {

  if(!length(arg_nms))
    return(as.call(list(fn_expr)))

  stopifnot(is.character(arg_nms))
  args <- lapply(arg_nms, as.symbol)
  names(args) <- arg_nms
  names(args)[names(args) == "..."] <- ""

  as.call(c(list(fn_expr), args))
}
