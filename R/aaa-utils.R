
`%||%` <- function (x, y) if (is.null(x)) y else x


#' @export
`%<-active%` <- function(sym, value) {
  makeActiveBinding(substitute(sym), value, parent.frame())
  invisible(value)
}

#' @export
`append1<-` <- function (x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}

`append<-` <- function (x, value) {
  stopifnot(identical(mode(x), mode(value)))
  x[seq.int(from = length(x) + 1L, along.with = value)] <- value
  x
}

#' @export
`add<-` <- function (x, value) x + value

catlf <- function(...) writeLines(sprintf(...))

is_string <- function(x) is.character(x) && length(x) == 1
