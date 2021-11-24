
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R9000

<!-- badges: start -->
<!-- badges: end -->

R9000 is a playground where I explored and sketched out some ideas for
OOP in R.

## Installation

You can install the development version of R9000 like so:

``` r
remotes::install_github("t-kalinowski/R9000")
```

## Example

There is really only one interesting exported function: `%class%`. This
is what it looks like:

``` r
library(R9000)

Stopwatch %class% {
  
  total_previous_laps <- NULL # private attribute
  
  ..init.. <- function(name = "", start = FALSE) {
    self$name <- name # set public attributes
    self$start_time <- 
    self$stop_time <- .POSIXct(NA_real_)
    self$running <- FALSE
    total_previous_laps <<- lap_time
    if (start) 
      self()
  }

  ..call.. <- function() {
    # self() toggles start/stop
    if (self$running) {
      self$stop_time <- Sys.time()
      add(total_previous_laps) <<- self$stop_time - self$start_time
    } else {
      self$start_time <- Sys.time()
    }

    self$running <- !self$running
  }

  lap_time %<-active% function(x) {
    if (!missing(x)) stop("Protected property")
    
    if (self$running) Sys.time() - self$start_time
    else if (is.na(self$start_time)) 0
    else self$stop_time - self$start_time
  }

   total_time %<-active% function(x) {
    if (!missing(x)) stop("Protected property")
     
    if (self$running) total_previous_laps + lap_time
    else total_previous_laps
  }

  ..format.. <- function(..., digits = 3) {
    sprintf(
      "Stopwatch: name = '%s', lap = %s, total = %s, %s",
      self$name,
      format(self$lap_time, digits = digits, ...),
      format(self$total_time, digits = digits, ...),
      if (self$running) "running" else "stopped")
  }

  reset <- ..init..
}
```

Calling `%class%` binds a class generator in the frame. The class
generator has an identical signature to `..init..()`, if it is defined.
Instantiate a class instance by calling the generator.

``` r
str(formals(Stopwatch))
#> Dotted pair list of 2
#>  $ name : chr ""
#>  $ start: logi FALSE
watch <- Stopwatch("Nap time")
```

The `print` method for class instances shows all resolvable objects from
`self$*`.

``` r
watch
#> class instance of type: <Stopwatch, R7>
#> <self>:
#>  $ name      : chr "Nap time"
#>  $ running   : logi FALSE
#>  $ start_time: POSIXct[1:1], format: NA
#>  $ stop_time : POSIXct[1:1], format: NA
#> <Stopwatch>:
#>  $ ..call..           :function ()  
#>  $ ..format..         :function (..., digits = 3)  
#>  $ ..init..           :function (name = "", start = FALSE)  
#>  $ lap_time (active)  : num 0
#>  $ reset              :function (name = "", start = FALSE)  
#>  $ total_time (active): num 0
```

Invoke double dotted methods like `..format..()` via S3.

``` r
cat(format(watch))
#> Stopwatch: name = 'Nap time', lap = 0, total = 0, stopped

identical(format(watch), watch$..format..())
#> [1] TRUE
```

A call of `self()` invokes `self$..call..()`.

``` r
watch()
cat(format(watch))
#> Stopwatch: name = 'Nap time', lap = 0.000353 secs, total = 0.000605 secs, running
```

Active binding as properties.

``` r
watch$lap_time
#> Time difference of 0.002830982 secs
Sys.sleep(1)
watch$lap_time
#> Time difference of 1.009485 secs
try(watch$lap_time <- NA)
#> Error in (function (x)  : Protected property
```

What works?

-   Single inheritance
-   Multiple inheritance / mixin subclasses (object resolution order is
    flattened and fixed at instantiation time)
-   Instances are callable if a `..call..` method is defined.
-   Active bindings are supported.
-   Private attributes are supported.
-   Public attributes (callable or otherwise) autocomplete after `$`
    (via `.DollarNames()` and `names()` methods)
-   Double-dotted (“dottr”) methods are auto registered as S3 methods.
-   `self` and `super` are in scope of all methods. Both are callable
    and accessible with `$`.
-   `self` is available at initialization time.
-   `super` is available always, even at methods construction time.
-   `super("a_classname")` resolves a specific superclasses public
    environment.
-   `self(...)` invokes `self$..call..(...)`.
-   The instance generator has the signature of `..init..()` if it is
    defined.
-   Instance generator has `$` method that lets you access methods
    directly, bypassing instantiation.

Inheritance example:

``` r
Class1 %class% {
  a_method <- function() {
    writeLines("Called Class1$a_method")
  }
}

Class2(Class1) %class% {
  a_method <- function() {
    writeLines("Entering Class2$a_method")
    super$a_method()
    writeLines("Exiting Class2$a_method")
  }
}

Class3(Class2) %class% {
  a_method <- function() {
    writeLines("Entering Class3$a_method")
    super$a_method()
    writeLines("Exiting Class3$a_method")
  }
}

x <- Class3()

class(x)
#> [1] "Class3" "Class2" "Class1" "R7"

x$a_method()
#> Entering Class3$a_method
#> Entering Class2$a_method
#> Called Class1$a_method
#> Exiting Class2$a_method
#> Exiting Class3$a_method

x
#> class instance of type: <Class3, Class2, Class1, R7>
#> <self>:
#>  list()
#> <Class3>:
#>  $ a_method:function ()  
#> <Class2>:
#>  $ a_method:function ()  
#> <Class1>:
#>  $ a_method:function ()
```

Call `super()` to resolve a specific super class’s environment.

``` r
Skipper(Class3) %class% {
  a_method <- function(to_classname = NULL) {
    writeLines("Entering Skipper$a_method")
    super(to_classname)$a_method()
    writeLines("Exiting Skipper$a_method")
  }
}

x <- Skipper()
x$a_method()
#> Entering Skipper$a_method
#> Entering Class3$a_method
#> Entering Class2$a_method
#> Called Class1$a_method
#> Exiting Class2$a_method
#> Exiting Class3$a_method
#> Exiting Skipper$a_method

x$a_method("Class3") # equivalent to default of NULL
#> Entering Skipper$a_method
#> Entering Class3$a_method
#> Entering Class2$a_method
#> Called Class1$a_method
#> Exiting Class2$a_method
#> Exiting Class3$a_method
#> Exiting Skipper$a_method

x$a_method("Class2")
#> Entering Skipper$a_method
#> Entering Class2$a_method
#> Called Class1$a_method
#> Exiting Class2$a_method
#> Exiting Skipper$a_method

x$a_method("Class1")
#> Entering Skipper$a_method
#> Called Class1$a_method
#> Exiting Skipper$a_method
```

Mixin example:

``` r
Mixin %class% {
  mixin_method <- function() {
    cat("Called mixin_method\n")
  }
}

Class4(Class3, Mixin) %class% {
  a_method <- function() {
    writeLines("Entering Class4$a_method")
    self$mixin_method()
    writeLines("Exiting Class4$a_method")
  }
}

x <- Class4()

x
#> class instance of type: <Class4, Class3, Class2, Class1, Mixin, R7>
#> <self>:
#>  list()
#> <Class4>:
#>  $ a_method:function ()  
#> <Class3>:
#>  $ a_method:function ()  
#> <Class2>:
#>  $ a_method:function ()  
#> <Class1>:
#>  $ a_method:function ()  
#> <Mixin>:
#>  $ mixin_method:function ()

x$a_method()
#> Entering Class4$a_method
#> Called mixin_method
#> Exiting Class4$a_method

x$mixin_method()
#> Called mixin_method

class(x)
#> [1] "Class4" "Class3" "Class2" "Class1" "Mixin"  "R7"
```

Access class methods directly from the class generator, without
instantiation.

``` r
Class1$a_method()
#> Called Class1$a_method
```

Pass an instance of `self` as the first argument when accessing a method
directly from the class generator and not from a class instance:

``` r
Stopwatch$..format..(watch)
#> [1] "Stopwatch: name = 'Nap time', lap = 1.04 secs, total = 1.04 secs, running"
```

Active properties lose their “activeness” when invoked directly from the
class generator, but also gain the ability to take an instance of `self`
as the first argument.

``` r
class(Stopwatch$lap_time)
#> [1] "R7_active_property_prototype"
Stopwatch$lap_time(self = watch)
#> Time difference of 1.046891 secs
```

Even methods that access private properties

``` r
Stopwatch$total_time(self = watch)
#> Time difference of 1.048784 secs
```

Example of defining a class with private attributes, `$`, and `$<-`
methods.

``` r
URLBuilder %class% {
  # properties initialized to NULL in the class definition body stay private
  # they can be:
  #  - accessed directly from within methods,
  #  - set from methods with <<-
  name <- root <- NULL

  ..init.. <- function(name, root = NULL) {
    name <<- name
    root <<- root
  }

  "..$.." <- function(name) URLBuilder(name, self)

  ..call.. <- function() {
    # just print to stdout for fun, doesn't actually interact w/ api.
    cat("GET ", ..format..(), "\n")
  }

  "..$<-.." <- function(name, value) {
    if (!inherits(value, "URLBuilder"))
      cat("POST", format(URLBuilder(name, self)), value, "\n")
    self
  }

  ..format.. <- function() {
    components <- name
    private_env <- parent.env(environment())
    repeat {
      # reach in for the private env of `root`'s URLBuilder methods
      private_env <- super("URLBuilder", root, private = TRUE)

      append1(components) <- get("name", private_env)
      root <- get("root", private_env)
      if (is.null(root))
        break
    }
    paste0(rev(components), collapse = "/")
  }
}


gh <- URLBuilder("https://api.github.com")

gh$orgs$"r-lib"$fs()
#> GET  https://api.github.com/orgs/r-lib/fs

gh$orgs$"r-lib"$fs <- "SOME DATA"
#> POST https://api.github.com/orgs/r-lib/fs SOME DATA
```
