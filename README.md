
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
  ..init.. <- function(name = "", start = FALSE) {
    self$name <- name
    self$running <- FALSE
    self$start_time <- .POSIXct(NA_real_)
    self$stop_time <- .POSIXct(NA_real_)
    self$.total <- lap_time
    if (start)
      self()
  }
  ..call.. <- function() {
    # self() toggles start/stop
    if (self$running) {
      self$stop_time <- Sys.time()
      add(self$.total) <-  self$stop_time - self$start_time
    } else {
      self$start_time <- Sys.time()
    }

    self$running <- !self$running
  }

  lap_time %<-active% function(x) {
    if (!missing(x))
      stop("Protected property")
    if (self$running)
      Sys.time() - self$start_time
    else
      self$stop_time - self$start_time
  }

   total_time %<-active% function(x) {
    if (!missing(x))
      stop("Protected property")
    if (self$running)
      self$.total + lap_time()
    else
      self$.total
  }

  ..format.. <- function(x, ...) {
    sprintf(
      "Stopwatch: name = '%s', lap = %s, total = %s, %s",
      self$name,
      format(self$lap_time, digits = 3),
      format(self$total, digits = 3),
      if (self$running) "running" else "stopped")
  }

  reset <- ..init..
}
```

Calling `%class%` binds a class generator in the frame. Instantiate a
class instance by calling the generator.

``` r
watch <- Stopwatch()
```

The `print` method for class instances shows all resolvable objects from
`self$*`.

``` r
watch
#> class instance of type: <Stopwatch, R7>
#> <self>:
#>  $ .total    : 'difftime' num NA
#>  $ name      : chr ""
#>  $ running   : logi FALSE
#>  $ start_time: POSIXct[1:1], format: NA
#>  $ stop_time : POSIXct[1:1], format: NA
#> <Stopwatch>:
#>  $ ..call..           :function ()  
#>  $ ..format..         :function (x, ...)  
#>  $ ..init..           :function (name = "", start = FALSE)  
#>  $ lap_time (active)  : 'difftime' num NA
#>  $ reset              :function (name = "", start = FALSE)  
#>  $ total_time (active): 'difftime' num NA
```

Invoke double dotted methods like `..format..()` via S3.

``` r
cat(format(watch))
#> Stopwatch: name = '', lap = NA secs, total = NULL, stopped

identical(format(watch), watch$..format..())
#> [1] TRUE
```

A call of `self()` invokes `self$..call..()`.

``` r
watch()
cat(format(watch))
#> Stopwatch: name = '', lap = 0.000312 secs, total = NULL, running
```

Active binding as properties.

``` r
watch$lap_time
#> Time difference of 0.002429008 secs
Sys.sleep(1)
watch$lap_time
#> Time difference of 1.005045 secs
try(watch$lap_time <- NA)
#> Error in (function (x)  : Protected property
```

What works?

-   Single inheritance
-   Multiple inheritance / mixin subclasses  
    (object resolution order is flattened and fixed at instantiation
    time)
-   Instances are callable if a `..call..` method is defined.
-   Active bindings are supported.
-   Double-dotted (“dottr”) methods are auto invoked via S3.
-   `self` and `super` are in scope of all methods. Both are callable
    and accessible with `$`.
-   `self` is available at initialization time.
-   `super` is available always, even at methods construction time.
-   `super("a_classname")` resolves a specific superclasses environment.
-   `self(...)` invokes `self$..call..(...)`.

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
  mixin_method <- function(x, name) {
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
#>  $ mixin_method:function (x, name)

x$a_method()
#> Entering Class4$a_method
#> Called mixin_method
#> Exiting Class4$a_method

x$mixin_method()
#> Called mixin_method

class(x)
#> [1] "Class4" "Class3" "Class2" "Class1" "Mixin"  "R7"
```
