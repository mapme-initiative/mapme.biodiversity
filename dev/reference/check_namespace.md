# Checks if namespace is available

Use this function if your resource/indicator function requires the
namespace of a certain package to be available. An informative
error/warning message is printed if that is not the case.

## Usage

``` r
check_namespace(pkg, error = TRUE)
```

## Arguments

- pkg:

  A character vector of length one indicating a package name for which
  the namespace is tested

- error:

  A logical indicating whether or not to promote missing namespace to
  error. If FALSE, a warning is emitted.

## Value

TRUE, invisible, if the namespace is available. An error message if
`error = TRUE`, FALSE and a warning otherwise.
