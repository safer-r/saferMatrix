# mat_op

Assemble several matrices of same dimensions by performing by case
operation. For instance, with kind.of.operation = "+", the function will
add the value of all the case 1 (row1 & column1) of the matrices and put
it in the case 1 of a new matrix M, according to the formula

\$\$c\_{ij} = \sum\_{k=1}^{k=z}c\_{ijk}\$\$ c: case

i: row number

j: column number

k: matrix number

z: number of matrices

## Usage

``` r
mat_op(
  mat.list,
  kind.of.operation = "+",
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- mat.list:

  List of matrices (matrix class) of numerical values. NA allowed.

- kind.of.operation:

  Either "+" (by case addition), "-" (by case subtraction) or "\*" (by
  case multiplication).

- safer_check:

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Warning: must
  be set to `FALSE` if this function is used inside another "safer"
  function to avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

The assembled matrix, with row and/or column names only if all the
matrices have identical row/column names.

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
mat1 <- base::matrix(data = c(1,1,1,2,1,5,9,8), ncol = 2) ;
mat2 <- base::matrix(data = c(1,1,1,2,1,5,9,NA), ncol = 2) ;
mat_op(mat.list = base::list(mat1, mat2), kind.of.operation = "+")
#>      [,1] [,2]
#> [1,]    2    2
#> [2,]    2   10
#> [3,]    2   18
#> [4,]    4   NA
```
