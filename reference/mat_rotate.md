# mat_rotate

90° clockwise matrix rotation.

Applied twice, the function provide the mirror matrix, according to
vertical and horizontal symmetry.

## Usage

``` r
mat_rotate(data, lib_path = NULL, safer_check = TRUE, error_text = "")
```

## Arguments

- data:

  Matrix (matrix class). NA allowed.

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

- safer_check:

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Warning: must
  be set to `FALSE` if this function is used inside another "safer"
  function to avoid pointless multiple checkings.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

The modified matrix.

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
mat1 <- matrix(data = 1:10, ncol = 2) ; mat_rotate(data = mat1)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    5    4    3    2    1
#> [2,]   10    9    8    7    6

mat2 <- matrix(data = LETTERS[1:10], ncol = 5) ; mat_rotate(data = mat2)
#>      [,1] [,2]
#> [1,] "B"  "A" 
#> [2,] "D"  "C" 
#> [3,] "F"  "E" 
#> [4,] "H"  "G" 
#> [5,] "J"  "I" 
```
