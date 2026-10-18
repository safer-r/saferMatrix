# mat_num2color

Convert a matrix made of numbers into a hexadecimal matrix for rgb
colorization.

## Usage

``` r
mat_num2color(
  mat1,
  mat_hsv_h = TRUE,
  notch = 1,
  s = 1,
  v = 1,
  forced_color = NULL,
  lib_path = NULL,
  safer_check = TRUE,
  error_text = ""
)
```

## Arguments

- mat1:

  Matrix 1 of non negative numerical values that has to be colored
  (matrix class). NA allowed.

- mat_hsv_h:

  Single logical value. Is mat1 the h of hsv colors ? (if TRUE, mat1
  must be between zero and 1). If FALSE, mat1 must be made of positive
  integer values without 0.

- notch:

  Single numeric value between 0 and 1 to shift the successive colors on
  the hsv circle by + notch.

- s:

  Single numeric value between 0 and 1 (s argument of
  [`grDevices::hsv()`](https://rdrr.io/r/grDevices/hsv.html)).

- v:

  Single numeric value between 0 and 1 (v argument of
  [`grDevices::hsv()`](https://rdrr.io/r/grDevices/hsv.html)).

- forced_color:

  Must be NULL or hexadecimal color code or name given by
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html). The
  first minimal values of mat1 will be these colors. All the color of
  mat1 can be forced using this argument.

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

A list containing:

\- \$mat1.name: name of mat1

\- \$colored.mat: colors of mat1 in hexa

\- \$problem: logical. Is any colors of forced_color overlap the colors
designed by the function. NULL if forced_color = NULL

\- \$text.problem: text when overlapping colors. NULL if forced_color =
NULL or problem == FALSE

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
mat1 <- base::matrix(data = c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4) ;
base::dimnames(x = mat1) <- base::list(LETTERS[1:4], letters[1:4]) ;
mat_num2color(mat1 = mat1, mat_hsv_h = FALSE, notch = 1, s = 1, v = 1, forced_color = NULL)
#> $mat1.name
#> [1] "mat1"
#> 
#> $colored.mat
#>   a           b           c           d          
#> A "#CCFF00FF" "#FF0000FF" "#FF0000FF" "#FF0000FF"
#> B "#CCFF00FF" "#00FF66FF" "#FF0000FF" "#FF0000FF"
#> C "#CCFF00FF" "#0066FFFF" "#CCFF00FF" "#FF0000FF"
#> D "#00FF66FF" NA          "#CCFF00FF" "#CCFF00FF"
#> 
#> $problem
#> NULL
#> 
#> $text.problem
#> NULL
#> 
```
