# mat_num2color

\
[`library`](https://rdrr.io/r/base/library.html)`(``saferMatrix``)`

\

### Datasets

\
`mat1`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``, ``5``,``2``,``3``,``NA``, ``5``,``5``,``1``,``1``, ``5``,``5``,``5``,``1``)``, ncol ``=`` ``4``)`\
`mat2`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``/``3``,``1``/``3``,``1``/``3``,``1``/``7``, ``1``/``3``,``1``/``3``,``1``/``9``,``1``/``3``, ``1``/``3``,``1``/``7``,``1``/``3``,``1``/``3``, ``1``/``9``,``1``/``3``,``1``/``3``,``1``/``3``)``, ncol ``=`` ``4``)`

\

### Datasets info

``` r
mat1
>      [,1] [,2] [,3] [,4]
> [1,]    1    5    5    5
> [2,]    1    2    5    5
> [3,]    1    3    1    5
> [4,]    2   NA    1    1
mat2
>           [,1]      [,2]      [,3]      [,4]
> [1,] 0.3333333 0.3333333 0.3333333 0.1111111
> [2,] 0.3333333 0.3333333 0.1428571 0.3333333
> [3,] 0.3333333 0.1111111 0.3333333 0.3333333
> [4,] 0.1428571 0.3333333 0.3333333 0.3333333
```

\

### Simple examples

``` r
mat_num2color(mat1 = mat2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#FFAA00FF"
> [2,] "#00FF00FF" "#00FF00FF" "#FFDB00FF" "#00FF00FF"
> [3,] "#00FF00FF" "#FFAA00FF" "#00FF00FF" "#00FF00FF"
> [4,] "#FFDB00FF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```

\

### Argument mat.hsv.h

``` r
mat_num2color(mat1 = mat1, mat.hsv.h = FALSE)
> $mat1.name
> [1] "mat1"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#CCFF00FF" "#FF0000FF" "#FF0000FF" "#FF0000FF"
> [2,] "#CCFF00FF" "#00FF66FF" "#FF0000FF" "#FF0000FF"
> [3,] "#CCFF00FF" "#0066FFFF" "#CCFF00FF" "#FF0000FF"
> [4,] "#00FF66FF" NA          "#CCFF00FF" "#CCFF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, mat.hsv.h = TRUE)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#FFAA00FF"
> [2,] "#00FF00FF" "#00FF00FF" "#FFDB00FF" "#00FF00FF"
> [3,] "#00FF00FF" "#FFAA00FF" "#00FF00FF" "#00FF00FF"
> [4,] "#FFDB00FF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```

\

### Argument notch

``` r
mat_num2color(mat1 = mat2, notch = 0.2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#00EEFFFF"
> [2,] "#00FF00FF" "#00FF00FF" "#00FF0FFF" "#00FF00FF"
> [3,] "#00FF00FF" "#00EEFFFF" "#00FF00FF" "#00FF00FF"
> [4,] "#00FF0FFF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, notch = 0.5)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#FFAA00FF"
> [2,] "#00FF00FF" "#00FF00FF" "#0024FFFF" "#00FF00FF"
> [3,] "#00FF00FF" "#FFAA00FF" "#00FF00FF" "#00FF00FF"
> [4,] "#0024FFFF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, notch = 0.7)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#00EEFFFF"
> [2,] "#00FF00FF" "#00FF00FF" "#FF00F0FF" "#00FF00FF"
> [3,] "#00FF00FF" "#00EEFFFF" "#00FF00FF" "#00FF00FF"
> [4,] "#FF00F0FF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```

\

### Argument s

``` r
mat_num2color(mat1 = mat2, s = 0.2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF" "#FFEECCFF"
> [2,] "#CCFFCCFF" "#CCFFCCFF" "#FFF8CCFF" "#CCFFCCFF"
> [3,] "#CCFFCCFF" "#FFEECCFF" "#CCFFCCFF" "#CCFFCCFF"
> [4,] "#FFF8CCFF" "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, s = 0.2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF" "#FFEECCFF"
> [2,] "#CCFFCCFF" "#CCFFCCFF" "#FFF8CCFF" "#CCFFCCFF"
> [3,] "#CCFFCCFF" "#FFEECCFF" "#CCFFCCFF" "#CCFFCCFF"
> [4,] "#FFF8CCFF" "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, s = 0.2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF" "#FFEECCFF"
> [2,] "#CCFFCCFF" "#CCFFCCFF" "#FFF8CCFF" "#CCFFCCFF"
> [3,] "#CCFFCCFF" "#FFEECCFF" "#CCFFCCFF" "#CCFFCCFF"
> [4,] "#FFF8CCFF" "#CCFFCCFF" "#CCFFCCFF" "#CCFFCCFF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```

\

### Argument v

``` r
mat_num2color(mat1 = mat2, v = 0.2)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#003300FF" "#003300FF" "#003300FF" "#332200FF"
> [2,] "#003300FF" "#003300FF" "#332C00FF" "#003300FF"
> [3,] "#003300FF" "#332200FF" "#003300FF" "#003300FF"
> [4,] "#332C00FF" "#003300FF" "#003300FF" "#003300FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, v = 0.5)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#008000FF" "#008000FF" "#008000FF" "#805500FF"
> [2,] "#008000FF" "#008000FF" "#806D00FF" "#008000FF"
> [3,] "#008000FF" "#805500FF" "#008000FF" "#008000FF"
> [4,] "#806D00FF" "#008000FF" "#008000FF" "#008000FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(mat1 = mat2, v = 0.7)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00B300FF" "#00B300FF" "#00B300FF" "#B37700FF"
> [2,] "#00B300FF" "#00B300FF" "#B39900FF" "#00B300FF"
> [3,] "#00B300FF" "#B37700FF" "#00B300FF" "#00B300FF"
> [4,] "#B39900FF" "#00B300FF" "#00B300FF" "#00B300FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```

\

### Argument forced.color

``` r
mat_num2color(mat1 = mat2, forced.color = "snow")
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "snow"     
> [2,] "#00FF00FF" "#00FF00FF" "#FFDB00FF" "#00FF00FF"
> [3,] "#00FF00FF" "snow"      "#00FF00FF" "#00FF00FF"
> [4,] "#FFDB00FF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> [1] FALSE
> 
> $text.problem
> NULL
```

\

### All the arguments

``` r
mat_num2color(
        mat1 = mat1, 
        mat.hsv.h = FALSE, 
        notch = 1, 
        s = 1, 
        v = 1, 
        forced.color = NULL
)
> $mat1.name
> [1] "mat1"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#CCFF00FF" "#FF0000FF" "#FF0000FF" "#FF0000FF"
> [2,] "#CCFF00FF" "#00FF66FF" "#FF0000FF" "#FF0000FF"
> [3,] "#CCFF00FF" "#0066FFFF" "#CCFF00FF" "#FF0000FF"
> [4,] "#00FF66FF" NA          "#CCFF00FF" "#CCFF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL

mat_num2color(
        mat1 = mat2, 
        mat.hsv.h = TRUE, 
        notch = 1, 
        s = 1, 
        v = 1, 
        forced.color = NULL
)
> $mat1.name
> [1] "mat2"
> 
> $colored.mat
>      [,1]        [,2]        [,3]        [,4]       
> [1,] "#00FF00FF" "#00FF00FF" "#00FF00FF" "#FFAA00FF"
> [2,] "#00FF00FF" "#00FF00FF" "#FFDB00FF" "#00FF00FF"
> [3,] "#00FF00FF" "#FFAA00FF" "#00FF00FF" "#00FF00FF"
> [4,] "#FFDB00FF" "#00FF00FF" "#00FF00FF" "#00FF00FF"
> 
> $problem
> NULL
> 
> $text.problem
> NULL
```
