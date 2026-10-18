# mat_op

\
[`library`](https://rdrr.io/r/base/library.html)`(``saferMatrix``)`

\

### Datasets

\
`mat1`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``1``,``5``,``9``,``8``)``, ncol ``=`` ``2``, dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``LETTERS``[``1``:``4``]``, ``letters``[``1``:``2``]``)``)`\
`mat2`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``1``,``5``,``9``,``NA``)``, ncol ``=`` ``2``, dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``LETTERS``[``1``:``4``]``, ``letters``[``1``:``2``]``)``)`\
`mat3`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``1``,``5``,``9``,``8``)``, ncol ``=`` ``2``, dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``LETTERS``[``1``:``4``]``, `[`c`](https://rdrr.io/r/base/c.html)`(``NA``, ``NA``)``)``)`\
`mat4`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``1``,``5``,``9``,``8``)``, ncol ``=`` ``2``, dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"A1"``, ``"A2"``, ``"A3"``, ``"A4"``)``, ``letters``[``1``:``2``]``)``)`

\

### Datasets info

``` r
mat1
>   a b
> A 1 1
> B 1 5
> C 1 9
> D 2 8
mat2
>   a  b
> A 1  1
> B 1  5
> C 1  9
> D 2 NA
mat3
>   <NA> <NA>
> A    1    1
> B    1    5
> C    1    9
> D    2    8
mat4
>    a b
> A1 1 1
> A2 1 5
> A3 1 9
> A4 2 8
```

\

### Simple examples

``` r
mat_op(mat_list = list(mat1, mat2))
>   a  b
> A 2  2
> B 2 10
> C 2 18
> D 4 NA
mat_op(mat_list = list(mat2, mat3))
>   [,1] [,2]
> A    2    2
> B    2   10
> C    2   18
> D    4   NA
mat_op(mat_list = list(mat2, mat2, mat4))
>      a  b
> [1,] 3  3
> [2,] 3 15
> [3,] 3 27
> [4,] 6 NA
```

\

### All the arguments

``` r
mat_op(mat_list = list(mat1, mat2), kind_of_operation = "*")
>   a  b
> A 1  1
> B 1 25
> C 1 81
> D 4 NA
mat_op(mat_list = list(mat2, mat3), kind_of_operation = "-")
>   [,1] [,2]
> A    0    0
> B    0    0
> C    0    0
> D    0   NA
mat_op(mat_list = list(mat2, mat2, mat4), kind_of_operation = "+")
>      a  b
> [1,] 3  3
> [2,] 3 15
> [3,] 3 27
> [4,] 6 NA
```
