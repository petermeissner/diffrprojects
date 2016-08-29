
Using diffr for more than two files
===================================

**Status**

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/diffrprojects.svg?branch=master)](https://travis-ci.org/petermeissner/diffrprojects) [![codecov](https://codecov.io/gh/petermeissner/diffrprojects/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/diffrprojects/tree/master/R) [![CRAN version](http://www.r-pkg.org/badges/version/diffrprojects)](https://cran.r-project.org/package=diffrprojects)

*unstable* - in wild developement with fuRiouS rEstRucturINg and biG biG pOKing

*R code:* 775<br> *C++ code:* 112<br> *test code:* 577

**Version**

0.1.4.90000

**Description**

This is a description still to be done but to prevent checks about complaining about to short descriptions this does not simply read TBD.

**License**

MIT + file LICENSE <br>Peter Meissner <retep.meissner@gmail.com> \[aut, cre\] Ulrich Sieberer <ulrich.sieberer@uni-bamberg.de> \[cph\] University of Konstanz <willkommen@uni-konstanz.de> \[cph\]

**Citation**

Meißner P (2016). *diffrprojects: Using diffr for more than two texts*. R package version 0.1.4.90000, &lt;URL: <https://github.com/petermeissner/diffrprojects>&gt;.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and Explaining Parliamentary Rule Changes in Europe: A Research Program." *Legislative Studies Quarterly*, *41*(1), pp. 61-88. ISSN 1939-9162, doi: 10.1111/lsq.12106 (URL: <http://doi.org/10.1111/lsq.12106>), &lt;URL: <http://dx.doi.org/10.1111/lsq.12106>&gt;.

**BibTex for citing**

``` r
toBibtex(citation("diffrprojects"))
```

**Installation**

``` r
install.packages("diffrprojects", repos = "https://petermeissner.github.io/drat")
```

**Example Usage**

``` r
library(diffrprojects)
```

    ## Loading required package: stringb

    ## Loading required package: rtext

``` r
dp <- diffrproject$new()

testfiles <- rtext:::testfile(pattern="rc_\\d.txt", full.names = TRUE)

dp$text_add(text_file=testfiles)
```

    ## rtext : initializing

    ## rtext : initializing
    ## rtext : initializing

``` r
names(dp$texts)
```

    ## NULL

``` r
dp$text_data()
```

    ##                                                   text_file character encoding sourcetype
    ## 1 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_1.txt    926166    UTF-8  text_file
    ## 2 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_2.txt    844551    UTF-8  text_file
    ## 3 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_3.txt    643012    UTF-8  text_file

``` r
dp$text_link()
dp$link
```

    ## $rc_1.txt_rc_2.txt
    ## $rc_1.txt_rc_2.txt$from
    ## [1] "rc_1.txt"
    ## 
    ## $rc_1.txt_rc_2.txt$to
    ## [1] "rc_2.txt"
    ## 
    ## 
    ## $rc_2.txt_rc_3.txt
    ## $rc_2.txt_rc_3.txt$from
    ## [1] "rc_2.txt"
    ## 
    ## $rc_2.txt_rc_3.txt$to
    ## [1] "rc_3.txt"
    ## 
    ## 
    ## attr(,"class")
    ## [1] "alignment_list" "list"
