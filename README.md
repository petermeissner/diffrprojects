
Using diffr for more than two files
===================================

**Status**

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/diffrprojects.svg?branch=master)](https://travis-ci.org/petermeissner/diffrprojects) [![codecov](https://codecov.io/gh/petermeissner/diffrprojects/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/diffrprojects/tree/master/R) [![CRAN version](http://www.r-pkg.org/badges/version/diffrprojects)](https://cran.r-project.org/package=diffrprojects)

*unstable* - in wild developement with fuRiouS rEstRucturINg and biG biG pOKing

*R code:* 663<br> *C++ code:* 112<br> *test code:* 594

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
devtools::install_github("petermeissner/stringb")
devtools::install_github("petermeissner/rtext")
devtools::install_github("petermeissner/diffrprojects")
# install.packages("diffrprojects", repos = "https://petermeissner.github.io/drat")
```

**Example Usage**

``` r
# loading package
library(diffrprojects)


# the first chapter of Robinson Crusoe from three different sources
rcs <- rtext:::testfile(pattern="rc.*ch1.txt", full.names = TRUE) 


# creating a new project
dp <- diffrproject$new()


# setting options
dp$options$verbose <- FALSE


# adding texts to the corpus
dp$text_add(text_file = rcs)
```

    ## rtext : initializing
    ## rtext : initializing
    ## rtext : initializing

``` r
dp$text_data()
```

    ##                                                       text_file character encoding sourcetype
    ## 1 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_1_ch1.txt     28851    UTF-8  text_file
    ## 2 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_2_ch1.txt     27617    UTF-8  text_file
    ## 3 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_3_ch1.txt     27548    UTF-8  text_file

``` r
# linking the files (which file should be compared to which)
dp$text_link()
dp$link %>% as.data.frame()
```

    ##           from           to                      link
    ## 1 rc_1_ch1.txt rc_2_ch1.txt rc_1_ch1.txt_rc_2_ch1.txt
    ## 2 rc_2_ch1.txt rc_3_ch1.txt rc_2_ch1.txt_rc_3_ch1.txt

``` r
# calculating text alignments
dp$text_align(tokenizer=text_tokenize_words)
dp$alignment[[1]] %>% head(30)
```

    ##    alignment_i token_i_1 token_i_2 distance      type from_1 to_1 from_2  to_2
    ## 1            1         1      1932        0 no-change      1    3  10326 10328
    ## 2            2         2        NA        7  deletion      5   11     NA    NA
    ## 3            3         3        NA        9  deletion     13   21     NA    NA
    ## 4            4         4        NA        5  deletion     23   27     NA    NA
    ## 5            5         5      1932        0 no-change     30   32  10326 10328
    ## 6            6         6        NA        4  deletion     34   37     NA    NA
    ## 7            7         7        87        0 no-change     39   41    513   515
    ## 8            8         8        NA       10  deletion     43   52     NA    NA
    ## 9            9         9        57        0 no-change     54   55    355   356
    ## 10          10        10         3        0 no-change     57   64     15    22
    ## 11          11        11         4        0 no-change     66   71     24    29
    ## 12          12        12        85        0 no-change     74   75    497   498
    ## 13          13        13         1        0 no-change     77   82      1     6
    ## 14          14        14         2        0 no-change     84   88      8    12
    ## 15          15        15      1520        0 no-change     92   95   8187  8190
    ## 16          16        16        NA        5  deletion     97  101     NA    NA
    ## 17          17        17      3667        0 no-change    103  104  19215 19216
    ## 18          18        18       263        0 no-change    106  108   1495  1497
    ## 19          19        19        51        0 no-change    110  112    328   330
    ## 20          20        20        NA        3  deletion    114  116     NA    NA
    ## 21          21        21        57        0 no-change    118  119    355   356
    ## 22          22        22        NA        6  deletion    121  126     NA    NA
    ## 23          23        23        NA        8  deletion    128  135     NA    NA
    ## 24          24        24        50        0 no-change    137  138    325   326
    ## 25          25        25        51        0 no-change    140  142    328   330
    ## 26          26        26        NA        6  deletion    144  149     NA    NA
    ## 27          27        27        NA        6  deletion    151  156     NA    NA
    ## 28          28        28        87        0 no-change    158  160    513   515
    ## 29          29        29       513        0 no-change    162  165   2853  2856
    ## 30          30        30       306        0 no-change    167  171   1724  1728
