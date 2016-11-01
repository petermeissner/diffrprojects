





# Using diffr for more than two files

## Overview

**Status**


[![Travis-CI Build Status](https://travis-ci.org/petermeissner/diffrprojects.svg?branch=master)](https://travis-ci.org/petermeissner/diffrprojects)
[![codecov](https://codecov.io/gh/petermeissner/diffrprojects/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/diffrprojects/tree/master/R)
[![CRAN version](http://www.r-pkg.org/badges/version/diffrprojects)](https://cran.r-project.org/package=diffrprojects)









*R code:* 1614<br> 
*C++ code:* 112<br> 
*test code:* 1010




<br><br>
**Version**

0.1.12




<br><br>
**Description**

Provides data structures and methods for manual as wells as
    automated R based text comparison and text as well as change coding.



<br><br>
**License**

MIT + file LICENSE <br>Peter Meissner <retep.meissner@gmail.com> [aut, cre] Ulrich Sieberer <ulrich.sieberer@uni-bamberg.de> [cph] University of Konstanz <willkommen@uni-konstanz.de> [cph]



<br><br>
**Citation**



Meißner P (2016). _diffrprojects: Using diffr for more than two texts_. R package version
0.1.12, <URL: https://github.com/petermeissner/diffrprojects>.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and Explaining Parliamentary
Rule Changes in Europe: A Research Program." _Legislative Studies Quarterly_, *41*(1),
pp. 61-88. ISSN 1939-9162, doi: 10.1111/lsq.12106 (URL:
http://doi.org/10.1111/lsq.12106), <URL: http://dx.doi.org/10.1111/lsq.12106>.

**BibTex for citing**


```r
toBibtex(citation("diffrprojects"))
```



<br><br>
**Installation**

stable CRAN version


```r
install.packages("rtext")
library(rtext)
```



(stable) development version


```r
standard_repos <- options("repos")$repos
install.packages( "rtext", repos = c(standard_repos, "https://petermeissner.github.io/drat/"))
library(rtext)
```



    
## Usage

### Fast Introduction for the Impatient

For those in a hurry here is a very brief


```r
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

```
## rtext : initializing
## rtext : initializing
## rtext : initializing
```

```r
dp$text_data(1) %>% head(11)
```

```
##     i char         name
## 1   1    T rc_1_ch1.txt
## 2   2    h rc_1_ch1.txt
## 3   3    e rc_1_ch1.txt
## 4   4      rc_1_ch1.txt
## 5   5    P rc_1_ch1.txt
## 6   6    r rc_1_ch1.txt
## 7   7    o rc_1_ch1.txt
## 8   8    j rc_1_ch1.txt
## 9   9    e rc_1_ch1.txt
## 10 10    c rc_1_ch1.txt
## 11 11    t rc_1_ch1.txt
```

```r
# linking the files (which file should be compared to which)
dp$text_link()
dp$link %>% as.data.frame()
```

```
##           from           to                      link
## 1 rc_1_ch1.txt rc_2_ch1.txt rc_1_ch1.txt~rc_2_ch1.txt
## 2 rc_2_ch1.txt rc_3_ch1.txt rc_2_ch1.txt~rc_3_ch1.txt
```

```r
# calculating text alignments
dp$text_align(tokenizer=text_tokenize_words)
dp$alignment[[1]] %>% head(30)
```

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
```
   
   
   
### Creating a Diffr Project

To create a diffrproject we use the diffrproject creator object - its simply an object with an function that knows how to create a project. 

Creating a project looks like this:


```r
library(diffrprojects)
```

```
## Loading required package: stringb
```

```
## Loading required package: rtext
```

```r
dp <- diffrproject$new()
```

Et violà - we created a first, for now empty, project that we will use throughout the tutorial. 


## Some help please 

To get a better idea about what this thing called *diffrproject* really is you can consult its help page which gives a broad overview over its capabilities:


```r
?diffrproject
```

Another way is to call its ls() method. This will present us with a data frame listing all fields where data is stored and all the methods (aka object specific functions). Those methods and fields located in *private* are not for the user to mess around with while non-private (*self* aka public) data fields can be read by the user and public methods can be triggered by the user to manipulate the data or retrieve data in a specific format. 


```r
dp$ls()
```

```
##                               name   where                     class
## 1                     execute_load private                  function
## 2                             hash private                  function
## 3                           hashed private                  function
## 5                     prepare_save private                  function
## 4                           hashes private                      list
## 9                   alignment_data    self alignment_data_list, list
## 6                        alignment    self      alignment_list, list
## 21                            link    self      alignment_list, list
## 7                    alignment_add    self                  function
## 8                   alignment_code    self                  function
## 10             alignment_data_full    self                  function
## 11              alignment_data_set    self                  function
## 12                alignment_delete    self                  function
## 13                           clone    self                  function
## 14                           debug    self                  function
## 15                      export_csv    self                  function
## 16                   export_sqlite    self                  function
## 17                             get    self                  function
## 18                      import_csv    self                  function
## 19                   import_sqlite    self                  function
## 20                      initialize    self                  function
## 22                            load    self                  function
## 23                              ls    self                  function
## 24                         message    self                  function
## 27                            save    self                  function
## 29                        text_add    self                  function
## 30                      text_align    self                  function
## 31                       text_code    self                  function
## 32       text_code_alignment_token    self                  function
## 33 text_code_alignment_token_regex    self                  function
## 34                 text_code_regex    self                  function
## 35                       text_data    self                  function
## 36               text_data_inherit    self                  function
## 37                     text_delete    self                  function
## 38                       text_link    self                  function
## 39                  text_meta_data    self                  function
## 40        tokenize_text_data_lines    self                  function
## 41        tokenize_text_data_regex    self                  function
## 42        tokenize_text_data_words    self                  function
## 43                         warning    self                  function
## 25                            meta    self                      list
## 26                         options    self                      list
## 28                            text    self                      list
```





    
    

