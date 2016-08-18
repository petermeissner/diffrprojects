# README






# Using diffr for more than two files


**Status**

*unstable* - in wild developement with fuRiouS rEstRucturINg and biG biG pOKing

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/diffrprojects.svg?branch=master)](https://travis-ci.org/petermeissner/diffrprojects)


**Version**

0.1.2.90000


**Description**

This is a description still to be done but to
    prevent checks about complaining about to short descriptions
    this does not simply read TBD.


**License**

MIT + file LICENSE <br>Peter Meissner [aut, cre],
  Ulrich Sieberer [cph],
  University of Konstanz [cph]




**Citation**



Meißner P (2016). _diffrprojects: Using diffr for more than two texts_. R package version
0.1.2.90000, <URL: https://github.com/petermeissner/diffrprojects>.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and Explaining Parliamentary Rule
Changes in Europe: A Research Program." _Legislative Studies Quarterly_, *41*(1), pp. 61-88. ISSN
1939-9162, doi: 10.1111/lsq.12106 (URL: http://doi.org/10.1111/lsq.12106), <URL:
http://dx.doi.org/10.1111/lsq.12106>.

**BibTex for citing**


```r
toBibtex(citation("diffrprojects"))
```



**Installation**


```r
    install.packages("diffrprojects", repos = "https://petermeissner.github.io/drat")
```


    

**Example Usage**


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

testfiles <- rtext:::testfile(pattern="rc_\\d.txt", full.names = TRUE)

dp$text_add(testfiles)
```

```
## rtext : initializing
```

```
## rtext : initializing
## rtext : initializing
```

```r
names(dp$texts)
```

```
## [1] "rc_1.txt" "rc_2.txt" "rc_3.txt"
```

```r
dp$text_data()
```

```
##                                                   text_file character encoding sourcetype
## 1 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_1.txt    926166    UTF-8  text_file
## 2 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_2.txt    844551    UTF-8  text_file
## 3 C:/Users/peter/R/win-library/3.3/rtext/testfiles/rc_3.txt    643012    UTF-8  text_file
```

```r
dp$texts_link()
dp$links
```

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
```
   

    
    
    
    
    
    
    
    
    
    
