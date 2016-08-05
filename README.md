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

<code style="white-space:normal;">
@Manual{Meissner2016,
  title = {diffrprojects: Using diffr for more than two texts},
  author = {Peter Meißner},
  year = {2016},
  note = {R package version 0.1.2.90000},
  url = {https://github.com/petermeissner/diffrprojects},
}

@Article{Sieberer2016,
  title = {Mapping and Explaining Parliamentary Rule Changes in Europe: A Research Program},
  author = {Ulrich Sieberer and Peter Meißner and Julia F. Keh and Wolfgang C. Müller},
  journal = {Legislative Studies Quarterly},
  volume = {41},
  number = {1},
  issn = {1939-9162},
  url = {http://dx.doi.org/10.1111/lsq.12106},
  doi = {10.1111/lsq.12106},
  pages = {61--88},
  year = {2016},
  abstract = {We outline a comprehensive research program on institutional reforms in European parliaments. Original data show that parliamentary rules in Western European parliaments have been changed frequently and massively during the period from 1945 to 2010 suggesting that actors use institutional reforms as a distinct strategy to pursue their substantive goals. We discuss how institutional instability affects existing theoretical and empirical arguments about institutional effects. Furthermore, we present four ideal-typical approaches to analyzing rule changes, present new software tools for identifying and coding changes in large text corpora, and demonstrate their usefulness for valid measurement of the overall change between subsequent text versions.},
}
</code>



**Installation**


```r
    install.packages("diffrprojects", repos = "https://petermeissner.github.io/drat")
```


    

**Example Usage**


```r
library(diffrprojects)

dp <- diffrproject$new()

testfiles <- diffrprojects:::test_file(pattern="rc_\\d.txt", full.names = TRUE)

dp$text_add(testfiles)
```

```
## rtext : initializing
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
##                                                                        text_file character encoding
## 1 /home/peter/R/x86_64-pc-linux-gnu-library/3.3/diffrprojects/testfiles/rc_1.txt      1501    UTF-8
## 2 /home/peter/R/x86_64-pc-linux-gnu-library/3.3/diffrprojects/testfiles/rc_2.txt     10842    UTF-8
## 3 /home/peter/R/x86_64-pc-linux-gnu-library/3.3/diffrprojects/testfiles/rc_3.txt     10268    UTF-8
##   sourcetype
## 1  text_file
## 2  text_file
## 3  text_file
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
   

    
    
    
    
    
    
    
    
    
    
