
[![Travis-CI Build Status](https://travis-ci.org/mexindian/TileMaker.svg?branch=master)](https://travis-ci.org/mexindian/TileMaker)

TileMaker
=========

An R script that enables the creation of data tiles for inclusion in a html dashboard or some such.

Now available as a package!
---------------------------

To install, please type:

    devtools::install_github("mexindian/TileMaker")

Please see the [Intro vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/DataStrategist/TileMaker/master/Vignette.html) to learn more about how to do some lovely stuff, but in the meantime, here's the basics:

This package is intended to "highlight single values", mainly in dashboards, reports or Shiny apps, and is highly customizeable. This is what it looks like:

``` r
library(TileMaker)
a <- solo_box(value = 3,text = "Little piggies<br>go to the market",icon="piggy-bank")
b <- solo_gradient_box(value = 65,text="test score I got")
c <- solo_gradient_box(value=95,text="test score I wanted")
d <- multi_box(values = c(4,5,6),text = c("Sally","George", "Mohammed"),icons = c("check","plus","calendar"),title = "Candidates")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
e <- iris %>% group_by(Species) %>% summarize(a=mean(Petal.Length)) %>% 
  tile_matrix(values = a,text = Species)
f <- iris %>% group_by(Species) %>% summarize(a=mean(Petal.Length)) %>% 
  mutate(old_a=c(3,4,5)) %>% 
  tile_matrix(values = a,text = Species,former=old_a)
```

    ## Warning in tile_matrix(., values = a, text = Species, former = old_a):
    ## Converted NAs in former to 0.001
