
[![Travis-CI Build Status](https://travis-ci.org/DataStrategist/TileMaker.svg?branch=master)](https://travis-ci.org/DataStrategist/TileMaker)

TileMaker
=========

An R script that enables the creation of data tiles for inclusion in a html dashboard or some such.

Now available as a package!
---------------------------

To install, please type:

    devtools::install_github("DataStrategist/TileMaker")

Please see the [Intro vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/DataStrategist/TileMaker/master/Vignette.html) to learn more about how to do some lovely stuff, but in the meantime, here's the basics:

This package is intended to "highlight single values", mainly in dashboards, reports or Shiny apps, and is highly customizeable. This is what it looks like:

``` r
library(TileMaker)
a <- solo_box(value = 3, txt = "Little piggies<br>go to the market", icon = "piggy-bank")
b <- solo_gradient_box(value = 65, txt = "test score I got")
c <- solo_gradient_box(value = 95, txt = "test score I wanted")
d <- multi_box(values = c(4, 5, 6), txt = c("Sally", "George", "Mohammed"), icons = c("check", "plus", "calendar"), title = "Candidates")

library(dplyr,quietly = TRUE)
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(htmltools)
e <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  tile_matrix(values = a, txt = Species)
f <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  mutate(old_a = c(3, 4, 5)) %>%
  tile_matrix(values = a, txt = Species, former = old_a)
```

    ## Warning in tile_matrix(., values = a, txt = Species, former = old_a):
    ## Converted NAs in former to 0.001

``` r
d1 <- div_maker(span(a, b))
# d2 <- div_maker(e)
# d3 <- div_maker(f)

finisher(
  title = "Important Reportings", css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
  file = NULL, textModifier = "h1", d1, d1
)
```

<!--html_preserve-->
<html>
<body>
<h1>
Important Reportings
</h1>
<h1>
<span> <a>
<button class="btn btn-md btn-info" role="button" type="info">
<h1>
<i class="glyphicon glyphicon-piggy-bank"></i> 3
</h1>
Little piggies<br>go to the market
</button>
</a> <a>
<button class="btn btn-md btn-warning" role="button" type="warning">
<h1>
65
</h1>
test score I got
</button>
</a> </span>
</h1>

<h1>
<span> <a>
<button class="btn btn-md btn-info" role="button" type="info">
<h1>
<i class="glyphicon glyphicon-piggy-bank"></i> 3
</h1>
Little piggies<br>go to the market
</button>
</a> <a>
<button class="btn btn-md btn-warning" role="button" type="warning">
<h1>
65
</h1>
test score I got
</button>
</a> </span>
</h1>

</body>
</html>
<!--/html_preserve-->
