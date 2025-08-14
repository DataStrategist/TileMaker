
[![Travis-CI Build
Status](https://travis-ci.org/DataStrategist/TileMaker.svg?branch=master)](https://travis-ci.org/DataStrategist/TileMaker)
[![Coverage
Status](https://coveralls.io/repos/github/DataStrategist/TileMaker/badge.svg?branch=master)](https://coveralls.io/github/DataStrategist/TileMaker?branch=master)
[![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://saythanks.io/to/DataStrategist)

# TileMaker

An R library that enables the creation of data tiles for inclusion in a
html dashboard or some such.

To install, please type:

    devtools::install_github("DataStrategist/TileMaker", build_vignettes = TRUE)

Please see the [Intro
Vignette](http://datastrategist.github.io/TileMaker/articles/Intro.html)
to learn more about how to do some lovely stuff, but in the meantime,
here’s the basics:

This package is intended to “highlight single values”, mainly in
dashboards, reports or Shiny apps, and is highly customizeable. This is
what it looks like:

``` r
suppressWarnings(suppressMessages(library(tidyverse,quietly = TRUE)))
library(TileMaker)

a <- solo_box(value = 3, txt = "Little piggies<br>go to the market", icon = "piggy-bank")
b <- solo_gradient_box(value = 65, txt = "test score I got")
```

    ## -- using target value of 100 --

``` r
c <- solo_gradient_box(value = 95, txt = "test score I wanted")
```

    ## -- using target value of 100 --

``` r
d <- multi_box(values = c(4, 5, 6), txt = c("Sally", "George", "Mohammed"), icons = c("check", "plus", "calendar"), title = "Candidates")

e <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  tile_matrix(values = a, txt = Species)
```

    ## -- using target value of 100 --
    ## -- using target value of 100 --
    ## -- using target value of 100 --

``` r
f <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  mutate(old_a = c(3, 4, 5)) %>%
  tile_matrix(data = ., values = a, txt = Species, former =  old_a)
```

    ## -- using target value of 100 --
    ## -- using target value of 100 --
    ## -- using target value of 100 --

``` r
d1 <- div_maker(subtitle = "First line", textModifier = "h1", a, b)
d2 <- div_maker(subtitle = "Second line", textModifier = "h1", c, d)
d3 <- div_maker(subtitle = "Boom line", textModifier = "h1", e, f)

finisher(
  title = "Important Reportings", 
  css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
  file = NULL, 
  textModifier = "h1",div_maker(subtitle = "Boom", textModifier = "hi",d1, d2, d3)
)
```

<html>
<body>
<h1>Important Reportings</h1>
<div>
<hi>Boom</hi>
<div class="container">
<h1>First line</h1>
<div class="panel panel-info">
<div class="panel-body text-center">
<h1>
<i class="glyphicon glyphicon-piggy-bank"></i>
3
</h1>
<div style="margin-top: 10px;">Little piggies<br>go to the market</div>
</div>
</div>
<div class="panel panel-warning">
<div class="panel-body text-center">
<h1>65</h1>
<div style="margin-top: 10px;">test score I got</div>
</div>
</div>
</div>
<div class="container">
<h1>Second line</h1>
<div class="panel panel-success">
<div class="panel-body text-center">
<h1>95</h1>
<div style="margin-top: 10px;">test score I wanted</div>
</div>
</div>
<div class="panel panel-info">
<div class="panel-body text-center">
<h1>Candidates</h1>
<h3>
<i class="glyphicon glyphicon-check"></i>
<span style="font-size:150%">4</span>
Sally
</h3>
<h3>
<i class="glyphicon glyphicon-plus"></i>
<span style="font-size:150%">5</span>
George
</h3>
<h3>
<i class="glyphicon glyphicon-calendar"></i>
<span style="font-size:150%">6</span>
Mohammed
</h3>
</div>
</div>
</div>
<div class="container">
<h1>Boom line</h1>
<a>
<h1></h1>
<div class="container">
<h2>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>1.5</h1>
<div style="margin-top: 10px;">setosa</div>
</div>
</div>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>4.3</h1>
<div style="margin-top: 10px;">versicolor</div>
</div>
</div>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>5.6</h1>
<div style="margin-top: 10px;">virginica</div>
</div>
</div>
</h2>
</div>
</a>
<a>
<h1></h1>
<div class="container">
<h2>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>
1.5
<sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;">
<i class="glyphicon glyphicon-chevron-down" style="font-size: 10px; vertical-align: top;"></i>
50%
</sup>
</h1>
<div style="margin-top: 10px;">setosa</div>
</div>
</div>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>
4.3
<sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;">
<i class="glyphicon glyphicon-chevron-up" style="font-size: 10px; vertical-align: top;"></i>
7.5%
</sup>
</h1>
<div style="margin-top: 10px;">versicolor</div>
</div>
</div>
<div class="panel panel-danger">
<div class="panel-body text-center">
<h1>
5.6
<sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;">
<i class="glyphicon glyphicon-chevron-up" style="font-size: 10px; vertical-align: top;"></i>
12%
</sup>
</h1>
<div style="margin-top: 10px;">virginica</div>
</div>
</div>
</h2>
</div>
</a>
</div>
</div>
</body>
</html>
