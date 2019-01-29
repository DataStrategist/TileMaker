
[![Travis-CI Build Status](https://travis-ci.org/DataStrategist/TileMaker.svg?branch=master)](https://travis-ci.org/DataStrategist/TileMaker)

TileMaker
=========

An R library that enables the creation of data tiles for inclusion in a html dashboard or some such.

To install, please type:

    devtools::install_github("DataStrategist/TileMaker", build_vignettes = TRUE)

Please see the [pkgdown site](http://datastrategist.github.io/TileMaker/) to learn more about how to do some lovely stuff, but in the meantime, here's the basics:

This package is intended to "highlight single values", mainly in dashboards, reports or Shiny apps, and is highly customizeable. This is what it looks like:

``` r
suppressPackageStartupMessages(library(tidyverse,quietly = TRUE))
library(TileMaker)

a <- solo_box(value = 3, txt = "Little piggies<br>go to the market", icon = "piggy-bank")
b <- solo_gradient_box(value = 65, txt = "test score I got")
c <- solo_gradient_box(value = 95, txt = "test score I wanted")
d <- multi_box(values = c(4, 5, 6), txt = c("Sally", "George", "Mohammed"), icons = c("check", "plus", "calendar"), title = "Candidates")

e <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  tile_matrix(values = a, txt = Species)

f <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  mutate(old_a = c(3, 4, 5)) %>%
  tile_matrix(data = ., values = a, txt = Species, former =  old_a)

d1 <- div_maker(subtitle = "First line", textModifier = "h1", a, b)
d2 <- div_maker(subtitle = "Second line", textModifier = "h1", c, d)
d3 <- div_maker(subtitle = "Boom line", textModifier = "h1", e, f)

finisher(
  title = "Important Reportings", 
  css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
  file = NULL, 
  textModifier = "h1",divs = c(d1, d2, d3)
)
```

<!--html_preserve-->
<html>
<body>
<h1>
Important Reportings
</h1>
div
<h1>
First line
</h1>
<a>
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
</a> div container
<h1>
Second line
</h1>
<a>
<button class="btn btn-md btn-success" role="button" type="success">
<h1>
95
</h1>
test score I wanted
</button>
</a> <a>
<button class="btn btn-md btn-info" role="button" type="info">
<h1>
Candidates
</h1>
<h3>
<i class="glyphicon glyphicon-check"></i> <span style="font-size:150%">4</span> Sally
</h3>
<h3>
<i class="glyphicon glyphicon-plus"></i> <span style="font-size:150%">5</span> George
</h3>
<h3>
<i class="glyphicon glyphicon-calendar"></i> <span style="font-size:150%">6</span> Mohammed
</h3>
</button>
</a> div container
<h1>
Boom line
</h1>
<a>
<h1>
</h1>
<h2>
<a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
1.5
</h1>
setosa
</button>
</a> <a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
4.3
</h1>
versicolor
</button>
</a> <a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
5.6
</h1>
virginica
</button>
</a>
</h2>

</a> <a>
<h1>
</h1>
<h2>
<a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
1.5 <sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;"> <i class="glyphicon glyphicon-chevron-down" style="font-size: 10px; vertical-align: top;"></i> 50% </sup>
</h1>
setosa
</button>
</a> <a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
4.3 <sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;"> <i class="glyphicon glyphicon-chevron-up" style="font-size: 10px; vertical-align: top;"></i> 7.5% </sup>
</h1>
versicolor
</button>
</a> <a>
<button class="btn btn-2 btn-danger" role="button" type="danger">
<h1>
5.6 <sup style="font-size: 12px;color:#EEEEEE;vertical-align: top;"> <i class="glyphicon glyphicon-chevron-up" style="font-size: 10px; vertical-align: top;"></i> 12% </sup>
</h1>
virginica
</button>
</a>
</h2>

</a>
</body>
</html>
<!--/html_preserve-->
