
[![Travis-CI Build Status](https://travis-ci.org/DataStrategist/TileMaker.svg?branch=master)](https://travis-ci.org/DataStrategist/TileMaker)

TileMaker
=========

An R library that enables the creation of data tiles for inclusion in a html dashboard or some such.

To install, please type:

    devtools::install_github("DataStrategist/TileMaker", build_vignettes = TRUE)

Please see the [pkgdown site](http://datastrategist.github.io/TileMaker/) to learn more about how to do some lovely stuff, but in the meantime, here's the basics:

This package is intended to "highlight single values", mainly in dashboards, reports or Shiny apps, and is highly customizeable. This is what it looks like:

``` r
suppressPackageStartupMessages(library(dplyr,quietly = TRUE))
library(htmltools)
library(TileMaker)

a <- solo_box(value = 3, subtitle = "Little piggies<br>go to the market", icon = "piggy-bank")
b <- solo_gradient_box(value = 65, subtitle = "test score I got")
c <- solo_gradient_box(value = 95, subtitle = "test score I wanted")
d <- multi_box(values = c(4, 5, 6), txt = c("Sally", "George", "Mohammed"), icons = c("check", "plus", "calendar"), title = "Candidates")

e <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  tile_matrix(data = ., values = "a", txt = "Species")

f <- iris %>%
  group_by(Species) %>%
  summarize(a = mean(Petal.Length)) %>%
  mutate(old_a = c(3, 4, 5)) %>%
  tile_matrix(data = ., values = a, txt = Species, former =  old_a)

d1 <- div_maker(span(a, b))
d2 <- div_maker(span(c, d))
d3 <- div_maker(e, f)

finisher(
  title = "Important Reportings", css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
  file = NULL, textModifier = "h1", d1, d2
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
<button class="btn btn-md btn-info" role="button" subtitle="Little piggies&lt;br&gt;go to the market" type="info">
<h1>
<i class="glyphicon glyphicon-piggy-bank"></i> 3
</h1>
</button>
</a> <a>
<button class="btn btn-md btn-warning" role="button" subtitle="test score I got" type="warning">
<h1>
65
</h1>
</button>
</a> </span>
</h1>

<h1>
<span> <a>
<button class="btn btn-md btn-success" role="button" subtitle="test score I wanted" type="success">
<h1>
95
</h1>
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
</a> </span>
</h1>

</body>
</html>
<!--/html_preserve-->
