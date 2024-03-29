% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tile_maker.R
\name{tile_matrix}
\alias{tile_matrix}
\title{tileMatrix}
\usage{
tile_matrix(
  data,
  values,
  txt,
  icon,
  former,
  target = 100,
  thresholdHigh = 90,
  thresholdLow = 50,
  cols = 4,
  title = NULL,
  roundVal = 1,
  textModifier = "h1"
)
}
\arguments{
\item{data}{a dataframe containing the data you would like to plot}

\item{values}{a Vector containing values for each tile, contained in the
dataframe `data`}

\item{txt}{Vector containing titles for each tile, contained in the datframe
`data`}

\item{icon}{Optional glyphicon that should be displayed from
https://getbootstrap.com/docs/3.3/components/ you need only supply the name
of thing you want, like "check"... not the full "gyphicon-check"}

\item{former}{optional vector containing former values (to show change from
last), contained in the datframe `data`}

\item{target}{Optional target that the value should be compared against. Use
with thresholdHigh and THresholdLow}

\item{thresholdHigh}{Optional edge between \"green\" and \"orange\" from
0-100 as a percent of target. IE, this value represents the RATIO of the
VALUE to the target that, if above or equal to the thresholdHigh will show
as green, and if not, as orange. Use w/ target and thresholdLow.}

\item{thresholdLow}{Optional border between \"orange\" and \"red\" from 0-100
as a percent of target. IE, this value represents the RATIO of the VALUE to
the target that, if above or equal to the ThresholdLow will show as orange,
and if not, as red. Use w/ target and thresholdHigh.}

\item{cols}{Number of columns that the matrix should tile around. Defaults to
4}

\item{title}{The title the matrix should have.}

\item{roundVal}{Number of decimals that Value will be rounded to. Defaults to
1}

\item{textModifier}{Optional css category of "large" text. In this case, the
icon, value and unit. Default=h1}
}
\value{
Returns a list object containing the matrix of buttons
}
\description{
Create a matrix of buttons suitable for quick comparisons
}
\examples{
finisher(title = "Tile Matrix", divs = tile_matrix(
  data = head(iris),
  values = Sepal.Length,
  txt = Species
))
}
