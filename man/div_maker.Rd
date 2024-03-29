% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tile_maker.R
\name{div_maker}
\alias{div_maker}
\title{Div maker}
\usage{
div_maker(subtitle, textModifier, ...)
}
\arguments{
\item{subtitle}{The text heading of this row of buttons}

\item{textModifier}{Optional css category of "large" text. In this case,
  subtitle. Use css flags
like "h2", "h3","p", etc. Default = "h1"}

\item{...}{\code{buttons to insert into the div} elements.}
}
\description{
This function takes buttons made by any of the solo or multi buttons and
makes an a row (HTML `div`) suitable for inclusion in other HTML code, or for
inclusion within the function of this package `finisher`.
}
\examples{
div_maker(
  subtitle = "Quantativity factors", textModifier = "h1",
  solo_gradient_box(value = 70),
  solo_box(value = 34)
)
}
