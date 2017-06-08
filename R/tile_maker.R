# library(htmltools)
#' Tags
#' @importFrom htmltools tags
#' @name tags
#' @export
#' @rdname TileMaker-exports
NULL

#' Auxiliar function to generate icons
#' @param x Icon name.
#' @export
ico <- function(x) {

  if(is.null(x)) NULL else tags$i(class = "glyphicon", class = paste0("glyphicon-", x))

}

#' Button maker
#' @param value .
#' @param subtitle .
#' @param size .
#' @param icon .
#' @param type .
#' @param link .
#' @param units .
#' @param hover .
#' @export
button_maker <- function(value = NULL, subtitle = NULL, size = "md", icon = NULL,
                         type = "warning", link = NULL, units = NULL, hover = NULL) {

  tags$a(
    href = link,
    title = hover,
    type = "button", role = "button",
    # classes: size, color
    class = "btn", class = paste0("btn-", size), class = paste0("btn-", type),
    tags$h1(ico(icon), value, units),
    subtitle
  )

}

#' Div maker
#' @param title Title.
#' @param ... \code{button_maker} elements.
#' @export
div_maker <- function(title = NULL, ...) {

  tags$div(
    class = "container",
    tags$h2(title),
    ...
  )

}

#' Tile maker
#' Tile maker
#' @param title Title.
#' @param ... \code{div_maker} elements.
#' @param css A string indicating css url
#' @importFrom htmltools browsable
#' @export
tile_maker <- function(title = NULL, ..., css = "https://bootswatch.com/flatly/bootstrap.css") {

  tl <- tags$html(
    tags$head(
      tags$link(rel = "stylesheet", href = css)
    ),
    tags$body(
      tags$h1(title),
      ...
    )
  )

  browsable(tl)

}

# ico(NULL)
# ico("apple")
#
# b1 <- button_maker(value = 3.57, subtitle = "Times apple eaten", icon = "apple")
# b2 <- button_maker(value = 13.7, subtitle = "Nutritional value", size = "lg")
# b3 <- button_maker(value = 1, subtitle = "Yumminess factor", type = "danger")
# b4 <- button_maker(value = 5, subtitle = "Inconsistencies", hover = "This is the description")
#
# d1 <- div_maker(title = "Quantativity factors", b1, b2)
# d2 <- div_maker(title = "Implementation procedures", b3, b4)
#
# tile_maker(title = "Hello", d1, d2)
#
# map <- leaflet::addTiles(leaflet::leaflet())
# map
#
# tile_maker(title = "Hello", d1, map, d2)



