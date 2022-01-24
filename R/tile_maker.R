#' ico
#'
#' Auxiliary function to generate icons
#'
#' @param x Icon name. See https://getbootstrap.com/docs/3.3/components/
#' @param chevron binary to denote whether there is a former value to compare against or not.
#' @importFrom htmltools tags browsable
#' @rdname ico
#' @export
ico <- function(x, chevron = FALSE) {
  if (is.null(x)) {
    NULL
  } else if (chevron == FALSE) {
    tags$i(class = "glyphicon", class = paste0("glyphicon-", x))
  } else if (chevron == TRUE) {
    tags$i(
      class = "glyphicon",
      class = paste0("glyphicon-", x),
      style = "font-size: 10px; vertical-align: top;"
    )
  }
}


#' solo_box
#'
#' This function crafts the actual tile per se, including the specific aesthetic
#' traits for each tile. This is the simple version where you explicitly state
#' the color.
#'
#' @param value The numeric value you want to highlight (the main enchilada)
#' @param former The numeric old value to use for comparison to 'value'
#' @param txt Optional subtext that should appear under the value
#' @param size Optional size specified in the bootstrap css classes:
#'   "xs","sm","md","lg")
#' @param icon Optional glyphicon that should be displayed from
#'   https://getbootstrap.com/docs/3.3/components/ you need only supply the name
#'   of thing you want, like "check"... not the full "gyphicon-check"
#' @param color Optional bootstrap css element that governs the color.
#'   https://v4-alpha.getbootstrap.com/utilities/colors/ Choose from: "muted",
#'   "primary", "success", "info", "warning", "danger"
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests
#'   their mouse over the tile.
#' @param textModifier Optional css category of "large" text. In this case, the
#'   icon, value and unit. In this case, title. Default=h1
#' @param ... Optional additional html elements
#' @importFrom htmltools HTML tag tags
#' @examples
#' b1 <- solo_box(color = "warning", value = 3.57, txt = "B")
#' b2 <- solo_box(color = "danger", value = 13.7, txt = "Nutritional value")
#' b3 <- solo_box(color = "success", value = 1, txt = "Yumminess factor")
#' b4 <- solo_box(value = 3.57, former = 3, txt = "Times apple eaten", icon = "apple")
#' finisher(title = "straight buttons", divs = b1)
#' finisher(
#'   title = "with divs",
#'   divs = div_maker(
#'     subtitle = "boom",
#'     textModifier = "h1",
#'     div_maker(subtitle = "Boom", textModifier = "hi", b1, b2, b3)
#'   )
#' )
#'
#' ## Or taking advantage of the ability to change the textModifier:
#' finisher(
#'   title = "h4 modifier",
#'   divs = solo_box(
#'     value = 3, txt = "uh huh",
#'     former = 2, textModifier = "h4"
#'   )
#' )
#' @export solo_box
solo_box <- function(value = NULL, txt = NULL, former = NULL, size = "md",
                     icon = NULL, color = "info", link = NULL, units = NULL,
                     hover = NULL, textModifier = "h1", ...) {
  tags$a(
    href = link,
    tags$button(
      title = hover,
      # color= "button",
      color = color,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size), class = paste0("btn-", color),
      if (!(is.null(value) & is.null(units) & is.null(icon))) {
        tag(textModifier, tags$span(
          ico(icon), value, units,
          if (!is.null(former)) {
            if (former > value) {
              tags$sup(
                style = "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                ico("chevron-down", chevron = TRUE),
                paste(round((as.numeric(former) - as.numeric(value)) /
                  as.numeric(former) * 100, 1), "%", sep = "")
              )
            } else {
              tags$sup(
                style = "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                ico("chevron-up", chevron = TRUE),
                paste(round((as.numeric(value) - as.numeric(former)) /
                  as.numeric(former) * 100, 1), "%", sep = "")
              )
            }
          }
        )$children)
      },
      HTML(txt),
      ...
    )
  )
}


#' solo_gradient_box
#'
#' This function crafts a solo_box tile displaying a red orange green color. The
#' color is defined by the value of the target compared to the thresholds.
#'
#' @param value The numeric value you want to highlight (the main enchilada)
#' @param txt Optional subtext that should appear under the value
#' @param former The last value that should be used as information in the
#'   chevron, or for relative mode
#' @param size Optional size specified in the bootstrap css classes:
#'   "xs","sm","md","lg")
#' @param icon Optional glyphicon that should be displayed from
#'   https://getbootstrap.com/docs/3.3/components/ you need only supply the name
#'   of thing you want, like "check"... not the full "gyphicon-check"
#' @param target Optional target that the value should be compared against. Use
#'   with thresholdHigh and thresholdLow. Note, `target` is ignored in relative
#'   mode, and you might want to change the thresholdHigh to 105 and threholdLow
#'   to 95 (to trigger red/green if +/- 5% outside the margins)
#' @param thresholdHigh Optional edge between \"green\" and \"orange\" from
#'   0-100 as a percent of target. IE, this value represents the RATIO of the
#'   VALUE to the target that, if above or equal to the thresholdHigh will show
#'   as green, and if not, as orange. Use w/ target and thresholdLow.
#' @param thresholdLow Optional border between \"orange\" and \"red\" from 0-100
#'   as a percent of target. IE, this value represents the RATIO of the VALUE to
#'   the target that, if above or equal to the ThresholdLow will show as orange,
#'   and if not, as red. Use w/ target and thresholdHigh.
#' @param relative Alternate mode where the `value` is compared against `former`
#'   rather than `target`. This mode is suitable to change the color of the
#'   button based on previous values rather than comparison to a standard.
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests
#'   their mouse over the tile.
#' @param hide_value Optionally and paradoxically hide value. Normally FALSE,
#'   change this value to TRUE in order to suppress the large number, but still
#'   take advantage of the conditional formatting.
#' @param textModifier Optional css category of "large" text. In this case, the
#'   icon, value and unit. Default=h1
#' @param revert Invert colorbox. Green become red and red become green.
#' @param ... Optional additional html elements. For example, if you would like
#'   two buttons to fit into a section in a flexdashboard, you could specify
#'   `style = 'width:100%;height:50%'`
#' @importFrom htmltools HTML tags tag
#' @examples
#' # ADD EXAMPLES HERE
#' g1 <- solo_gradient_box(value = 40)
#' g2 <- solo_gradient_box(
#'   value = 40, target = 50,
#'   thresholdHigh = 80, thresholdLow = 60
#' )
#' g3 <- solo_gradient_box(
#'   value = 20, txt = "Test1", target = 50,
#'   thresholdHigh = 80, thresholdLow = 60, hide_value = TRUE
#' )
#' g4 <- solo_gradient_box(
#'   value = 35, txt = "Test2", target = 50,
#'   thresholdHigh = 80, thresholdLow = 60, hide_value = TRUE
#' )
#' ## This one shows relative and revert options. Since 29160
#' is about 6% higher than 27420, it is triggered by the "high"
#' level, but since revert is TRUE, insteaad of showing as
#' green, it's showing as red.
#' g5 <- solo_gradient_box(
#'   value = 29160, former = 27420,
#'   relative = TRUE, revert = TRUE,
#'   thresholdHigh = 105, thresholdLow = 95
#' )
#' finisher(title = "Item", divs = div_maker(
#'   subtitle = "subitems",
#'   textModifier = "h1", g1, g2, g3, g4, g5
#' ))
#' @export solo_gradient_box
solo_gradient_box <- function(value = NULL, txt = NULL, former = NULL,
                              size = "md", icon = NULL, target = 100,
                              thresholdHigh = 90, thresholdLow = 50,
                              relative = FALSE, link = NULL, units = NULL,
                              hover = NULL, hide_value = FALSE,
                              textModifier = "h1", revert = FALSE, ...) {
  if (relative == FALSE) {
    Perc <- value / target * 100
  } else {
    Perc <- value / former * 100
  }

  if (Perc >= thresholdHigh) {
    if (revert == FALSE) finalcolor <- "success" else finalcolor <- "danger"
  } else if (Perc < thresholdLow) {
    if (revert == FALSE) finalcolor <- "danger" else finalcolor <- "success"
  } else {
    finalcolor <- "warning"
  }

  tags$a(
    href = link,
    tags$button(
      href = link,
      title = hover,
      # color= "button",
      color = finalcolor,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size),
      class = paste0("btn-", finalcolor),
      if (hide_value == FALSE) {
        tag(textModifier, tags$span(
          ico(icon), value, units,
          if (!is.null(former)) {
            if (former > value) {
              tags$sup(
                style = "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                ico("chevron-down", chevron = TRUE),
                paste(round((as.numeric(former) -
                  as.numeric(value)) /
                  as.numeric(former) * 100, 1), "%", sep = "")
              )
            } else {
              tags$sup(
                style = "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                ico("chevron-up", chevron = TRUE),
                paste(round((as.numeric(value) -
                  as.numeric(former)) /
                  as.numeric(former) * 100, 1),
                "%",
                sep = ""
                )
              )
            }
          }
        )$children)
      },
      HTML(txt),
      ...
    )
  )
}


#' @title multi_box
#' @description Create a tile that contains more than one value, icon and text
#' @param icons vector of Icons to display, Default: NULL
#' @param txt Optional subtext that should appear under the value
#' @param values vector of values to display, Default: NULL
#' @param title Top title, Default: NULL
#' @param size Optional size specified in the bootstrap css classes:
#'   "xs","sm","md","lg")
#' @param color Optional bootstrap css element that governs the color.
#'   https://v4-alpha.getbootstrap.com/utilities/colors/ Choose from: "muted",
#'   "primary", "success", "info", "warning", "danger", Default: 'info'
#' @param link Optional hyperlink to redirect to after a user click, Default:
#'   NULL
#' @param number_zoom Optional magnification \% for number vs normal text,
#'   Default: 150
#'
#' @param hover Optional tooltip, or text that will show up when a user rests their
#' mouse over the tile, Default: NULL
#' @param ... add any other html code here
#' @importFrom purrr pmap
#' @importFrom htmltools HTML span a tags
#' @return an HTML object
#' @details Allows for each button to contain several icon-number-text descriptions.
#' @examples
#' library(dplyr)
#' multi_box(
#'   values = c(21, 45), title = "Important <br>button",
#'   number_zoom = 300, icons = c("apple", "calendar"), color = "warning",
#'   txt = c("times", "reports")
#' ) %>%
#'   finisher(divs = .)
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname multi_box
#' @export
multi_box <- function(icons = NULL, txt = NULL, values = NULL,
                      title = NULL, size = "md",
                      color = "info", link = NULL, number_zoom = 150,
                      hover = NULL, ...) {
  ## Define function that can be pmapped
  gutsMaker <- function(values, txt, icons) {
    tags$h3(
      ico(icons),
      span(values, style = paste("font-size:", number_zoom, "%", sep = "")),
      txt
    )
  }
  ## Protect gainst empty values icons or text
  if (is.null(txt)) txt <- rep(" ", length(values))
  if (is.null(icons)) icons <- rep(" ", length(values))

  ## Now build button
  tags$a(
    href = link,
    tags$button(
      href = link,
      title = hover,
      # color= "button",
      color = color,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size), class = paste0("btn-", color),
      tags$h1(HTML(title)),
      pmap(list(values, txt, icons), gutsMaker)
    )
  )
}


#' tileMatrix
#'
#' Create a matrix of buttons suitable for quick comparisons
#'
#' @param data a dataframe containing the data you would like to plot
#' @param values a Vector containing values for each tile, contained in the
#'   dataframe `data`
#' @param txt Vector containing titles for each tile, contained in the datframe
#'   `data`
#' @param icon Optional glyphicon that should be displayed from
#'   https://getbootstrap.com/docs/3.3/components/ you need only supply the name
#'   of thing you want, like "check"... not the full "gyphicon-check"
#' @param former optional vector containing former values (to show change from
#'   last), contained in the datframe `data`
#' @param target Optional target that the value should be compared against. Use
#'   with thresholdHigh and THresholdLow
#' @param thresholdHigh Optional edge between \"green\" and \"orange\" from
#'   0-100 as a percent of target. IE, this value represents the RATIO of the
#'   VALUE to the target that, if above or equal to the thresholdHigh will show
#'   as green, and if not, as orange. Use w/ target and thresholdLow.
#' @param thresholdLow Optional border between \"orange\" and \"red\" from 0-100
#'   as a percent of target. IE, this value represents the RATIO of the VALUE to
#'   the target that, if above or equal to the ThresholdLow will show as orange,
#'   and if not, as red. Use w/ target and thresholdHigh.
#' @param cols Number of columns that the matrix should tile around. Defaults to
#'   4
#' @param title The title the matrix should have.
#' @param roundVal Number of decimals that Value will be rounded to. Defaults to
#'   1
#' @param textModifier Optional css category of "large" text. In this case, the
#'   icon, value and unit. Default=h1
#' @importFrom htmltools HTML tag tags
#' @importFrom dplyr pull %>%  data_frame
#' @importFrom rlang !! enquo syms
#' @return Returns a list object containing the matrix of buttons
#' @examples
#' finisher(title = "Tile Matrix", divs = tile_matrix(
#'   data = head(iris),
#'   values = Sepal.Length,
#'   txt = Species
#' ))
#' @export tile_matrix
tile_matrix <- function(data, values, txt, icon, former, target = 100,
                        thresholdHigh = 90, thresholdLow = 50, cols = 4,
                        title = NULL, roundVal = 1, textModifier = "h1") {


  # browser()

  ## Prep the NSE of the inputnames
  v <- enquo(values)
  t <- enquo(txt)
  f <- enquo(former)
  i <- enquo(icon)

  ## Now push them back into original names
  ifelse(!missing(values),
    values <- pull(data, !!v),
    values <- rep(NA, nrow(data))
  )
  ifelse(!missing(txt),
    txt <- pull(data, !!t) %>% as.character(),
    txt <- rep(NA, nrow(data))
  )
  ifelse(!missing(former),
    former <- pull(data, !!f),
    former <- rep(NA, nrow(data))
  )
  ifelse(!missing(icon),
    icon <- pull(data, !!i),
    icon <- rep(NA, nrow(data))
  )

  ## Errors
  if (class(values) != "numeric" & class(values) != "integer") {
    stop(
      "values should be numeric"
    )
  }


  ## Clean inputs
  values <- round(values, roundVal)

  ## Remake df and start adding extra stuffs
  df <- data_frame(txt, values, former, icon)

  df$id <- 1:nrow(df)
  df$butts <- list("")

  # df$stuff <- str_trunc(df$stuff,min(str_length(df$stuff)),side="right")

  ## protect against NAs
  if (any(is.na(df$values))) {
    df$values[is.na(df$values)] <- 0.001
    warning("Converted NAs in values to 0.001")
  }
  # browser()
  ## Need to protect against some NAs, but not if all of them are
  if (any(is.na(former)) & !all(is.na(former))) {
    # browser()
    df$former[is.na(df$former)] <- 0.001
    warning("Converted NAs in former to 0.001")
  }

  for (i in seq_along(1:nrow(df))) {
    ## do the top one if there's any formers, otherwise do the other
    if (any(!is.na(former))) {
      df$butts[[i]] <- solo_gradient_box(
        value = df$values[i], txt = df$txt[i],
        size = 2, target = target, thresholdHigh = thresholdHigh,
        thresholdLow = thresholdLow, former = df$former[i]
      )
    } else {
      df$butts[[i]] <- solo_gradient_box(
        value = df$values[i], txt = df$txt[i],
        size = 2, target = target, thresholdHigh = thresholdHigh,
        thresholdLow = thresholdLow
      )
    }
  }


  Sausage <- df$butts

  # ## Break the button sausage every cols
  splitter <- function(Sausage, cols) {
    Outputter <- list("")
    for (i in 1:ceiling(length(Sausage) / cols)) {
      Outputter[[i]] <- div_maker(Sausage[((i - 1) * cols + 1):(cols * i)],
        textModifier = "h2"
      )
    }
    Outputter
  }

  tags$a(
    tags$h1(HTML(title)),
    splitter(Sausage, cols)
  )
}


#' Div maker
#'
#' This function takes buttons made by any of the solo or multi buttons and
#' makes an a row (HTML `div`) suitable for inclusion in other HTML code, or for
#' inclusion within the function of this package `finisher`.
#'
#' @param subtitle The text heading of this row of buttons
#' @param textModifier Optional css category of "large" text. In this case,
#'   subtitle. Use css flags
#' like "h2", "h3","p", etc. Default = "h1"
#' @param ... \code{buttons to insert into the div} elements.
#' @examples
#' div_maker(
#'   subtitle = "Quantativity factors", textModifier = "h1",
#'   solo_gradient_box(value = 70),
#'   solo_box(value = 34)
#' )
#' @export div_maker
div_maker <- function(subtitle, textModifier, ...) {
  tags$div(
    class = "container",
    tag(textModifier, tags$span(subtitle)$children),
    ...
  )
}

# div_maker <- function(title = NULL, ...) {
#
#   tags$div(
#     class = "container",
#     tags$h2(title),
#     ...
#   )
#
# }

#' finisher
#'
#' Function 3 of 3, the last step. This function grabs the Divs created by
#' `DivMaker`, or individual buttons if so desired, and combines them into a
#' freestanding html file. Use this when you don't want the buttons to be part
#' of a file, but a file itself. or, you could also use this as a convenient way
#' of wrapping up buttons without using a div (although it is a bit irregular).
#'
#' @param title Title. Default NULL
#' @param css A string indicating css url, for final installations pls save the
#'   css file locally. By default we are using the 3.3.7 bootstrap CDN because
#'   they support icons, but some others that might be interesting to you are:
#'   https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css or
#'   https://bootswatch.com/4/flatly/bootstrap.css (but if you use version 4 you
#'   will lose the ability to display icons).
#' @param file Optional filename if you desire to save the file.
#' @param textModifier Optional css category of "large" text. In this case,
#'   title. Default=h1
#' @param divs \code{div_maker} elements.
#' @importFrom htmltools browsable save_html
#' @export finisher
finisher <- function(title = NULL, css =
                       "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
                     file = NULL, textModifier = "h1", divs) {
  divs[[2]] <- NULL
  tl <- tags$html(
    tags$head(
      tags$link(rel = "stylesheet", href = css)
    ),
    tags$body(
      tag(textModifier, tags$span(title)$children),
      divs
    )
  )

  if (is.null(file)) {
    browsable(tl)
  } else {
    save_html(tl, file = file)
  }
}
