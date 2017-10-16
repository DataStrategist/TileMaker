# library(htmltools)
#' Tags
#' @importFrom htmltools tags browsable
#' @name tags
#' @export
#' @rdname TileMaker-exports
NULL

#' Auxiliary function to generate icons
#' @param x Icon name. See http://getbootstrap.com/components/
#' @param chevron binary to denote whether there is a former value to compare against or not.
#' @export
ico <- function(x,chevron=FALSE) {

  if(is.null(x)){
    NULL
    } else if(chevron==FALSE){
      tags$i(class = "glyphicon", class = paste0("glyphicon-", x))
    } else if(chevron==TRUE) {
      tags$i(class = "glyphicon",
             class = paste0("glyphicon-", x),
             style="font-size: 10px; vertical-align: top;")
    }
}

#' solo_box
#'
#' This function crafts the actual tile per se, including the specific
#' aesthetic traits for each tile. This is the simple version where you explicitly state the color.
#'
#' @param value The numeric value you want to highlight (the main enchilada)
#' @param former The numeric old value to use for comparison to 'value'
#' @param subtitle Optional subtext that should appear under the value
#' @param size Optional size specified in the bootstrap css classes:
#' \"xs\",\"sm\",\"md\",\"lg\")
#' @param icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/
#' @param type Optional bootstrap css element that governs the color. https://v4-alpha.getbootstrap.com/utilities/colors/
#' Choose from: "Muted", "Primary", "Success", "Info", "Warning", "Danger"
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests their mouse over the tile.
#' @examples
#' tile1 <- solo_box(type="warning",value = 3.57,subtitle = "B")
#' tile2 <- solo_box(type="danger",value = 13.7,subtitle = "Nutritional value")
#' tile3 <- solo_box(type="success",value = 1,subtitle = "Yumminess factor")
#' tile4 <- solo_box(value = 3.57, former=3,subtitle = "Times apple eaten", icon = "apple")
#' file_maker(tile1,tile2,tile3,tile4)
#' file_maker(div_maker(tile1,tile2,tile3),div_maker(tile4))
#' @export solo_box
solo_box <- function(value = NULL, subtitle = NULL, former=NULL,size = "md", icon = NULL,
                    type = "info", link = NULL, units = NULL, hover = NULL) {

  tags$a(
    href = link,
    title = hover,
    # type = "button",
    type = type,
    role = "button",
    # classes: size, color
    class = "btn", class = paste0("btn-", size), class = paste0("btn-", type),
    tags$h1(ico(icon), value, units,
            if(!is.null(former)){
              if(former>value){
                tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                         ico('chevron-down',chevron = T),paste(round((former-value)/former*100,1),'%',sep=''))
              } else {
                tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                         ico('chevron-up',chevron = T),paste(round((former-value)/former*100,1),'%',sep=''))
              }
            }),
    HTML(subtitle)
  )
}


#' solo_gradient_box
#'
#' This function crafts a tile like solo_box, but this one changes color according to value
#'
#' @param value The numeric value you want to highlight (the main enchilada)
#' @param subtitle Optional subtext that should appear under the value
#' @param former The last value that should be used for comparison purposes
#' @param size Optional size specified in the bootstrap css classes:
#' \"xs\",\"sm\",\"md\",\"lg\")
#' @param icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/
#' @param target Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow
#' @param thresholdHigh Optional edge between \"green\" and \"orange\" from 0-100. Use w/ Target and ThresholdLow. This value represents the RATIO
#' of the VALUE to the TARGET that, if above the ThresholdHigh will show as green, and if not, as orange
#' @param thresholdLow Optional border between \"orange\" and \"red\" from 0-100. Use w/ Target and ThresholdLow. This value represents the RATIO
#' of the VALUE to the TARGET that, if above the ThresholdHigh will show as orange, and if not, as red
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests their mouse over the tile.
#' @examples
#' # ADD EXAMPLES HERE
#' solo_gradient_box(value = 40)
#' solo_gradient_box(value = 40,target = 50,thresholdHigh = 80, thresholdLow=60)
#'
#' @export solo_gradient_box
solo_gradient_box <- function(value = NULL, subtitle = NULL, former=NULL, size = "md", icon = NULL,
                    target=100, thresholdHigh=90, thresholdLow=50,
                    link = NULL, units = NULL, hover = NULL) {

  Perc <- value/target *100
  if(Perc >= thresholdHigh){
    finalType='btn-success'
  } else if(Perc< thresholdLow){
    finalType='btn-danger'
  } else {
    finalType='btn-warning'
  }

  tags$a(
    href = link,
    title = hover,
    # type = "button",
    type = finalType,
    role = "button",
    # classes: size, color
    class = "btn", class = paste0("btn-", size), class = finalType,
    tags$h1(ico(icon), value, units,
            if(!is.null(former)){
              if(former>value){
                tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                         ico('chevron-down',chevron = T),paste(round((former-value)/former*100,1),'%',sep=''))
              } else {
                tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                         ico('chevron-up',chevron = T),paste(round((former-value)/former*100,1),'%',sep=''))
              }
            }),
    HTML(subtitle)
  )

}

#' Div maker
#'
#' This function takes strings created with the function `ButtonMaker` and makes an HTML div suitable for inclusion in other HTML code,
#' or for inclusion within the function of this package `TileMaker`.
#'
#' @param title Title.
#' @param ... \code{solo_box OR solo_gradient_box} elements.
#' @examples
#' div_maker(title = "Quantativity factors",
#'           solo_gradient_box(value = 70),
#'           solo_box(value=34))
#' @export div_maker
div_maker <- function(title = NULL, ...) {

  tags$div(
    class = "container",
    tags$h2(title),
    ...
  )

}


#' file maker
#'
#' Function 3 of 3, the last step. This function grabs the Divs created by `DivMaker` and
#' combines them into an html file.
#'
#' @param title Title.
#' @param ... \code{div_maker} elements.
#' @param css A string indicating css url
#' @param file Optional filename if you desire to save the file. Should end with ".html"
#' @importFrom htmltools browsable save_html
#' @export file_maker
file_maker <- function(title = NULL, ..., css = "https://bootswatch.com/flatly/bootstrap.css",
                       file=NULL) {

  tl <- tags$html(
    tags$head(
      tags$link(rel = "stylesheet", href = css)
    ),
    tags$body(
      tags$h1(title),
      ...
    )
  )

  if (is.null(file)) {
    browsable(tl)
  } else {
    save_html(tl,file = file)
  }
}


#' tileMatrix
#'
#' Create a matrix of buttons suitable for quick comparisons
#'
#' @param values Vector containing values for each tile
#' @param former vector containing former values (to show change from last)
#' @param titles Vector containing titles for each tile
#' @param tar Target value (What's the highest value to compare against). Defaults to 100
#' @param thre.H The limit between "high" and "medium" values IN PERCENT. Defaults to 90
#' @param thre.L The limit between "medium" and "low" values IN PERCENT. Defaults to 50
#' @param cols Number of columns that the matrix should tile around. Defaults to 4
#' @param mainTitle The title the matrix should have.
#' @param roundVal Number of decimals that Value will be rounded to. Defaults to 1
#' @importFrom htmltools HTML
#'
#' @return Returns a list object containing the matrix of buttons
#' @examples
#' file_maker(tile_matrix(values=c(3,4,5),titles=c("Bob","Pedro","Ana")))
#' @export tile_matrix
tile_matrix <- function(values,subtitles,former=NULL,tar=100,thre.H=90,thre.L=50,cols=4,
                       mainTitle=NULL,roundVal=1){

  ## Errors
  if(class(values) != "numeric"&class(values) != "integer") stop("values should be numeric")
  if(class(subtitles) == "factor") subtitles <- as.character(subtitles)
  if(class(subtitles) != "character") stop("subtitles should be a character vector")
  if(length(values) != length(subtitles)) stop("values and subtitles vectors should be the same length, but they are not.")
  if(!is.null(former) & length(values) != length(former)) stop("'values' and 'former' vectors should be the same length, but they are not.")


  ## Clean inputs
  values <- round(values,roundVal)

  ## Make df and start adding extra stuffs
  if (is.null(former)){
    df <- data.frame(subtitles,values)
  } else{
    df <- data.frame(subtitles,values,former)
  }

  df$id <- 1:nrow(df)
  df$butts <- list("")

  # df$stuff <- str_trunc(df$stuff,min(str_length(df$stuff)),side="right")

  ## protect against NAs
  if(any(is.na(df$value))){
    df$value[is.na(df$value)] <-0.001
    warning("Converted NAs in values to 0.001")
  }
  if(!is.null(former) ) {
    df$former[is.na(df$former)] <-0.001
    warning("Converted NAs in former to 0.001")
  }

  for(i in 1:nrow(df)){
      if(!is.null(former)){
        df$butts[[i]] <- solo_gradient_box(value = df$value[i],subtitle = df$title[i],
                                         size = 2,target=tar,thresholdHigh = thre.H,
                                   thresholdLow = thre.L,former=df$former[i])
      } else {
        df$butts[[i]] <- solo_gradient_box(value = df$value[i],subtitle = df$title[i],
                                           size = 2,target=tar,thresholdHigh = thre.H,
                                           thresholdLow = thre.L)
      }
  }

  Sausage = df$butts

  # ## Break the button sausage every cols
  splitter <- function(Sausage,cols){
    Outputter <- list("")
    for (i in 1:ceiling(length(Sausage)/cols)){
      Outputter[[i]] <- div_maker(Sausage[((i-1)*cols+1):(cols*i)])
    }
    Outputter
  }

  tags$a(
    tags$h1(HTML(mainTitle)),
    splitter(Sausage,cols)
  )
}

#' @title multi_box
#' @description Create a tile that contains more than one value, icon and units
#' @param MB_values vector of values to display, Default: NULL
#' @param mainTitle Top title, Default: NULL
#' @param size Optional size specified in the bootstrap css classes:
#' \"xs\",\"sm\",\"md\",\"lg\")
#' @param MB_icons vector of Icons to display, Default: NULL
#' @param type Optional bootstrap css element that governs the color. https://v4-alpha.getbootstrap.com/utilities/colors/
#' Choose from: "Muted", "Primary", "Success", "Info", "Warning", "Danger", Default: 'info'
#' @param link Optional hyperlink to redirect to after a user click, Default: NULL
#' @param MB_units vector of Units that explain the values, Default: NULL
#' @param hover Optional tooltip, or text that will show up when a user rests their
#' mouse over the tile, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' multi_box(MB_values=c(3,45),mainTitle = "Important <br>button",MB_icons=c("apple","calendar"),
#' type="warning", MB_units=c("times","reports")) %>% file_maker
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname multi_box
#' @export

multi_box <- function(MB_values = NULL, mainTitle = NULL ,size = "md", MB_icons = NULL,
                     type = "info", link = NULL, MB_units = NULL, hover = NULL) {
  ## Define function that can be pmapped
  gutsMaker <- function(MB_values,MB_units,MB_icons){
    tags$h3(ico(MB_icons), MB_values, MB_units)
  }

  ## Now build button
  tags$a(
    href = link,
    title = hover,
    # type = "button",
    type = type,
    role = "button",
    # classes: size, color
    class = "btn", class = paste0("btn-", size), class = paste0("btn-", type),
    tags$h1(HTML(mainTitle)),
    pmap(list(MB_values,MB_units,MB_icons),gutsMaker)
  )
}


# library(dplyr)
# library(htmltools)
# a <- tile_matrix(values = iris$Sepal.Length,titles = iris$Species,tar = 5,thre.H = 4,thre.L = 3)

# ico(NULL)
# ico("apple")
#
# b1 <- solo_box(value = 3.57, subtitle = "Times apple eaten", icon = "apple")
# b2 <- solo_box(value = 13.7, subtitle = "Nutritional value", size = "lg")
# b3 <- solo_box(value = 1, subtitle = "Yumminess factor", type = "danger")
# b4 <- solo_box(value = 5, subtitle = "Inconsistencies", hover = "This is the description")
#
# d1 <- div_maker(title = "Quantativity factors", b1, b2)
# d2 <- div_maker(title = "Implementation procedures", b3, b4)
#
# file_maker(title = "Hello", b1, b2)
#
# map <- leaflet::addTiles(leaflet::leaflet())
# map
#
# file_maker(title = "Hello", d1, map, d2)
#
#
# ## Shinydashboard Test --------------
# ## app.R ##
# library(shinydashboard)
# library(shiny)
#
# ui <- dashboardPage(
#   dashboardHeader(title = "Basic dashboard"),
#   dashboardSidebar(),
#   dashboardBody(
#     # Boxes need to be put in a row (or column)
#     fluidRow(
#       box(plotOutput("plot1", height = 250)),
#
#       box(
#         title = "Controls",
#         d1,
#         d2,
#         sliderInput("slider", "Number of observations:", 1, 100, 50)
#       ),
#       box(d1),
#       box(d2),
#       box(d1,d2)
#     )
#   )
# )
#
# server <- function(input, output) {
#   set.seed(122)
#   histdata <- rnorm(500)
#
#   output$plot1 <- renderPlot({
#     data <- histdata[seq_len(input$slider)]
#     hist(data)
#   })
# }
#
# shinyApp(ui, server)
#
# ## shinyTest ---------
# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     hist(rnorm(input$obs), col = 'darkgray', border = 'white')
#   })
# }
#
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
#     ),
#     mainPanel(d1,d2,plotOutput("distPlot"),
#               fluidRow(d1,d2))
#   )
# )
#
# shinyApp(ui = ui, server = server)



