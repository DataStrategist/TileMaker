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
#' @param text Optional subtext that should appear under the value
#' @param size Optional size specified in the bootstrap css classes:
#' "xs","sm","md","lg")
#' @param icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/ you need only supply
#' the name of thing you want, like "check"... not the full "gyphicon-check"
#' @param type Optional bootstrap css element that governs the color. https://v4-alpha.getbootstrap.com/utilities/colors/
#' Choose from: "Muted", "Primary", "Success", "Info", "Warning", "Danger"
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests their mouse over the tile.
#' @param textModifier Optional css category of "large" text. In this case, the icon, value and unit.
#' In this case, title. Default=h1
#' @param ... Optional additional html elements
#' @importFrom htmltools HTML tag
#' @examples
#' b1 <- solo_box(type="warning",value = 3.57,text = "B")
#' b2 <- solo_box(type="danger",value = 13.7,text = "Nutritional value")
#' b3 <- solo_box(type="success",value = 1,text = "Yumminess factor")
#' b4 <- solo_box(value = 3.57, former=3,text = "Times apple eaten", icon = "apple")
#' finisher(b1,b2,b3,b4)
#' finisher(div_maker(b1,b2,b3),div_maker(b4))
#'
#' ## Or taking advantage of the ability to change the textModifier:
#' finisher(solo_box(value = 3,text="uh huh",former = 2,textModifier = "h4"))
#' @export solo_box
solo_box <- function(value = NULL, text = NULL, former=NULL,size = "md", icon = NULL,
                    type = "info", link = NULL, units = NULL, hover = NULL, textModifier="h1",...) {
  tags$a(
    href = link,
    tags$button(
      title = hover,
      # type = "button",
      type = type,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size), class = paste0("btn-", type),
      tag(textModifier,tags$span(ico(icon), value, units,
              if(!is.null(former)){
                if(former>value){
                  tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                           ico('chevron-down',chevron = T),paste(round((as.numeric(former)-as.numeric(value))/as.numeric(former)*100,1),'%',sep=''))
                } else {
                  tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                           ico('chevron-up',chevron = T),paste(round((as.numeric(former)-as.numeric(value))/as.numeric(former)*100,1),'%',sep=''))
                }
              })$children),
      HTML(text),
      ...
    )
  )
}


#' solo_gradient_box
#'
#' This function crafts a tile like solo_box, but this one changes color according to value
#'
#' @param value The numeric value you want to highlight (the main enchilada)
#' @param text Optional subtext that should appear under the value
#' @param former The last value that should be used for comparison purposes
#' @param size Optional size specified in the bootstrap css classes:
#' "xs","sm","md","lg")
#' @param icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/ you need only supply
#' the name of thing you want, like "check"... not the full "gyphicon-check"
#' @param target Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow
#' @param thresholdHigh Optional edge between \"green\" and \"orange\" from 0-100 as a percent of target. IE, this value
#' represents the RATIO of the VALUE to the target that, if above or equal to the ThresholdHigh will show as green,
#' and if not, as orange. Use w/ target and ThresholdLow.
#' @param thresholdLow Optional border between \"orange\" and \"red\" from 0-100 as a percent of target. IE, this value
#' represents the RATIO of the VALUE to the target that, if above or equal to the ThresholdLow will show as orange,
#' and if not, as red. Use w/ target and ThresholdHigh.
#' @param link Optional hyperlink that should be followed on click
#' @param units Optional units that should be displayed after Value
#' @param hover Optional tooltip, or text that will show up when a user rests their mouse over the tile.
#' @param hide_value Optionally and paradoxically hide value. Normally FALSE, change this value to TRUE in order to suppress
#' the large number, but still take advantage of the conditional formatting.
#' @param textModifier Optional css category of "large" text. In this case, the icon, value and unit. Default=h1
#' @param ... Optional additional html elements
#' @importFrom htmltools HTML
#' @examples
#' # ADD EXAMPLES HERE
#' g1 <- solo_gradient_box(value = 40)
#' g2 <- solo_gradient_box(value = 40,target = 50,thresholdHigh = 80, thresholdLow=60)
#' g3 <- solo_gradient_box(value = 20,text="Test1",target = 50,thresholdHigh = 80, thresholdLow=60,hide_value=T)
#' g4 <- solo_gradient_box(value = 35,text="Test2",target = 50,thresholdHigh = 80, thresholdLow=60,hide_value=T)
#' finisher(div_maker(g1,g2,g3,g4))
#' @export solo_gradient_box
solo_gradient_box <- function(value = NULL, text = NULL, former=NULL, size = "md", icon = NULL,
                    target=100, thresholdHigh=90, thresholdLow=50,
                    link = NULL, units = NULL, hover = NULL, hide_value=FALSE, textModifier="h1",...) {

  Perc <- value/target *100
  if(Perc >= thresholdHigh){
    finalType='success'
  } else if(Perc< thresholdLow){
    finalType='danger'
  } else {
    finalType='warning'
  }

  tags$a(
    href = link,
    tags$button(
      href = link,
      title = hover,
      # type = "button",
      type = finalType,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size), class = paste0("btn-", finalType),
      if(hide_value==FALSE){
        tag(textModifier,tags$span(ico(icon), value, units,
              if(!is.null(former)){
                if(former>value){
                  tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                           ico('chevron-down',chevron = T),paste(round((as.numeric(former)-as.numeric(value))/as.numeric(former)*100,1),'%',sep=''))
                } else {
                  tags$sup(style= "font-size: 12px;color:#EEEEEE;vertical-align: top;",
                           ico('chevron-up',chevron = T),paste(round((as.numeric(former)-as.numeric(value))/as.numeric(former)*100,1),'%',sep=''))
                }
              }
              )$children)},
      HTML(text),
      ...
    )
  )
}


#' @title multi_box
#' @description Create a tile that contains more than one value, icon and text
#' @param icons vector of Icons to display, Default: NULL
#' @param text Optional subtext that should appear under the value
#' @param values vector of values to display, Default: NULL
#' @param title Top title, Default: NULL
#' @param size Optional size specified in the bootstrap css classes:
#' "xs","sm","md","lg")
#' @param type Optional bootstrap css element that governs the color. https://v4-alpha.getbootstrap.com/utilities/colors/
#' Choose from: "Muted", "Primary", "Success", "Info", "Warning", "Danger", Default: 'info'
#' @param link Optional hyperlink to redirect to after a user click, Default: NULL
#' @param number_zoom Optional magnification \% for number vs normal text, Default: 150
#'
#' @param hover Optional tooltip, or text that will show up when a user rests their
#' mouse over the tile, Default: NULL
#' @param ... add any other html code here
#' @importFrom purrr pmap
#' @importFrom htmltools HTML span
#' @return an HTML object
#' @details Allows for each button to contain several icon-number-text descriptions.
#' @examples
#' library(dplyr)
#' multi_box(values=c(3,45),title = "Important <br>button",number_zoom=300,
#' icons=c("apple","calendar"), type="warning", text=c("times","reports")) %>%
#' file_maker
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname multi_box
#' @export
multi_box <- function(icons = NULL,text=NULL,values = NULL,
                      title = NULL ,size = "md",
                      type = "info", link = NULL,number_zoom=150, hover = NULL,...) {
  ## Define function that can be pmapped
  gutsMaker <- function(values,text,icons){
    tags$h3(ico(icons),
            span(values,style=paste('font-size:',number_zoom,'%',sep='')),
            text)
  }
  ## Protect gainst empty values icons or text
  if(is.null(text)) text <- rep(" ",length(values))
  if(is.null(icons)) icons <- rep(" ",length(values))

  ## Now build button
  tags$a(
    href = link,
    tags$button(
      href = link,
      title = hover,
      # type = "button",
      type = type,
      role = "button",
      # classes: size, color
      class = "btn", class = paste0("btn-", size), class = paste0("btn-", type),
      tags$h1(HTML(title)),
      pmap(list(values,text,icons),gutsMaker)
    )
  )
}


#' tileMatrix
#'
#' Create a matrix of buttons suitable for quick comparisons
#'
#' @param data a dataframe containing the data you would like to plot
#' @param values a Vector containing values for each tile, contained in the datframe `data`
#' @param text Vector containing titles for each tile, contained in the datframe `data`
#' @param icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/ you need only supply
#' the name of thing you want, like "check"... not the full "gyphicon-check"
#' @param former optional vector containing former values (to show change from last), contained in the datframe `data`
#' @param target Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow
#' @param thresholdHigh Optional edge between \"green\" and \"orange\" from 0-100 as a percent of target. IE, this value
#' represents the RATIO of the VALUE to the target that, if above or equal to the ThresholdHigh will show as green,
#' and if not, as orange. Use w/ target and ThresholdLow.
#' @param thresholdLow Optional border between \"orange\" and \"red\" from 0-100 as a percent of target. IE, this value
#' represents the RATIO of the VALUE to the target that, if above or equal to the ThresholdLow will show as orange,
#' and if not, as red. Use w/ target and ThresholdHigh.
#' @param cols Number of columns that the matrix should tile around. Defaults to 4
#' @param title The title the matrix should have.
#' @param roundVal Number of decimals that Value will be rounded to. Defaults to 1
#' @param textModifier Optional css category of "large" text. In this case, the icon, value and unit. Default=h1
#' @importFrom htmltools HTML
#' @importFrom dplyr pull
#' @importFrom rlang !! enquo UQ
#' @return Returns a list object containing the matrix of buttons
#' @examples
#' finisher(tile_matrix(values=c(3,4,5),text=c("Bob","Pedro","Ana")))
#' @export tile_matrix
tile_matrix <- function(data,values=NULL,text=NULL,icon=NULL,former=NULL,target=100,thresholdHigh=90,thresholdLow=50,cols=4,
                        title=NULL,roundVal=1, textModifier="h1"){
# browser()
  ## Prep the NSE of the inputnames
  v <- enquo(values)
  t <- enquo(text)
  f <- enquo(former)
  i <- enquo(icon)

  ## Now push them back into original names
  if (as.character(UQ(v))[2]!="." & as.character(UQ(v))[2]!="NULL") values <- pull(data,!!v)
  if (as.character(UQ(t))[2]!="." & as.character(UQ(t))[2]!="NULL") text <- pull(data,!!t) %>% as.character
  if (as.character(UQ(f))[2]!="." & as.character(UQ(f))[2]!="NULL") former <- pull(data,!!f)
  if (as.character(UQ(i))[2]!="." & as.character(UQ(i))[2]!="NULL") icon <- pull(data,!!i)


  ## Errors
  if(class(values) != "numeric" & class(values) != "integer") stop("values should be numeric")
  if(length(values) != length(text)) stop("values and text vectors should be the same length, but they are not.")
  if(!is.null(former) & length(values) != length(former)) stop("'values' and 'former' vectors should be the same length, but they are not.")


  ## Clean inputs
  values <- round(values,roundVal)

  ## Make df and start adding extra stuffs
  if (is.null(former)){
    df <- data.frame(text,values,stringsAsFactors = F)
  } else{
    df <- data.frame(text,values,former,stringsAsFactors = F)
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
      df$butts[[i]] <- solo_gradient_box(value = df$value[i],text = df$text[i],
                                         size = 2,target=target,thresholdHigh = thresholdHigh,
                                         thresholdLow = thresholdLow,former=df$former[i])
    } else {
      df$butts[[i]] <- solo_gradient_box(value = df$value[i],text = df$text[i],
                                         size = 2,target=target,thresholdHigh = thresholdHigh,
                                         thresholdLow = thresholdLow)
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
    tags$h1(HTML(title)),
    splitter(Sausage,cols)
  )
}


#' Div maker
#'
#' This function takes strings created with the function `ButtonMaker` and makes an HTML div suitable for inclusion in other HTML code,
#' or for inclusion within the function of this package `TileMaker`.
#'
#' @param subtitle The text heading of this row of buttons
#' @param textModifier Optional css category of "large" text. In this case, subtitle. Default=h2
#' @param ... \code{solo_box OR solo_gradient_box} elements.
#' @examples
#' div_maker(title = "Quantativity factors",
#'           solo_gradient_box(value = 70),
#'           solo_box(value=34))
#' @export div_maker
div_maker <- function(subtitle = NULL, textModifier="h2",...) {

  tags$div(
    class = "container",
    tag(textModifier,tags$span(subtitle)$children),
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
#' @param css A string indicating css url, for final installations pls save the css file locally.
#' @param file Optional filename if you desire to save the file. By default we are using the 3.3.7 bootstrap CDN because
#' they support icons, but some others that might be interesting to you are:
#' https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css or https://bootswatch.com/4/flatly/bootstrap.css
#' @param textModifier Optional css category of "large" text. In this case, title. Default=h1
#' @importFrom htmltools browsable save_html
#' @export finisher
finisher <- function(title = NULL, ..., css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
                     file=NULL, textModifier="h1") {

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


# library(dplyr)
# library(htmltools)
# a <- tile_matrix(values = iris$Sepal.Length,titles = iris$Species,target = 5,thresholdHigh = 4,thresholdLow = 3)

# ico(NULL)
# ico("apple")
#
# b1 <- solo_box(value = 3.57, text = "Times apple eaten", icon = "apple")
# b2 <- solo_box(value = 13.7, text = "Nutritional value", size = "lg")
# b3 <- solo_box(value = 1, text = "Yumminess factor", type = "danger")
# b4 <- solo_box(value = 5, text = "Inconsistencies", hover = "This is the description")
#
# d1 <- div_maker(title = "Quantativity factors", b1, b2)
# d2 <- div_maker(title = "Implementation procedures", b3, b4)
#
# finisher(title = "Hello", b1, b2)
#
# map <- leaflet::addTiles(leaflet::leaflet())
# map
#
# finisher(title = "Hello", d1, map, d2)
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



