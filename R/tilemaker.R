
## Make a html file containing tiles for single data point visualization using bootstrap
## This includes 3 functions that should be used in sequence:
## TileMaker(
##   DivMaker(
##     ButtonMaker()))






#' ButtonMaker
#'
#' This function crafts the actual button per se, including the specific
#' aesthetic traits for each button
#'
#' @param Color Optional numeric 1-6, corresponding to the colors specified in the bootstrap css classes:
#' \"success\",  \"warning\", \"danger\", \"info\", \"primary\", \"default\"
#' @param Size Optional numeric 1-4, corresponding to the sizes specified in the bootstrap css classes:
#' \"xs\",\"sm\",\"md\",\"lg\")
#' @param Value The numeric value you want to highlight (the main enchilada)
#' @param Subtitle Optional subtext that should appear under the value
#' @param Link Optional hyperlink that should be followed on click
#' @param Icon Optional glyphicon that should be displayed from http://getbootstrap.com/components/
#' @param Units Optional units that should be displayed after Value
#' @param Target Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow
#' @param ThresholdHigh Optional border between \"green\" and \"orange\". Use w/ Target and ThresholdLow. This value represents the RATIO
#' of the VALUE to the TARGET that, if above the ThresholdHigh will show as green, and if not, as orange
#' @param ThresholdLow Optional border between \"orange\" and \"red\". Use w/ Target and ThresholdLow. This value represents the RATIO
#' of the VALUE to the TARGET that, if above the ThresholdHigh will show as orange, and if not, as red
#' @param Hover Optional tooltip, or text that will show up when a user rests their mouse over the button.
#' @param alpha Optional transparency coefficient for the icon, a decimal from 0-1.
#' @param Former Optional value to compare against the current value. Will show up as a little arrow pointig the percent change
#'
#' @return Returns a character string containing HTML code to show the button, assuming the appropriate CSS elements will be available downstream
#' @examples
#' # ADD EXAMPLES HERE
#' # Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "B")
#' # Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
#' # Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor")
#' # Button1;Button2;Button3
#' @export

ButtonMaker <- function(Color=1,Size=4,Value,Subtitle="",Link="",Icon="", Units="",
                        Target=0,ThresholdHigh=0,ThresholdLow=0, Hover="", alpha=0.5,
                        Former=Value){
  ## colors
  colorList = c("success",  "warning", "danger", "info", "primary", "default")

  ## sizes:
  SizeList = c("xs","sm","md","lg")

  paste(paste('<',
              if(Link !=""){paste('a href="',Link,'" role="button" ',sep='')} else{'button'},
              ' type="button" class="btn ',sep=''),
        if(Target ==0){
          paste('btn-',colorList[Color],sep='')
        } else {
          Perc <- Value/Target *100
          if(Perc > ThresholdHigh){
            'btn-success'
          } else if(Perc< ThresholdLow){
            'btn-danger'
          } else {
            'btn-warning'
          }
        },
        paste(' btn-', SizeList[Size],
              '"',

              if(Hover !=""){paste(' title="',Hover,'" ')},
              '><h1>',sep=''),

        if(Icon !=""){paste(' <span class="',Icon,'" aria-hidden="true" style="opacity:',alpha,'"></span> ',sep='')},
        paste(Value,Units,sep=''),

        if(Former>Value){
          paste('<sup style= "font-size: 12px;color:#EEEEEE;vertical-align: top;">&#9660;',round((Former-Value)/Former*100,1),'%</sup>',sep='')
        } else if (Former<Value){
          paste('<sup style= "font-size: 12px;color:#EEEEEE;vertical-align: top;">&#9650;',round((Value-Former)/Former*100,1),'%</sup>',sep='')
        },
        '</h1>',
        Subtitle,
        if(Link !=""){'</a>'} else{'</button>'},
        sep="")
}

#' DivMaker
#'
#' This function takes strings created with the function `ButtonMaker` and makes an HTML div suitable for inclusion in other HTML code,
#' or for inclusion within the function of this package `TileMaker`.
#'
#' @param Title The title for this row of buttons
#' @param Buttons The Buttons that you want inserted into this row. If you have more than one button, use paste(Button1,Button2)
#'
#' @return Returns an HTML string containing \"div\" elements. Beware of using these in Shiny... a it might break the container.
#' @examples
#' # ADD EXAMPLES HERE
#' Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "B")
#' Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
#' DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
#' @export

DivMaker <- function(Title="",Buttons){
  paste('<div class="container"><h2>',
        Title,
        '</h2>',
        Buttons,
        '</div>',sep="")
}


#' TileMaker
#'
#' Function 3 of 3, the last step. This function grabs the Divs created by `DivMaker` and
#' combines them into an html file.
#'
#' @param MainTitle Optional title for the whole set of titles
#' @param Divs The Divs that you want inserted into the tile. If you have more than one div,
#' use `paste(Div1,Div2)`
#' @param FileName The filename the tile should spit out as, including the extension (which
#' should always be html)
#' @param ShowDate Optional boolean controlling whether the date should be included or suppressed
#' @param localCSS Optional boolean to specify whether the bootstrap css file should be served from
#' the internet, or if you have saved a local version. If you have saved a local version, make sure
#' to download the "fonts" folder too, otherwise glyphicons won't work.
#'
#' @return Use this function to output an html file containing all the divs.If you would like just
#' HTML code (suitable for inserting in a dashboard or another document, you can use the Divs.
#'
#' @references Uses Twitter's awesome bootstrap V3
#'
#' @examples
#' # ADD EXAMPLES HERE
#' Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "B")
#' Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
#' Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
#' Div2 <- DivMaker(Title = "Inverse proportions",Buttons = paste(Button2,Button1))
#' TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "example.html")
#' TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2))
#' browseURL("example.html")
#' @export

TileMaker <- function(MainTitle="",Divs,FileName="x",ShowDate=FALSE,localCSS=FALSE){
  paste('<!DOCTYPE html><html lang="en"><head>
      <meta name="viewport" content="width=device-width, initial-scale=1">',
      if(localCSS==TRUE){'<link rel="stylesheet" href="bootstrap.min.css">'
      } else {'<link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">'},
      '</head><body><h1>',
      MainTitle,
      '</h1>',
      if(ShowDate){paste('<h2>Report Date: ',Sys.Date(),'</h2>',sep="")},
      Divs,
      '</body></html>',
      sep="") -> somethin

      ## Output file
      if (FileName !="x") {
        # sink(FileName)
        # somethin
        # sink()
        cat(somethin,file=FileName)
      } else {
          somethin
        }
}



#' tileMatrix
#'
#' Create a matrix of buttons suitable for quick comparisons
#'
#' @param df Dataframe containing information to use. It expects either 2 columns
#' @param Tar Target value (What's the highest value to compare against). Defaults to 100
#' @param Thre.H The limit between "high" and "medium" values IN PERCENT. Defaults to 90
#' @param Thre.L The limit between "medium" and "low" values IN PERCENT. Defaults to 50
#' @param cols Number of columns that the matrix should tile around. Defaults to 4
#' @param Title The title the matrix should have.
#' @param FileName The filename that will contain the html
#' @param RoundVal Number of decimals that Value will be rounded to. Defaults to 1
#' @param ButtWidth The width of each button element, in Number of pixels. Defaults to 100.
#' @param Margin The amount of margin desired between buttons in pixels. Defaults to 3.
#'
#' @return Returns an HTML object containing the matrix of buttons
#' @examples
#' df <- aggregate(Petal.Length ~ Species, data=iris, mean)
#' tileMatrix(df,Tar=7,Thre.H=90,Thre.L=80,FileName="matrixTest.html",Title="", ButtWidth=200)
#' df$previous = c(1,4.260,6)
#' tileMatrix(df,Tar=7,Thre.H=90,Thre.L=80,FileName="matrixTest.html",Title="", Margin=2)
#' @export
tileMatrix <- function(df,Tar=100,Thre.H=90,Thre.L=50,cols=4,
                       Title,FileName,RoundVal=1,ButtWidth=100,Margin=3){
  # tileMatrix <- function(SubT,Value,FormerValue,Tar,Thre.H,Thre.L,cols,Title,FileName){
  # map(1,ButtonMaker,Value= Value,
  #     Size = 2,Subtitle = SubT,
  #     Target = Tar,ThresholdHigh = Thre.H,ThresholdLow = Thre.L,
  #     Former = FormerValue)  -> completed
  # allButtons <- completed[[1]]
  # allButtons <- data.frame(allButtons,
  #                          id=1:length(allButtons),
  #                          stringsAsFactors = F)

  if(ncol(df)==2){
    names(df)=c("stuff","Values")
  } else if (ncol(df)==3){
    names(df)=c("stuff","Values","Previous")
  } else {
    stop("Data frame should consist of a name and a value (and optionally a previous value)")
  }

  df$stuff <- as.character(df$stuff)

  df$id <- 1:nrow(df)
  df$butts <- ""
  df$Values <- round(df$Values,RoundVal)
  # df$stuff <- str_trunc(df$stuff,min(str_length(df$stuff)),side="right")

  ## protect against NAs
  df$Values[is.na(df$Values)] <-0.001
  if(ncol(df)==3) df$Previous[is.na(df$Previous)] <-0.001

  for(i in 1:nrow(df)){
    if(ncol(df)==5){
      df$butts[i] <- ButtonMaker(Size = 2,Value = df$Values[i],Subtitle = df$stuff[i],
                               Target=Tar,ThresholdHigh = Thre.H,
                               ThresholdLow = Thre.L,Former=df$Previous[i])
    } else {
      df$butts[i] <- ButtonMaker(Size = 2,Value = df$Values[i],Subtitle = df$stuff[i],
                                 Target=Tar,ThresholdHigh = Thre.H,
                                 ThresholdLow = Thre.L)
    }
  }

  df <- as.data.frame(df)

  ## Break the button sausage every COLS
  df[df$id %% cols == 0,"butts"] <-
    paste0(df[df$id %% cols == 0,"butts"],"<br>",sep="")

  ## Ghetto css element adder
  df$butts = gsub('class',paste('style="width:',ButtWidth,'px; margin:',Margin,'px" class',sep=""),df$butts)

  ## Output file
  if (FileName !="x") {
    TileMaker(Title,paste(df$butts,collapse=""),FileName = FileName)

  } else {
    TileMaker(Title,paste(df$butts,collapse=""))
  }
}

