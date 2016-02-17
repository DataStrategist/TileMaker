
## Make a html file containing tiles for single data point visualization using bootstrap
## This includes 3 functions that should be used in sequence: 
## TileMaker(
##   DivMaker(
##     ButtonMaker()))


TileMaker <- function(MainTitle="",Divs,FileName,ShowDate=FALSE){
  cat('<!DOCTYPE html><html lang="en"><head>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
  </head><body><h1>',
        MainTitle,
        '</h1>',
        if(ShowDate){paste('<h2>Report Date: ',Sys.Date(),'</h2>',sep="")},
        Divs,
        '</body></html>',
        file=FileName,sep="")
}



DivMaker <- function(Title="",Buttons){
  paste('<div class="container"><h2>',
        Title,
        '</h2>',
        Buttons,
        '</div>',sep="")
}


ButtonMaker <- function(Color=1,Size=4,Value,Subtitle="",Link="",Icon="", Units="",
                        Target=0,ThresholdHigh=0,ThresholdLow=0){
  ## colors
  colorList = c("success",  "warning", "danger", "info", "primary", "default")
    
  ## sizes:
  SizeList = c("xs","sm","md","lg")
  
  ## for icons, goto http://getbootstrap.com/components/
  
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
  paste(' btn-', SizeList[Size],'"><h1>',sep=''),
        if(Icon !=""){paste(' <span class="',Icon,'" aria-hidden="true"></span> ',sep='')},
        if(Units == ""){Value} else {paste(Value,Units,sep="")},
        '</h1>',
        Subtitle,
        if(Link !=""){'</a>'} else{'</button>'},
        sep="")
}

#########################################

Button1 <- ButtonMaker(Color = 2,Value = 65,Subtitle = "Times apple eaten",
                       Link = "https://en.wikipedia.org/wiki/Apple",Icon="glyphicon glyphicon-apple", 
                       Target=100,ThresholdHigh=70,ThresholdLow=40)
Button2 <- ButtonMaker(Color = 3,Value = 93.7,Subtitle = "Nutritional value", 
                       Target=100,ThresholdHigh=70,ThresholdLow=40,Units="%")
Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor")
Button4 <- ButtonMaker(Color = 5,Size=1,Value = 5,Subtitle = "Inconsistencies",Icon = "glyphicon glyphicon-pushpin")

Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "example.html")
browseURL("example.html")
