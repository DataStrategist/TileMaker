
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


ButtonMaker <- function(Color=1,Size=4,Value,Subtitle=""){
  ## colors
  colorList = c("default",  "primary",  "success",  "info",  "warning",  "danger")
    
  ## sizes:
  SizeList = c("xs","sm","md","lg")
  
  paste('<button type="button" class="btn btn-',
        colorList[Color],' btn-', SizeList[Size],'"><h1>',
        Value,
        '</h1>',
        Subtitle,
        '</button>',sep="")
}



#########################################

# Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "Times apple eaten")
# Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
# Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor")
# Button4 <- ButtonMaker(Color = 5,Size=1,Value = 5,Subtitle = "Inconsistencies")
# 
# Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
# Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
# TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "example.html")
# browseURL("example.html")
