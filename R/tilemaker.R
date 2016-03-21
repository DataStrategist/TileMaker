
## Make a html file containing tiles for single data point visualization using bootstrap
## This includes 3 functions that should be used in sequence:
## TileMaker(
##   DivMaker(
##     ButtonMaker()))


TileMaker <- function(MainTitle="",Divs,FileName,ShowDate=FALSE,localCSS=FALSE){
  cat('<!DOCTYPE html><html lang="en"><head>
  <meta name="viewport" content="width=device-width, initial-scale=1">',
  if(localCSS==TRUE){'<link rel="stylesheet" href="bootstrap.min.css">'
    } else {'<link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">'},
  '</head><body><h1>',
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
          paste('<sup style= "font-size: 10px;color:#EEEEEE">&#9660;',round((Former-Value)/Former*100,1),'%</sup>',sep='')
        } else if (Former<Value){
          paste('<sup style= "font-size: 10px;color:#EEEEEE">&#9650;',round((Value-Former)/Former*100,1),'%</sup>',sep='')
        },
        '</h1>',
        Subtitle,
        if(Link !=""){'</a>'} else{'</button>'},
        sep="")
}


