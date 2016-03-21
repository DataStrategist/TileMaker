\name{ButtonMaker, DivMaker, TileMaker}
\alias{ButtonMaker}
\alias{DivMaker}
\alias{TileMaker}

\title{
Three functions, one creates buttons, the other gathers them into divs (rows of buttons), and the third gathers the divs.
}
\description{
STEP 1/3, feed information into the buttons and specify the features you would like for each button, including the color, size, etc.

STEP 2, include those buttons into rows, assigning a title if you want.

STEP 3, include those rows, selecting where the css file is, assigning optional titles if desired, and whether the date should be shown or suppressed.
}
\usage{
ButtonMaker(Color=1,Size=4,Value,Subtitle="",Link="",Icon="", Units="",
                        Target=0,ThresholdHigh=0,ThresholdLow=0, Hover="", alpha=0.5)

DivMaker(Title="",Buttons)

TileMaker(MainTitle="",Divs,FileName,ShowDate=FALSE,localCSS=FALSE)
}

\arguments{

 %- ButtonMaker
  \item{Color}{
             Optional numeric 1-6, corresponding to the colors specified in the bootstrap css classes:
                  "success",  "warning", "danger", "info", "primary", "default"
                  }
  \item{Size}{
              Optional numeric 1-4, corresponding to the sizes specified in the bootstrap css classes:
                  "xs","sm","md","lg")
                  }
  \item{Value}{
             The numeric value you want to highlight (the main enchilada)
             }
  \item{Subtitle}{
          Optional subtext that should appear under the value
          }
  \item{Link}{
             Optional hyperlink that should be followed on click
              }
  \item{Icon}{
              Optional glyphicon that should be displayed from http://getbootstrap.com/components/
              }

  \item{Units}{
             Optional units that should be displayed after Value
             }

  \item{Target}{
            Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow
            }

  \item{ThresholdHigh}{
     Optional border between "green" and "orange". Use w/ Target and ThresholdLow. This value represents the RATIO
                  of the VALUE to the TARGET that, if above the ThresholdHigh will show as green, and if not, as orange
                  }

  \item{ThresholdLow}{
      Optional border between "orange" and "red". Use w/ Target and ThresholdLow. This value represents the RATIO
                  of the VALUE to the TARGET that, if above the ThresholdHigh will show as orange, and if not, as red
                  }

  \item{Hover}{
			  Optional tooltip, or text that will show up when a user rests their mouse over the button.
			  }

  \item{alpha}{
             Optional transparency coefficient for the icon, a decimal from 0-1.
             }

  %- ### DivMaker
	\item{Title}{
             The title for this row of buttons
             }

	\item{Buttons}{
           The Buttons that you want inserted into this row. If you have more than one button, use paste(Button1,Button2)
           }

  %- ### TileMaker
  \item{MainTitle}{
        Optional title for the whole set of titles
        }

	\item{Divs}{
              The Divs that you want inserted into the tile. If you have more than one div, use paste(Div1,Div2)
              }

	\item{FileName}{
          The filename the tile should spit out as, including the extension
          }

	\item{ShowDate}{
          Optional boolean controlling whether the date should be included or suppressed
          }

	\item{localCSS}{
          Optional boolean to specify whether the bootstrap css file should be served from the internet, or if you have saved a local version. If you have saved a local version, make sure to download the "fonts" folder too, otherwise glyphicons won't work.
          }
}

\value{
If you would like just HTML code (suitable for inserting in a dashboard or another document, you can use the Divs. If you would like a full html document suitable to be opened on it's own, use the TileMaker.
}
\references{
Uses Twitter's awesome bootstrap V3
}
\author{
Amit Kohli
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Button1 <- ButtonMaker(Color = 2,Value = 65,Subtitle = "Times apple eaten",
                       Link = "https://en.wikipedia.org/wiki/Apple",
                       Icon="glyphicon glyphicon-apple",
                       Target=100,ThresholdHigh=70,ThresholdLow=40)
Button2 <- ButtonMaker(Color = 3,Value = 93.7,Subtitle = "Nutritional value",
                       Target=100,ThresholdHigh=70,ThresholdLow=40,Units="\%",
                       Hover="Apples are high in good things. If you eat it, these good things go into your body
                       and further the cycle of good. Therefore, you should
                       eat
                       apples.")
Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor",
                       Hover="While the yuminess is inferior to chocolate,
                       it's still pretty friggin yummy.")
Button4 <- ButtonMaker(Color = 5,Size=1,Value = 5,Subtitle = "Inconsistencies",
                       Icon = "glyphicon glyphicon-pushpin")

Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "example.html")
browseURL("example.html")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
