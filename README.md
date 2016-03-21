# TileMaker
An R script that enables the creation of data tiles for inclusion in a html dashboard or some such.

## Now available as a package!
To install, please type:
`devtools::install_github("mexindian/TileMaker")`



Example:

```
library(TileMaker)
Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "Times apple eaten")
Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor")
Button4 <- ButtonMaker(Color = 5,Size=1,Value = 5,Subtitle = "Inconsistencies")

Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "a.html")
browseURL("a.html")
```
This will give something like this (every element of this is customizable):

![Example](https://github.com/mexindian/TileMaker/blob/master/example.PNG)

For latest live version, please see here: http://htmlpreview.github.io/?https://github.com/mexindian/TileMaker/blob/master/example.html

## Arguments
### TileMaker 
**MainTitle**        Optional title for the whole set of titles

**Divs**              The Divs that you want inserted into the tile. If you have more than one div, use paste(Div1,Div2)

**FileName**          The filename the tile should spit out as, including the extension

**ShowDate**          Optional boolean controlling whether the date should be included or suppressed

**localCSS**          Optional boolean to specify whether the bootstrap css file should be served from the internet, or if you have saved                   a local version. If you have saved a local version, make sure to download the "fonts" folder too, otherwise                           glyphicons won't work.


### DivMaker 
**Title**             The title for this row of buttons

**Buttons**           The Buttons that you want inserted into this row. If you have more than one button, use paste(Button1,Button2)


### ButtonMaker
**Color**             Optional numeric 1-6, corresponding to the colors specified in the bootstrap css classes: 
                  "success",  "warning", "danger", "info", "primary", "default"

**Size**              Optional numeric 1-4, corresponding to the sizes specified in the bootstrap css classes:
                  "xs","sm","md","lg")

**Value**             The numeric value you want to highlight (the main enchilada)

**Subtitle**          Optional subtext that should appear under the value

**Link**             Optional hyperlink that should be followed on click

**Icon**              Optional glyphicon that should be displayed from http://getbootstrap.com/components/

**Units**             Optional units that should be displayed after Value

**Target**            Optional target that the value should be compared against. Use with ThresholdHigh and THresholdLow

**ThresholdHigh**     Optional border between "green" and "orange". Use w/ Target and ThresholdLow. This value represents the RATIO
                  of the VALUE to the TARGET that, if above the ThresholdHigh will show as green, and if not, as orange

**ThresholdLow**      Optional border between "orange" and "red". Use w/ Target and ThresholdLow. This value represents the RATIO
                  of the VALUE to the TARGET that, if above the ThresholdHigh will show as orange, and if not, as red
                        
**Hover**			  Optional tooltip, or text that will show up when a user rests their mouse over the button.                      

**alpha**             Optional transparency coefficient for the icon, a decimal from 0-1.
