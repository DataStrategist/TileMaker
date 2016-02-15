# TileMaker
An R script that enables the creation of data tiles for inclusion in a html dashboard or some such.

Example:

```
source("https://raw.githubusercontent.com/mexindian/TileMaker/master/tilemaker.R")
Button1 <- ButtonMaker(Color = 2,Value = 3.57,Subtitle = "Times apple eaten")
Button2 <- ButtonMaker(Color = 3,Value = 13.7,Subtitle = "Nutritional value")
Button3 <- ButtonMaker(Color = 4,Value = 1,Subtitle = "Yumminess factor")
Button4 <- ButtonMaker(Color = 5,Size=1,Value = 5,Subtitle = "Inconsistencies")

Div1 <- DivMaker(Title = "Quantativity factors",Buttons = paste(Button1,Button2))
Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
TileMaker(MainTitle = "Hello",Divs = paste(Div1,Div2),FileName = "a.html")
browseURL("a.html")
```

![Example](https://github.com/images/example.png)
