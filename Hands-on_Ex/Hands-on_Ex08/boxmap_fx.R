boxmap <- function(vnam, df, 
                   legtitle=NA,
                   mtitle="Box Map",
                   mult=1.5){
  var <- get.var(vnam,df)
  bb <- boxbreaks(var)
  tm_shape(df) +
    tm_polygons() +
    tm_shape(df) +
    tm_fill(vnam,title=legtitle,
            breaks=bb,
            palette="Blues",
            labels = c("lower outlier", 
                       "< 25%", 
                       "25% - 50%", 
                       "50% - 75%",
                       "> 75%", 
                       "upper outlier"))  +
    tm_borders() +
    tm_layout(main.title = mtitle, 
              title.position = c("left",
                                 "top"))
}