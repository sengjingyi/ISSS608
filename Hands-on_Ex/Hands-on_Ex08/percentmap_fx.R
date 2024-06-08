percentmap <- function(vnam, df, legtitle=NA, mtitle="Percentile Map"){
  percent <- c(0,.01,.1,.5,.9,.99,1)
  var <- get.var(vnam, df)
  bperc <- quantile(var, percent)
  tm_shape(df) +
    tm_polygons() +
    tm_shape(df) +
    tm_fill(vnam,
            title=legtitle,
            breaks=bperc,
            palette="Blues",
            labels=c("< 1%", "1% - 10%", "10% - 50%", "50% - 90%", "90% - 99%", "> 99%"))  +
    tm_borders() +
    tm_layout(main.title = mtitle, 
              title.position = c("right","bottom"))
}