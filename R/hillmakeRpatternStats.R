plot.hillmakeRpatternStats<-function(hillmakeRpatternStats){
  
  maxy<-round(max(hillmakeRpatternStats$df$"90%")*1.10)
  
  if(length(hillmakeRpatternStats$factors) > 0){
    if(length(hillmakeRpatternStats$factors) > 1) {
    hillmakeRpatternStats$df$plotCol<-apply(hillmakeRpatternStats$df[,hillmakeRpatternStats$factors], 1, paste, collapse = " ")
    } else {
      hillmakeRpatternStats$df$plotCol<-paste(hillmakeRpatternStats$df[,hillmakeRpatternStats$factors])     
    }
    #figure out x-axis
    numFactors<- length(levels(factor(hillmakeRpatternStats$df$plotCol)))
    iter<-levels(factor(hillmakeRpatternStats$df$plotCol))
  } else {
    numFactors<-1
    iter<- NULL
  }
  pal<-rainbow(numFactors)
  bp<-barplot(as.matrix(hillmakeRpatternStats$df$mean), beside=TRUE, col=pal, ylim = c(0,maxy))
  numBars<-length(bp)
  #xgap<-(.5*(numBars/numFactors))
  #x.coor<-seq(from = xgap + length.out = numBars/numFactors, to = numBars - xgap)
  if(length(hillmakeRpatternStats$factors) > 0){
  colorcount <- 0
  for(i in iter){
  colorcount<-colorcount + 1
  x.coor<-bp[seq(from = 1.5+(colorcount-1), to = (max(bp) - numFactors) + colorcount , length = numBars / numFactors)]
  #par(new = TRUE)
    lines(x = x.coor,y = hillmakeRpatternStats$df[hillmakeRpatternStats$df$plotCol == i,]$"90%", type = "o",lwd = 2,  col = pal[colorcount], ylim = c(0,maxy)) #   require(ggplot2)
  }
  legend("topleft", legend = iter, fill = pal, bty = "n", cex = 0.6)
  } else {
    lines(x = bp,y = hillmakeRpatternStats$df$"90%", type = "o",lwd = 2,  col = pal, ylim = c(0,maxy)) #   require(ggplot2)    
  }
  
}