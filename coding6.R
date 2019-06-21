#rozklad zmiennej X-spozycie alkoholu oraz Y-ocena ucznia oraz wykres
d1=sample_math %>% filter(V28==1)
d2=sample_math %>% filter(V28==2)
d3=sample_math %>% filter(V28==3)
d4=sample_math %>% filter(V28==4)
d5=sample_math %>% filter(V28==5)

grades <- c()

for(i in c(0:20)){
  grades <- c(grades,length(data.matrix(d1 %>% filter(V31==i) %>% select(V31)))/length(data.matrix(d1 %>% select(V31))))
}
for(i in c(0:20)){
  grades <- c(grades,length(data.matrix(d2 %>% filter(V31==i) %>% select(V31)))/length(data.matrix(d2 %>% select(V31))))
}
for(i in c(0:20)){
  grades <- c(grades,length(data.matrix(d3 %>% filter(V31==i) %>% select(V31)))/length(data.matrix(d3 %>% select(V31))))
}
for(i in c(0:20)){
  grades <- c(grades,length(data.matrix(d4 %>% filter(V31==i) %>% select(V31)))/length(data.matrix(d4 %>% select(V31))))
}
for(i in c(0:20)){
  grades <- c(grades,length(data.matrix(d5 %>% filter(V31==i) %>% select(V31)))/length(data.matrix(d5 %>% select(V31))))
}

A=matrix(grades, nrow=5, ncol=21, byrow=TRUE, dimnames=list(c(1:5),c(0:20)))
for(row in 1:nrow(A)) {
  for(col in 1:ncol(A)) {
    A[row, col]<-round(A[row, col], digits=3)
  }
}

myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0 #170,60,60 to to 255,255,255
  ColorRamp <- rgb( seq(0, 0,length=256),  # Red
                    seq(0.19921875, 0.796875,length=256),  # Green
                    seq(0, 0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}