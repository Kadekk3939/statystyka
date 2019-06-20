# libraries
library(dplyr)
library(plotly)
library(ggpubr)
library(scatterplot3d)

# getdata
setwd("C:/Users/Kaczor3/Desktop/NIepotrzebne/Studia/Sem4/statystyka")
d1 <- read.csv("student-mat.csv",sep=",",header=TRUE)
d2 <- read.csv("student-por.csv",sep=",",header=TRUE)
d3 <- merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

# removing columns
student_mat = select(d1, -c(colnames(d1)[c(10,20,22,12,9,18,28)]))
student_por = select(d2, -c(colnames(d2)[c(10,20,22,12,9,18,28)]))

# filtering data
student_mat = union(
    sample_n(student_mat %>% filter(sex == "F"),150),
    sample_n(student_mat %>% filter(sex == "M"),150)
    )

student_por = union(
    sample_n(student_por %>% filter(sex == "F"),250),
    sample_n(student_por %>% filter(sex == "M"),250)
    )

# functions
# fit for x = somedata[1], y = somedata[2]
fitfunc <- function(somedata){
    mydata = data.frame(somedata[1],somedata[2])
    colnames(mydata) = c("x","y")
    fit = lm(y ~ poly(x, 3, raw=TRUE), data = mydata)
    fitdata = data.frame(x = seq(min(mydata[1]),max(mydata[1]), length.out = 250))
    fitdata$y = predict(fit,fitdata)
    return (fitdata)
}

# ----- Define a function for plotting a matrix ----- #
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
# ----- END plot function ----- #






# some statistics
# 1 - romantic x studytime
test1 <- student_mat[,c("romantic", "studytime")]
x = c(1,2,3,4)

test1y = c(length(which(test1$romantic == "yes" & test1$studytime == 1)),
    length(which(test1$romantic == "yes" & test1$studytime == 2)),
    length(which(test1$romantic == "yes" & test1$studytime == 3)),
    length(which(test1$romantic == "yes" & test1$studytime == 4))
    )

test1n = c(length(which(test1$romantic == "no" & test1$studytime == 1)),
    length(which(test1$romantic == "no" & test1$studytime == 2)),
    length(which(test1$romantic == "no" & test1$studytime == 3)),
    length(which(test1$romantic == "no" & test1$studytime == 4))
    )

#par(mfrow = c(1,2))
plot_ly(x = x, y = test1y, type = "bar", color = I("red"))
plot_ly(x = x, y = test1n, type = "bar", color = I("blue"))


# 2 - romantic x absences x G1, G2, G3
test2 <- student_mat[,c("romantic", "absences", "G1", "G2", "G3")]
#test2 <- student_por[,c("romantic", "absences", "G1", "G2", "G3")]

test2y = subset(test2, romantic == "yes")
test2n = subset(test2, romantic == "no")

test2y1 = aggregate(G1 ~ absences, data = test2y, FUN = mean)
test2y1$sd1 = aggregate(G1 ~ absences, data = test2y, FUN = sd)$G1
test2y1[is.na(test2y1)] <- 0

test2y2 = aggregate(G2 ~ absences, data = test2y, FUN = mean)
test2y2$sd2 = aggregate(G2 ~ absences, data = test2y, FUN = sd)$G2
test2y2[is.na(test2y2)] <- 0

test2y3 = aggregate(G3 ~ absences, data = test2y, FUN = mean)
test2y3$sd3 = aggregate(G3 ~ absences, data = test2y, FUN = sd)$G3
test2y3[is.na(test2y3)] <- 0

test2n1 = aggregate(G1 ~ absences, data = test2n, FUN = mean)
test2n1$sd1 = aggregate(G1 ~ absences, data = test2n, FUN = sd)$G1
test2n1[is.na(test2n1)] <- 0

test2n2 = aggregate(G2 ~ absences, data = test2n, FUN = mean)
test2n2$sd2 = aggregate(G2 ~ absences, data = test2n, FUN = sd)$G2
test2n2[is.na(test2n2)] <- 0

test2n3 = aggregate(G3 ~ absences, data = test2n, FUN = mean)
test2n3$sd3 = aggregate(G3 ~ absences, data = test2n, FUN = sd)$G3
test2n3[is.na(test2n3)] <- 0

plotline2y1 = fitfunc(test2y1)
plotline2y2 = fitfunc(test2y2)
plotline2y3 = fitfunc(test2y3)

plotline2n1 = fitfunc(test2n1)
plotline2n2 = fitfunc(test2n2)
plotline2n3 = fitfunc(test2n3)

# romance == yes with average grades
plot2y = ggplot()

plot2y = plot2y + geom_point(data = test2y1, aes(absences,G1), col = "green", size = 2)
plot2y = plot2y + geom_point(data = test2y2, aes(absences,G2), col = "red", size = 2)
plot2y = plot2y + geom_point(data = test2y3, aes(absences,G3), col = "blue", size = 2)

plot2y = plot2y + geom_line(data = plotline2y1, aes(x,y), col = "green")
plot2y = plot2y + geom_line(data = plotline2y2, aes(x,y), col = "red")
plot2y = plot2y + geom_line(data = plotline2y3, aes(x,y), col = "blue")

plot2y = plot2y + geom_errorbar(data = test2y1, aes(absences,G1, ymin = G1 - sd1, ymax = G1 + sd1), col = "green", width = 1)
plot2y = plot2y + geom_errorbar(data = test2y2, aes(absences,G2, ymin = G2 - sd2, ymax = G2 + sd2), col = "red", width = 0.8)
plot2y = plot2y + geom_errorbar(data = test2y3, aes(absences,G3, ymin = G3 - sd3, ymax = G3 + sd3), col = "blue", width = 0.5)


plot2ym = plot2y + labs(title = "Math Students", subtitle = "Romances == Yes", x = "Absences", y = "Average grade points")
plot2yp = plot2y + labs(title = "Portuguese Students", subtitle = "Romances == Yes", x = "Absences", y = "Average grade points")

# romance == no with average grades
plot2n = ggplot()

plot2n = plot2n + geom_point(data = test2n1, aes(absences,G1), col = "green", size = 2)
plot2n = plot2n + geom_point(data = test2n2, aes(absences,G2), col = "red", size = 2)
plot2n = plot2n + geom_point(data = test2n3, aes(absences,G3), col = "blue", size = 2)

plot2n = plot2n + geom_line(data = plotline2n1, aes(x,y), col = "green")
plot2n = plot2n + geom_line(data = plotline2n2, aes(x,y), col = "red")
plot2n = plot2n + geom_line(data = plotline2n3, aes(x,y), col = "blue")

plot2n = plot2n + geom_errorbar(data = test2n1, aes(absences,G1, ymin = G1 - sd1, ymax = G1 + sd1), col = "green", width = 1)
plot2n = plot2n + geom_errorbar(data = test2n2, aes(absences,G2, ymin = G2 - sd2, ymax = G2 + sd2), col = "red", width = 0.8)
plot2n = plot2n + geom_errorbar(data = test2n3, aes(absences,G3, ymin = G3 - sd3, ymax = G3 + sd3), col = "blue", width = 0.5)

plot2nm = plot2n + labs(title = "Math Students", subtitle = "Romances == No", x = "Absences", y = "Average grade points")
plot2np = plot2n + labs(title = "Portuguese Students", subtitle = "Romances == No", x = "Absences", y = "Average grade points")
# 3 - 



# sample_math = student_por
# d1=sample_math %>% filter(Dalc==1)
# d2=sample_math %>% filter(Dalc==2)
# d3=sample_math %>% filter(Dalc==3)
# d4=sample_math %>% filter(Dalc==4)
# d5=sample_math %>% filter(Dalc==5)
 
# grades <- c()
 
# for(i in c(0:20)){
#   grades <- c(grades,length(data.matrix(d1 %>% filter(G1==i) %>% select(G1)))/length(data.matrix(d1 %>% select(G1))))
# }
# for(i in c(0:20)){
#   grades <- c(grades,length(data.matrix(d2 %>% filter(G1==i) %>% select(G1)))/length(data.matrix(d2 %>% select(G1))))
# }
# for(i in c(0:20)){
#   grades <- c(grades,length(data.matrix(d3 %>% filter(G1==i) %>% select(G1)))/length(data.matrix(d3 %>% select(G1))))
# }
# for(i in c(0:20)){
#   grades <- c(grades,length(data.matrix(d4 %>% filter(G1==i) %>% select(G1)))/length(data.matrix(d4 %>% select(G1))))
# }
# for(i in c(0:20)){
#   grades <- c(grades,length(data.matrix(d5 %>% filter(G1==i) %>% select(G1)))/length(data.matrix(d5 %>% select(G1))))
# }
 
# A=matrix(grades, nrow=5, ncol=21, byrow=TRUE, dimnames=list(c(1:5),c(0:20)))
# for(row in 1:nrow(A)) {
#   for(col in 1:ncol(A)) {
#     A[row, col]<-round(A[row, col], digits=3)
#   }
# }
# A
# myImagePlot(A)



