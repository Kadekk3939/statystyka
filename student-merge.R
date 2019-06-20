# libraries
library("dplyr")
library("plotly")
library(ggpubr)

# getdata
setwd("C:/Users/Kaczor3/Desktop/NIepotrzebne/Studia/Sem4/statystyka")
d1 <- read.csv("student-mat.csv",sep=",",header=TRUE)
d2 <- read.csv("student-por.csv",sep=",",header=TRUE)
d3 <- merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

# removing columns
student_mat = select(d1, -c(colnames(d1)[c(10,20,22,12,9,18)]))
student_por = select(d2, -c(colnames(d2)[c(10,20,22,12,9,18)]))

# function
# fit for x = somedata[1], y = somedata[2]
fitfunc <- function(somedata){
    mydata = data.frame(somedata[1],somedata[2])
    colnames(mydata) = c("x","y")
    fit = lm(y ~ poly(x, 3, raw=TRUE), data = mydata)
    fitdata = data.frame(x = seq(min(mydata[1]),max(mydata[1]), length.out = 250))
    fitdata$y = predict(fit,fitdata)
    return (fitdata)
}

# travel distance impact on absences and failures
# sample_math=d1 %>% select(13, 15, 30)
# pos1_math=sample_math %>% filter(traveltime==1)
# pos2_math=sample_math %>% filter(traveltime==2)
# pos3_math=sample_math %>% filter(traveltime==3)
# pos4_math=sample_math %>% filter(traveltime==4)


# m1=data.matrix(pos1_math %>% select(3))
# m2=data.matrix(pos2_math %>% select(3))
# m3=data.matrix(pos3_math %>% select(3))
# m4=data.matrix(pos4_math %>% select(3))

# distances <- c("1-15min", "15-30min", "30-60min", "60min-")
# sums <- c(sum(m1)/length(m1), sum(m2)/length(m2), sum(m3)/length(m3), sum(m4)/length(m4))

# p <- plot_ly(
#   x = distances,
#   y = sums,
#   name = "Distances from home impact on absences",
#   type = "bar"
# )

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

plot2y = plot2y + geom_errorbar(data = test2y1, aes(absences,G1, ymin = G1 - sd1, ymax = G1 + sd1), col = "green", width = 0.5)
plot2y = plot2y + geom_errorbar(data = test2y2, aes(absences,G2, ymin = G2 - sd2, ymax = G2 + sd2), col = "red", width = 0.5)
plot2y = plot2y + geom_errorbar(data = test2y3, aes(absences,G3, ymin = G3 - sd3, ymax = G3 + sd3), col = "blue", width = 0.5)

# romance == no with average grades
plot2n = ggplot()

plot2n = plot2n + geom_point(data = test2n1, aes(absences,G1), col = "green", size = 2)
plot2n = plot2n + geom_point(data = test2n2, aes(absences,G2), col = "red", size = 2)
plot2n = plot2n + geom_point(data = test2n3, aes(absences,G3), col = "blue", size = 2)

plot2n = plot2n + geom_line(data = plotline2n1, aes(x,y), col = "green")
plot2n = plot2n + geom_line(data = plotline2n2, aes(x,y), col = "red")
plot2n = plot2n + geom_line(data = plotline2n3, aes(x,y), col = "blue")

plot2n = plot2n + geom_errorbar(data = test2n1, aes(absences,G1, ymin = G1 - sd1, ymax = G1 + sd1), col = "green", width = 0.5)
plot2n = plot2n + geom_errorbar(data = test2n2, aes(absences,G2, ymin = G2 - sd2, ymax = G2 + sd2), col = "red", width = 0.5)
plot2n = plot2n + geom_errorbar(data = test2n3, aes(absences,G3, ymin = G3 - sd3, ymax = G3 + sd3), col = "blue", width = 0.5)

# 3 - 