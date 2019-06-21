#sam rozklad wspolny bez wykresu
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
A
