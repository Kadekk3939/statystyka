#parents education connected?
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
base=sample_math
f0=base %>% filter(V8==0) 
f1=base %>% filter(V8==1)
f2=base %>% filter(V8==2)
f3=base %>% filter(V8==3)
f4=base %>% filter(V8==4)

a0=data.matrix(f0 %>% select(V7))
a1=data.matrix(f1 %>% select(V7))
a2=data.matrix(f2 %>% select(V7))
a3=data.matrix(f3 %>% select(V7))
a4=data.matrix(f4 %>% select(V7))

table = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=6, ncol=6)
#
rownames(table)<-c(0, 1, 2, 3, 4, "suma")
colnames(table)<-c(0, 1, 2, 3, 4, "suma")
qfor(i in c(1:length(a0))){
  table[a0[i]+1, 1] %+=% 1
}
for(i in c(1:length(a1))){
  table[a1[i]+1, 2] %+=% 1
}
for(i in c(1:length(a2))){
  table[a2[i]+1, 3] %+=% 1
}
for(i in c(1:length(a3))){
  table[a3[i]+1, 4] %+=% 1
}
for(i in c(1:length(a4))){
  table[a4[i]+1, 5] %+=% 1
}

for(i in c(1:5)){
  table[i, 6]=table[i, 1]+table[i, 2]+table[i, 3]+table[i, 4]+table[i, 5]
  table[6, i]=table[1, i]+table[2, i]+table[3, i]+table[4, i]+table[5, i]
  table[6,6]%+=%table[6, i]
}

copy=table

for(i in c(1:5)){
  for(j in c(1:5)){
    copy[i, j] <- (copy[i, 6]*copy[6, j])/copy[6, 6]
  }
}
copy2=table
for(i in c(1:5)){
  for(j in c(1:5)){
    copy2[i, j] <- (copy2[i, j]-copy[i, j])^2/copy[i, j]
  }
}
copy2[6, 6]=0
for(i in c(1:5)){
  copy2[i, 6]=copy2[i, 1]+copy2[i, 2]+copy2[i, 3]+copy2[i, 4]+copy2[i, 5]
  copy2[6, i]=copy2[1, i]+copy2[2, i]+copy2[3, i]+copy2[4, i]+copy2[5, i]
  copy2[6,6]%+=%copy2[6, i]
}
table
copy
copy2

chi=168.13135
df=26
p=0.5

#chi>>37,65	 => jest zaleznosc/chi jest zbyt duze by mialo to sens?
