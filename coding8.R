# studytime bar plot
df1=nrow(sample_por %>% filter(V14==1))
df2=nrow(sample_por %>% filter(V14==2))
df3=nrow(sample_por %>% filter(V14==3))
df4=nrow(sample_por %>% filter(V14==4))

df<-c(df1,df2, df3, df4)

p <- plot_ly(
  x = c(1, 2, 3, 4),
  y = df,
  type = "bar"
)
p


