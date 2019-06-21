#average value of certain columns
show_avg <- function(db){
  suma=sum(db)/length(db)
  return (suma)
}

show_avg(data.matrix(sample_math %>% select(3)))
show_avg(data.matrix(sample_por %>% select(3)))
show_avg(data.matrix(sample_math %>% select(V13)))
show_avg(data.matrix(sample_por %>% select(V13)))
show_avg(data.matrix(sample_por %>% select(V14)))

show_avg(data.matrix(sample_math %>% select(V15)))
show_avg(data.matrix(sample_por %>% select(V15)))

show_avg(data.matrix(sample_math %>% select(V24)))
show_avg(data.matrix(sample_por %>% select(V24)))

show_avg(data.matrix(sample_math %>% select(V26)))
show_avg(data.matrix(sample_por %>% select(V26)))


show_avg(data.matrix(sample_math %>% select(V27)))
show_avg(data.matrix(sample_por %>% select(V27)))


show_avg(data.matrix(sample_math %>% select(V28)))
show_avg(data.matrix(sample_por %>% select(V28)))

show_avg(data.matrix(sample_math %>% select(V29)))
show_avg(data.matrix(sample_por %>% select(V29)))

show_avg(data.matrix(sample_math %>% select(V30)))
show_avg(data.matrix(sample_por %>% select(V30)))

show_avg(data.matrix(sample_math %>% select(V33)))
show_avg(data.matrix(sample_por %>% select(V33)))