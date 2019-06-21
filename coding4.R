#example of funcitons used to get variation and quantiles
quantile(data.matrix(sample_math %>% select(V31)))

var(data.matrix(sample_por %>% select(V30)))