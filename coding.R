

#creates 2 samples
student.mat = select(student.mat, -c(V10, V20, V22, V12, V9, V18, V22))

guys=student.mat %>% filter(V2=="M")
girls=student.mat %>% filter(V2=="F")

guys=sample_n(guys, 150)
girls=sample_n(girls, 150)

sample=union(girls, guys)

student.por = select(student.por, -c(V10, V20, V22, V12, V9, V18, V22))

guys=student.por %>% filter(V2=="M")
girls=student.por %>% filter(V2=="F")

guys=sample_n(guys, 250)
girls=sample_n(girls, 250)

sample_por=union(girls, guys)

