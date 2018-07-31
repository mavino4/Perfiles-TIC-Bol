i_new_data_f$cluster = factor(i_new_data_f$cluster,
                              levels(i_new_data_f$cluster)[c(1,3,2)])
levels(i_new_data_f$cluster) <- c("A","B","C")
round(prop.table(table(i_new_data_f$cluster))*100,2)
i_data <- readRDS("/home/marco/Documents/Club Data Science/proyecto AGETIC/BASES/bd_imputada.rds")
i_input <- i_data[,-c(1,17,57,58,59,60)]





#creando piramides poblacionales

PyrAgeA <-  i_new_data_f[ which(i_new_data_f$cluster=='A'),c("P1", "P148" ) ]
PyrAgeB <-  i_new_data_f[ which(i_new_data_f$cluster=='B'),c("P1", "P148" ) ]
PyrAgeC <-  i_new_data_f[ which(i_new_data_f$cluster=='C'),c("P1", "P148" ) ]
ggplot(data=PyrAgeA, aes(PyrAgeA$P1)) + 
  geom_histogram(breaks=seq(10, 80, by =3), aes(fill=..count..))+
  scale_fill_gradient("Count", low = "red", high = "darkred")

ggplot(data=PyrAgeB, aes(PyrAgeB$P1)) + 
  geom_histogram(breaks=seq(10, 80, by =3), aes(fill=..count..))+
  scale_fill_gradient("Count", low = "green", high = "darkgreen")

ggplot(data=PyrAgeC, aes(PyrAgeC$P1)) + 
  geom_histogram(breaks=seq(10, 80, by =3), aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "blue", high = "darkblue")

