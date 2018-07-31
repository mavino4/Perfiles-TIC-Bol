elegidos <- bd_obs_completas[,c("P6", "P12", "P16", "P17", "P23", "P28", "P153", "P156A", "P156B", "P156D", "P156E", "P156H", "P156I", "P156J")]
mask <- elegidos$P16 == 1 | elegidos$P17 == 1
ElegidosFactor <- data.frame(apply(elegidosSinTV , 13, as.factor))

elegidosSinTV <- bd_obs_completas[,c("P6", "P16", "P17", "P23", "P28", "P153", "P156A", "P156B", "P156D", "P156E", "P156H", "P156I", "P156J")]


# Volviendo los valoes en factores para el MCA
ElegidosFactor <- data.frame(apply(elegidosSinTV[,c("P6", "P16", "P17", "P23", "P28", "P153", "P156A", "P156B", "P156D", "P156E", "P156H", "P156I", "P156J")] , 2, as.factor))

levels(ElegidosFactor$P16) <- c("Si", "No")
levels(ElegidosFactor$P17) <- c("Si", "No")
levels(ElegidosFactor$P23) <- c("Internet Fijo", "Modem", "Ninguno")
levels(ElegidosFactor$P28) <- c("Si", "No")
levels(ElegidosFactor$P153) <- c("Primaria o menos", "Secundaria incompleta", "Secundaria completa", "Técnico", "Educ. Terciaria", "Universidad incompleta", "Universidad Completa", "Postgrado")
levels(ElegidosFactor$P156A) <- c("Si", "No")
levels(ElegidosFactor$P156B) <- c("Si", "No")
levels(ElegidosFactor$P156D) <- c("Si", "No")
levels(ElegidosFactor$P156E) <- c("Si", "No")
levels(ElegidosFactor$P156H) <- c("Si", "No")
levels(ElegidosFactor$P156J) <- c("Si", "No")

summary(ElegidosFactor)
#Generando el análisis de componentes principales con restricción de no negatividad 


library(nsprcomp)
PCA1 <- nsprcomp(elegidosSinTV, nneg = TRUE)

#Recontruyendo la matriz para los valores de PCA
j1 <- matrix(elegidosSinTV[,c("P6")], nrow = 1738, ncol = 1 , byrow = FALSE)
j2 <- matrix(elegidosSinTV[,c("P16")], nrow = 1738, ncol = 1 , byrow = FALSE)
j3 <- matrix(elegidosSinTV[,c("P17")], nrow = 1738, ncol = 1 , byrow = FALSE)
j4 <- matrix(elegidosSinTV[,c("P23")], nrow = 1738, ncol = 1 , byrow = FALSE)
j5 <- matrix(elegidosSinTV[,c("P28")], nrow = 1738, ncol = 1 , byrow = FALSE)
j6 <- matrix(elegidosSinTV[,c("P153")], nrow = 1738, ncol = 1 , byrow = FALSE)
j7 <- matrix(elegidosSinTV[,c("P156A")], nrow = 1738, ncol = 1 , byrow = FALSE)
j8 <- matrix(elegidosSinTV[,c("P156B")], nrow = 1738, ncol = 1 , byrow = FALSE)
j9 <- matrix(elegidosSinTV[,c("P156D")], nrow = 1738, ncol = 1 , byrow = FALSE)
j10 <- matrix(elegidosSinTV[,c("P156E")], nrow = 1738, ncol = 1 , byrow = FALSE)
j11<- matrix(elegidosSinTV[,c("P156H")], nrow = 1738, ncol = 1 , byrow = FALSE)
j12 <- matrix(elegidosSinTV[,c("P156I")], nrow = 1738, ncol = 1 , byrow = FALSE)
j13 <- matrix(elegidosSinTV[,c("P156J")], nrow = 1738, ncol = 1 , byrow = FALSE)

m2 <- matrix(c(j1, j2, j3 , j4 ,j5 , j6 ,j7, j8, j9, j10, j11, j12, j13), nrow = 1738, ncol = 13)

m1 <- matrix(PCA1$rotation, nrow = 13, ncol = 12)

#Generando los valores y nombrandolos 
componentes <- m2%*%m1
componentesnames <- c("PC1", "PC2",  "PC3",  "PC4", "PC5",  "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12")


#Generando la clasificación en base al primer componente principal
grupos = hclust(dist(componentes[,1]), method = "complete")
plot(grupos)
soc_eco =cutree(grupos, k=3)
soc_eco <- factor(soc_eco)
levels(soc_eco) <- c("Bajo", "Medio", "Alto")
bd_clasificada <- bd_obs_completas
bd_clasificada["soc_eco"]<- soc_eco
head(bd_clasificada)


# análisis con la nueva clasificación
tc1 <- prop.table(table(bd_clasificada$P6, bd_clasificada$soc_eco))
barplot(tc1,bside=T)

P15A <- factor(bd_clasificada$P15A)
levels(bd_clasificada$P15A) <- c("Noticioas", "Deportes", "Salud y educación", "Entretenimiento", "Otro", "No mira TV")
bd_clasificada[,"P15A"] <- P15A


tc2 <- prop.table(table(bd_clasificada$P15A, bd_clasificada$soc_eco), margin = 2)
tc2


P36A <- factor(bd_clasificada$P36A)
levels(P36A)<- c("Android", "iOS", "Windows", "Otro" )

bd_clasificada[,"P36A"] <- P36A

prop.table(table(bd_clasificada$P36A, bd_clasificada$soc_eco), margin=1)
barplot(prop.table(table(bd_clasificada$P36A, bd_clasificada$soc_eco), margin=1), beside=TRUE)


