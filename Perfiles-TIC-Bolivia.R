# Perfiles TIC Bolivia 
# ¿Qué y cómo ? Un enfoque de conglomerados para el caso boliviano
" Marco Antonio Vino Chipana & Ayar Yuman Paco Sanizo" 


############ Importando librerias ###############

library(readxl)
library(readr)
library(ggplot2)
library(Amelia)
library(VIM)
library(mice)
library(grid)
library(gridExtra)
require(data.table)


############ Importando la base de datos  #####################

bd <- read_delim("bases de datos/base-5536-bdfinalcorregido.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)
bd <- as.data.table(bd)


############ Análisis de los ponderadores ####################

bd$Ponderador5033[5034:5536] <- 0
# Ajustando los ponderadores de pesos a ponderadores de frecuencias
minPon5033 = min(bd$Ponderador5033[bd$Ponderador5033 > 0])
minPon5536 = min(bd$Ponderador5536[bd$Ponderador5536 > 0])
bd$Ponderador5033_2 <- (bd$Ponderador5033 / minPon5033)*100
bd$Ponderador5536_2 <- (bd$Ponderador5536 / minPon5536)*100

# Ponderadores a nivel persona 
bd$Ponderador5033_3 <- round(bd$Ponderador5033_2)
bd$Ponderador5536_3 <- round(bd$Ponderador5536_2)

#ponderadores a nivel cada 100 personas
bd$Ponderador5033_4 <- round(bd$Ponderador5033_3/100)
bd$Ponderador5536_4 <- round(bd$Ponderador5536_3/100)


#Espandiendo las bases
bd$Ponderador5033_3
expandidoTotal <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5536_4),]
expandidoInternautas <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5033_4),]



############## Análisis de valores faltantes 

# Separando las bases de datos
Internautas = expandidoInternautas
NoInter = expandidoTotal[expandidoTotal$Tipo == 2]


missmap(bd,y.labels=NULL,
        y.at=NULL,x.cex = 0.1,
        main = "Mapa de valores faltantes")
missmap(Internautas,y.labels=NULL,
        y.at=NULL,x.cex = 0.1,
        main = "Mapa de valores faltantes")
missmap(NoInter,y.labels=NULL,
        y.at=NULL,x.cex = 0.1,
        main = "Mapa de valores faltantes")

# CONSIDERANDO COMO GRUPO DE ESTUDIO A LOS INTERNAUTAS 
# Ordendando las variables ne función de la cantidad de valores perdidos
mice_plot <- aggr(Internautas,
                  col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Internautas), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"),
                  plot = F)

missing_data <- mice_plot$missings
row.names(missing_data) <- NULL
missing_data$Percent <- missing_data$Count/length(Internautas$P152)
missing_data[order(missing_data$Percent),]

#Considerando aquellas que tienen las observaciones completas
var_consideradas <- missing_data[missing_data$Percent<=0.00,1]
var_consideradas

# Nueva Filtrada Base de Datos para Internautas 
Inter_f <- Internautas[,..var_consideradas]
missmap(obj = Inter_f, y.labels = NULL,
        y.at=NULL, x.cex = 0.4,
        main = "Mapa de valores faltantes")


# Eliminando las columnas que no entraran en el análisis

Inter_f$P2   <- as.factor(Inter_f$P2) 
Inter_f$P4   <- as.factor(Inter_f$P4)
Inter_f$P6   <- as.factor(Inter_f$P6)
Inter_f$P10  <- as.factor(Inter_f$P10)
Inter_f$P12  <- as.factor(Inter_f$P12)
Inter_f$P16  <- as.factor(Inter_f$P16)
Inter_f$P17  <- as.factor(Inter_f$P17)
Inter_f$P20  <- as.factor(Inter_f$P20)
Inter_f$P23  <- as.factor(Inter_f$P23)
Inter_f$P28  <- as.factor(Inter_f$P28)
Inter_f$P148  <- as.factor(Inter_f$P148)
Inter_f$P149  <- as.factor(Inter_f$P149)
Inter_f$P150  <- as.factor(Inter_f$P150)
Inter_f$P151  <- as.factor(Inter_f$P151)
Inter_f$P153  <- as.factor(Inter_f$P153)
Inter_f$P154A <- as.factor(Inter_f$P154A)
Inter_f$P155  <- as.factor(Inter_f$P155)
Inter_f$P156A <- as.factor(Inter_f$P156A)
Inter_f$P156B <- as.factor(Inter_f$P156B)
Inter_f$P156C <- as.factor(Inter_f$P156C)
Inter_f$P156D <- as.factor(Inter_f$P156D)
Inter_f$P156E <- as.factor(Inter_f$P156E)
Inter_f$P156F <- as.factor(Inter_f$P156F)
Inter_f$P156G <- as.factor(Inter_f$P156G)
Inter_f$P156H <- as.factor(Inter_f$P156H)
Inter_f$P156I <- as.factor(Inter_f$P156I)
Inter_f$P156J <- as.factor(Inter_f$P156J)
Inter_f$P156K <- as.factor(Inter_f$P156K)
Inter_f$P157  <- as.factor(Inter_f$P157)
Inter_f$P158  <- as.factor(Inter_f$P158)
Inter_f$P159  <- as.factor(Inter_f$P159)

Inter_f$P1 <- scale(Inter_f$P1, center= TRUE, scale=TRUE)

names(Inter_f)
#Inter_f_Values <- Inter_f[,c("NUMBOL", "Tipo", "P45A", "P44A", "P64A")]
Inter_f$NUMBOL <- NULL
Inter_f$Ponderador5536 <- NULL
Inter_f$Ponderador5033 <- NULL
Inter_f$Ponderador5536_2 <- NULL
Inter_f$Ponderador5033_2 <- NULL
Inter_f$Ponderador5536_3 <- NULL
Inter_f$Ponderador5033_3 <- NULL
Inter_f$Ponderador5536_4 <- NULL
Inter_f$Ponderador5033_4 <- NULL
Inter_f$Nse <- NULL
Inter_f$CodP160 <- NULL
Inter_f$Tipo <- NULL
Inter_f$P160 <- NULL
Inter_f$P45A <- NULL
Inter_f$P44A <- NULL
Inter_f$P64A <- NULL


names(Inter_f)



## Primera Clasificación 

Inter_f <- data.frame(Inter_f)
library(clustMixType)
set.seed(12345)
i_clust.mod <- kproto(Inter_f,4)
i_clust.table <- table(i_clust.mod$cluster)
round(i_clust.table/nrow(Inter_f)*100,2)

names(Inter_f)


# Identificando Variables a través de Random Forest 
library(randomForest)
i_new_data <- Inter_f
i_new_data$cluster <- as.factor(i_clust.mod$cluster)

set.seed(12345)
train = sample(1:nrow(i_new_data), nrow(i_new_data)/2)

set.seed(12345)
i_rf.mod <- randomForest(cluster ~ ., data = i_new_data, subset=train,
                         importance=TRUE, ntree=2000)

clust.i_rf.hat <- predict(i_rf.mod,newdata = i_new_data[-train,])
i_ct <- table(clust.i_rf.hat,i_new_data[-train,]$cluster)
i_ct 
i_ct <- as.matrix(i_ct)
round(sum(diag(i_ct))/nrow(i_new_data[-train,])*100,2)

vi.i_rf.mod <- i_rf.mod$importance[,4]
barplot(vi.i_rf.mod[order(vi.i_rf.mod)],
        main = "RF - Variables Importantes",
        xlab = "MeanDecreaseAccuracy",horiz = T,
        cex.axis = 0.8,
        cex.names = 0.4,las=2)
abline(v=0.01,col="red")

vi.i_rf.mod_f <- data.frame(vi.i_rf.mod[vi.i_rf.mod>0.01][order(vi.i_rf.mod[vi.i_rf.mod>0.01],decreasing = T)])
vi.i_rf.mod_f


# Clasificación con nuevos valores 

i_input_f <- Inter_f[,rownames(vi.i_rf.mod_f)]
set.seed(12345)
i_clust.mod_f <- kproto(i_input_f,4)

i_clust.table_f <- table(i_clust.mod_f$cluster)
round(i_clust.table_f/nrow(i_input_f)*100,2)


# Random Forest con los nuevos valores
i_new_data_f <- i_input_f
i_new_data_f$cluster <- as.factor(i_clust.mod_f$cluster)

set.seed(12345)
i_rf.mod_f <- randomForest(cluster ~ ., data = i_new_data_f, subset=train,
                           importance=TRUE, ntree=2000)

clust.i_rf_f.hat <- predict(i_rf.mod_f,newdata = i_new_data_f[-train,])
i_ct_f <- table(clust.i_rf_f.hat,i_new_data_f[-train,]$cluster)
i_ct_f <- as.matrix(i_ct_f)
i_ct_f
round(sum(diag(i_ct_f))/nrow(i_new_data_f[-train,])*100,2)



### Análisis de resultados 

i_new_data_f$cluster = factor(i_new_data_f$cluster,
                              levels(i_new_data_f$cluster)[c(1,3,2,4)])
levels(i_new_data_f$cluster) <- c("A","B","C","D")
round(prop.table(table(i_new_data_f$cluster))*100,2)

library(ggplot2)
#inlcuyendo variables para el análisis
i_new_data_f$P1    <- Internautas$P1
i_new_data_f$P150  <- Internautas$P150
i_new_data_f$P23   <- Internautas$P23
i_new_data_f$P134  <- Internautas$P134
i_new_data_f$P131A <- Internautas$P131A
i_new_data_f$P23   <- Internautas$P23
i_new_data_f$P36A  <- Internautas$P36A
i_new_data_f$P136  <- Internautas$P136
i_new_data_f$P129A <- Internautas$P129A
i_new_data_f$P133A <- Internautas$P133A
i_new_data_f$P15A <- Internautas$P15A
i_new_data_f$P3<- Internautas$P3

# Edades 
tapply(i_new_data_f$P1, i_new_data_f$cluster, summary)
ggplot(i_new_data_f, aes(x=cluster, y=P1, fill=cluster)) + 
  geom_boxplot() + ylab("edad (años)") #box plot 


# Análisis de última conexion
# Añadiendo etiquetas
i_new_data_f$P3 <- as.factor(i_new_data_f$P3)
levels(i_new_data_f$P3) <- c("[,7]","[8,15]", "[16,30]")

# Generando DF
a = round(prop.table(table(i_new_data_f$P3, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P3", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P3, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5))

# ¿Tiene computadora de escritorio, computadora portátil o tablet en su casa?
a = round(prop.table(table(i_new_data_f$P6, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P6", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P6, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5))


#### Días que utliza la PC, portatil o tablet en casa 
a = round(prop.table(table(i_new_data_f$P10, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P10", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P10, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5)) + theme_minimal() + scale_fill_brewer( 
    palette = "Greens"
  ) 




i_new_data_f$P10 <- as.factor(i_new_data_f$P10)
levels(i_new_data_f$P10) <- c(1,2,3,4,5,6,7,"< 1/s ", "No Usa")
table(i_new_data_f$P10)
ggplot(i_new_data_f, aes(x=P10, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position="fill") + scale_fill_brewer(palette = "RdYlGn") 
  #scale_fill_manual(values= c("skyblue", "royalblue", "blue", "navy"))

Sums(is.na(i_new_data_f$P10))


# Nivel de instrucción del entrevistado 
levels(i_new_data_f$P149) <- educ_labels <- c("Primaria o menos","Secundaria incompleta", "Secundaria completa", "Técnico", "FFAA,Policía, Normal", "Univeridad incompleta", "Universidad completa", "Posgrado")

round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster)),2)
round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster), margin=2),2)
round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster), margin =1),2)
ggplot(i_new_data_f, aes(x=P149, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())



