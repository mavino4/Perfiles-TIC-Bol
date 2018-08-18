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
library(fmsb)


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
        # main = "RF - Variables Importantes",
        xlab = "MeanDecreaseAccuracy",horiz = T,
        cex.axis = 0.8,
        cex.names = 0.4,las=2)
abline(v=0.01,col="red")

vi.i_rf.mod_f <- data.frame(vi.i_rf.mod[vi.i_rf.mod>0.01][order(vi.i_rf.mod[vi.i_rf.mod>0.01],decreasing = T)])
vi.i_rf.mod_f

vSelec <- as.data.frame(vi.i_rf.mod_f)
colnames(vSelec) <- c("Var")
vSelec$X <- runif(length(vSelec$Var), min = 0, max = 100)
vSelec$Y <- runif(length(vSelec$Var), min = 0, max = 100)

qplot(x = X, y = Y, data = vSelec, geom = "point") +  geom_point( aes(size=vSelec$Var), shape = 21, colour = "green2", fill = "green3", stroke =5)  + 
   geom_text(aes(label = rownames(vSelec)), size= 6, color="Black") +theme_classic() 

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
i_new_data_f$P11A<- Internautas$P11A
i_new_data_f$P11B<- Internautas$P11B
i_new_data_f$P11C<- Internautas$P11C
i_new_data_f$P11D<- Internautas$P11D
i_new_data_f$P11E<- Internautas$P11E
i_new_data_f$P11F<- Internautas$P11F
i_new_data_f$P11G<- Internautas$P11G
i_new_data_f$P19 <- Internautas$P19

# Definiendo funciones que apoyaran el análisis

dfToProp <- function(df){
  n = length(colnames(df))
  sumas = rowSums(df)
  for (i in colnames(df)){
    df[,i] <- df[,i] / sumas
  }
  return(df )
}

## Intensidad de uso
dfToPercapita <- function(df){
  n = length(colnames(df))
  personas = c(length(which(i_new_data_f$cluster == "A")), length(which(i_new_data_f$cluster == "B")), length(which(i_new_data_f$cluster == "C")) , length(which(i_new_data_f$cluster == "D")) )
  for (i in colnames(df)){
    df[,i] <- df[,i] / personas
  }
  data = df 
  data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
  radarchart( data  , axistype=1, 
              #custom polygon
              pcol=c("red3", "springgreen4", "steelblue3", "purple3",   "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                     rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
              #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
              plwd=2, 
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="white", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
              cglwd=0.4,
              #custom labels
              vlcex=1.6)
  
}

## Preferencia relativa respecto a todos los usos 
dfToPreRela <- function(df){
  n = length(colnames(df))
  personas = c(max(df[1,]), max(df[2,]), max(df[3,]) , max(df[4,])) 
  for (i in colnames(df)){
    df[,i] <- df[,i] / personas
  }
  data = df
  data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
  radarchart( data, axistype=1, 
              #custom polygon
              pcol=c("red3", "springgreen4", "steelblue3", "purple3",   "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                     rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
              #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
              plwd=2, 
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="gray", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
              cglwd=0.4,
              #custom labels
              vlcex=1.6)
}

# Edades 
tapply(i_new_data_f$P1, i_new_data_f$cluster, summary)
ggplot(i_new_data_f, aes(x=cluster, y=P1, fill=cluster)) + 
  geom_boxplot() + ylab("edad (años)") #box plot  
ggsave("Graphs/P1_edad.png", width = 6, height = 6)


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


#### Días que utliza la PC, portatil o tablet
i_new_data_f$P10 <- as.factor(i_new_data_f$P10)
levels(i_new_data_f$P10) <- c(1,2,3,4,5,6,7,"< 1/s ", "No Usa")
a = round(prop.table(table(i_new_data_f$P10, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P10", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P10, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=4, position = position_stack(vjust = 0.5)) + theme_minimal() + 
  scale_fill_brewer(palette = "Spectral") 
Sums(is.na(i_new_data_f$P10))
# Guardando 
ggsave("Graphs/P10_dias_computadora.eps", width = 6, height = 6)


# Usos de la computadora P11.
table(i_new_data_f$P11A)
table(i_new_data_f$P11B)
table(i_new_data_f$P11C)
table(i_new_data_f$P11F)

a1 <- as.data.frame(table(i_new_data_f$P11A, i_new_data_f$cluster))
a2 <- as.data.frame(table(i_new_data_f$P11B, i_new_data_f$cluster))
a3 <- as.data.frame(table(i_new_data_f$P11C, i_new_data_f$cluster))

total <- merge(a1,a2,by=c("Var1","Var2")) 
#total <- merge(total,a3,by=c("Var1","Var2"))
total$FreqT <- total$Freq.x + total$Freq.y
total[,3:4] <- NULL
df <- data.frame(total[which(total$Var1 ==1),3], total[which(total$Var1 ==2),3], total[which(total$Var1 ==3),3], total[which(total$Var1 ==4),3], total[which(total$Var1 ==5),3], total[which(total$Var1 ==6),3] )
colnames(df) <- c("Herramienta de trabajo", "Multimedia", "Juegos", "Internet", "Estudio", "Otro" )
row.names(df)<- c("A","B","C","D") 

data <- df
data <-  dfToProp(df)
data <- dfToPercapita(df) # Intensidad de uso 
data
data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
radarchart( data  , axistype=1, 
            #custom polygon
            pcol=c("red3", "springgreen4", "steelblue3", "purple3",   "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                   rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
            #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
            plwd=2, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="white", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
            cglwd=0.4,
            #custom labels
            vlcex=1.6)

data <- dfToPreRela(df) # Preferencia relativa uso
data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
radarchart( data  , axistype=1, 
            #custom polygon
            pcol=c("red3", "springgreen4", "steelblue3", "purple3",   "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                   rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
            #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
            plwd=2, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="gray", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
            cglwd=0.4,
            #custom labels
            vlcex=1.6)




# Televisores 
i_new_data_f$P12 <- Internautas$P12
table(i_new_data_f$P12)

a = round(prop.table(table(i_new_data_f$P12, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P12", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P12, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5))
ggsave("Graphs/P12_tiene_TV.png", width = 6, height = 6)



# Uso de la TV 
i_new_data_f$P15A <- Internautas$P15A 
i_new_data_f$P15B <- Internautas$P15B
i_new_data_f$P15C <- Internautas$P15C

table(i_new_data_f$P15C)

a1 <- as.data.frame(table(i_new_data_f$P15A, i_new_data_f$cluster))
a2 <- as.data.frame(table(i_new_data_f$P15B, i_new_data_f$cluster))
a3 <- as.data.frame(table(i_new_data_f$P15C, i_new_data_f$cluster))
total <- merge(a1,a2,by=c("Var1","Var2")) 
#total <- merge(total,a3,by=c("Var1","Var2"))
total$FreqT <- total$Freq.x + total$Freq.y
total[,3:4] <- NULL
df <- data.frame(total[which(total$Var1 ==1),3], total[which(total$Var1 ==2),3], total[which(total$Var1 ==3),3], total[which(total$Var1 ==4),3], total[which(total$Var1 ==5),3], total[which(total$Var1 ==6),3] )
# Cambiando las etiquetas en función de las opciones 
colnames(df) <- c("Noticias", "Deportes", "Salud y educ.", "Entretenimiento", "Otro", "No mira TV" )
row.names(df)<- c("A","B","C","D") 
data <-  dfToProp(df)
dfToPercapita(df) # Intensidad de uso 
dfToPreRela(df) # Preferencia relativa uso

### Nivel de satisfacción con el servicio de TV
# Calculando la 
a<-  as.data.frame(rbind(table(i_new_data_f$P19, i_new_data_f$cluster)))
a <- a[1:5,]
a$val <- c(1:5)
colSums(a)  
a
for (k in c(1:4)){
  print(k)
  ap = sum((a[,k]*a[,"val"]))/ colSums(a)[k]
  print(ap)
}


# Análisis de la radio
i_new_data_f$P20 <- Internautas$P20
table(i_new_data_f$P20)
# SI NO 
i_new_data_f$P12 <- Internautas$P12
table(i_new_data_f$P12)

a = round(prop.table(table(i_new_data_f$P12, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P12", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P12, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5))
ggsave("Graphs/P12_tiene_TV.png", width = 6, height = 6)





### 

# Nivel de instrucción del entrevistado 
levels(i_new_data_f$P149) <- c("Primaria o menos","Secundaria incompleta", "Secundaria completa", "Técnico", "FFAA,Policía, Normal", "Univeridad incompleta", "Universidad completa", "Posgrado")

round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster)),2)
round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster), margin=2),2)
round(prop.table(table(i_new_data_f$P149, i_new_data_f$cluster), margin =1),2)
ggplot(i_new_data_f, aes(x=P149, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())



