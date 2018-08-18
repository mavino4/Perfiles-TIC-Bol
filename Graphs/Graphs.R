# Library
library(fmsb)
library(ggplot2)

data = as.data.frame(t(rbind(round(prop.table(table(i_new_data_f$P138 , i_new_data_f$cluster), margin =2),2))))
colnames(data)=c("Tv", "Radio", "P. Imp.", "P. Dig.", "P. Web", "RR SS", "Ninguno", "NS/NR" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
# The default radar chart proposed by the library:
radarchart(data)
radarChartCustom(data)
# Custom the radarChart !
radarChartCustom <- function(data){
  radarchart( data  , axistype=1 , 
              #custom polygon
              pcol=c("red3", "springgreen4", "steelblue3", "purple3",   "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                     rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
              #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
              plwd=3, 
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="gray", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
              cglwd=0.8,
              #custom labels
              vlcex=0.8, 
              pangle = 30)  
}




############ gráfica para SI NO  directo (sin etiquetas) 
i_new_data_f$P6 <- as.factor(i_new_data_f$P6)
levels(i_new_data_f$P6) <- c("Si","No")

p <- ggplot(i_new_data_f, aes(cluster))
p + geom_bar (aes(fill=P6), position = "fill") + ggtitle("¿Tienen Computadoras?") + 
  ylab("%") 

# Gráfica para SI NO con etiquetas
a = round(prop.table(table(i_new_data_f$P6, i_new_data_f$cluster), margin =2),2) 
a <- as.data.frame(a)
colnames(a) = c("P6", "Cluster", "Freq")
a
ggplot(a, aes(x = Cluster, y = Freq , fill = P6, label= Freq )) + 
  geom_bar (stat = "identity") + #ggtitle("¿Tienen Computadoras?") + 
  ylab("%")  + geom_text(size=5, position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette = "Set2")
  #scale_fill_manual(values = c("#33CC33","#FF3300"))

ggsave("Graphs/P6_tienen_computadoras.png", width = 6, height = 6)


#  geom_text(aes(label = y), position = position_stack(vjust = 0.5))


# Ejemplo etiquetas 
Year      <- c(rep(c("2006-07", "2007-08", "2008-09", "2009-10"), each = 4))
Category  <- c(rep(c("A", "B", "C", "D"), times = 4))
Frequency <- c(168, 259, 226, 340, 216, 431, 319, 368, 423, 645, 234, 685, 166, 467, 274, 251)
Data      <- data.frame(Year, Category, Frequency)
library(ggplot2)
p
ggplot(Data, aes(x = Year, y = Frequency, fill = Category, label = Frequency)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))



# Ejemplo de etiquetas 
df <- data.frame(
  x = factor(c(1, 1, 2, 2)),
  y = c(1, 3, 2, 1),
  grp = c("a", "b", "a", "b")
)
df 
ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp)) +
  geom_text(aes(label = y), position = position_stack(vjust = 0.5))

# Ejemplo etiquetas 2 
foo <- data.frame(Foo = c(11, 12, 13))
ggplot(foo) + aes(x = 0, y = Foo, fill = Foo, label = Foo) + geom_bar(stat = "identity") +
  geom_text()
# Text labels are above their respective bars.
ggplot(foo) + aes(x = 0, y = Foo, fill = Foo, label = Foo) + geom_bar(stat = "identity") +
  geom_text(y = cumsum(foo$Foo))

ggplot(foo) + aes(x = 0, y = Foo, fill = Foo, label = Foo) + 
  geom_bar(stat = "identity") +
  geom_text(position = "stack")



###########3 Pai charts 

a <- as.data.frame((table(i_new_data_f$cluster)))
a$label <- c("24%", "27%" ,"34%" ,"15%")
a$cluster <- a$Var1
table(i_new_data_f$cluster)

ggplot(a, aes(x="", y=Freq, fill=cluster, label = label))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y") + theme(axis.text.x=element_blank()) +
  geom_text(size=5, position = position_stack(vjust = 0.5))

ggsave("Graphs/porcentajes_clusters.png", width = 6, height = 6)


table(expandidoTotal$Ponderador5536_4)
length(which(expandidoTotal$Ponderador5033_4>0))
length(which(expandidoTotal$Ponderador5033_4==0))

a$Freq <- c(67, 33) 
a$categoria <- c("Internauta", "No Internauta")
a$label <- c("68%", "32%")
ggplot(a, aes(x="", y=Freq, fill=categoria, label = label))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y") + theme(axis.text.x=element_blank()) +
  geom_text(size=5, position = position_stack(vjust = 0.5))+ scale_fill_brewer(palette = "Set1", direction = -1)

ggsave("Graphs/porcentajes_internautas.png", width = 6, height = 6)

