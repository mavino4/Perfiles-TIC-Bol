# Library
library(fmsb)

data = as.data.frame(t(rbind(round(prop.table(table(i_new_data_f$P138 , i_new_data_f$cluster), margin =2),2))))
data
colnames(data)=c("Tv", "Radio", "P. Imp.", "P. Dig.", "P. Web", "RR SS", "Ninguno", "NS/NR" )

#colnames(data)=c("Tv", "Radio", "P. Imp.", "P. Dig.", "Páginas web", "Redes sociales", "Charlas A.", "Otro", "NS/NR" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(max(data),length(colnames(data))) , rep(min(data),length(colnames(data))) , data)
# The default radar chart proposed by the library:
radarchart(data)

# Custom the radarChart !
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=c(rgb(0.2,0.5,0.3,0.8), rgb(0.2,0.5,0.7,0.8), rgb(0.2,0.2,0.3,0.8), "deeppink4" ,rgb(0.2,0.0,0.3,0.8),
                  rgb(0.2,0.0,0.3,0.8), rgb(0.2,0.5,0.3,0.8) ,rgb(0.1,0.5,0.3,0.8), rgb(0.4,0.5,0.3,0.8)) ,
                   #, pfcol=rgb(0.2,0.5,0.5,0.5) ,
            plwd=3, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="gray", caxislabels=round(seq(0,max(data),max(data)/4),2) , 
            cglwd=0.8,
            #custom labels
            vlcex=0.8, 
            pangle = 30, 
            title = "¿Qué medio de comunicación considera imparcial?"
            
)



############ gráfica para SI NO
i_new_data_f$P6 <- as.factor(i_new_data_f$P6)
levels(i_new_data_f$P6) <- c("Si","No")

round(prop.table(table(i_new_data_f$P6, i_new_data_f$cluster), margin =2),2)
p <- ggplot(i_new_data_f, aes(cluster))
p + geom_bar (aes(fill=P6), position = "fill") 



