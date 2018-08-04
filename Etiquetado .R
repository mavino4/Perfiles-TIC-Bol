# Explorando el etiquetado de las variables 

require(gdata)
(fac <- factor(c("B", "A", "Z", "D")))

(map <- mapLevels(x=fac))
# Volviendo a ser Int

str(map)

##### 

fac =factor( c("No existe el servicio en el barrio/localidad", "El costo del servicio alto", 
               "No tiene equipos para conectarse", "No sabe qué es Internet", "No sabe cómo usar Internet", 
               "No está interesado en Internet", "El contenido de Internet es malo", "Otro" ))
map <- mapLevels(x=fac)
map
bd$P1C = factor(bd$P1C)
table(bd$P1C)
bd$P1C <- as.character(bd$P1C)
levels(bd$P1C) <- list("1"="No existe el servicio en el barrio/localidad", "2" = "El costo del servicio alto", "3"  = "No tiene equipos para conectarse", "4" = "No sabe qué es Internet", "5" =  "No sabe cómo usar Internet", "6" = "No está interesado en Internet", "7" = "El contenido de Internet es malo", "8" = "Otro" )

mapLevels(x=bd$P1C) <- map
table(bd$P1C)

#### 
