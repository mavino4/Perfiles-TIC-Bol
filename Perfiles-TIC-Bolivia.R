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
############ Etiquetado de las variables ############

## definiendo algunas etiquetas comunes para los datos 
inter_no <- c("Internauta", "No Internauta")
si_no <- c("SI","NO") # 1 = Si , 2 = No


# Clasificando datos categóricos y datos numéricos
bd$Tipo <- as.factor(bd$Tipo)
levels(bd$Tipo) <- inter_no
bd$P2   <- as.factor(bd$P2) 
levels(bd$P2) <- si_no
bd$P4   <- as.factor(bd$P4)
levels(bd$P4)<- si_no
bd$P6   <- as.factor(bd$P6)
levels(bd$P6)<- si_no

bd$P10  <- as.factor(bd$P10)
bd$P12  <- as.factor(bd$P12)
bd$P15A <- as.factor(bd$P15A)
bd$P16  <- as.factor(bd$P16)
bd$P17  <- as.factor(bd$P17)
bd$P20  <- as.factor(bd$P20)
bd$P23  <- as.factor(bd$P23)
bd$P28  <- as.factor(bd$P28)
bd$P31A <- as.factor(bd$P31A) 
bd$P32  <- as.factor(bd$P32)
bd$P33  <- as.factor(bd$P33)
bd$P35  <- as.factor(bd$P35)
bd$P148  <- as.factor(bd$P148)
bd$P149  <- as.factor(bd$P149)
bd$P150  <- as.factor(bd$P150)
bd$P151  <- as.factor(bd$P151)
bd$P152  <- factor(bd$P152,labels = c("AS","CP","NP","PI","EM","ES","AC","RJ","SB","SN"))
bd$P153  <- as.factor(bd$P153)
bd$P154A <- as.factor(bd$P154A)
bd$P155  <- as.factor(bd$P155)
bd$P156A <- as.factor(bd$P156A)
bd$P156B <- as.factor(bd$P156B)
bd$P156C <- as.factor(bd$P156C)
bd$P156D <- as.factor(bd$P156D)
bd$P156E <- as.factor(bd$P156E)
bd$P156F <- as.factor(bd$P156F)
bd$P156G <- as.factor(bd$P156G)
bd$P156H <- as.factor(bd$P156H)
bd$P156I <- as.factor(bd$P156I)
bd$P156J <- as.factor(bd$P156J)
bd$P156K <- as.factor(bd$P156K)
bd$P157  <- as.factor(bd$P157)
bd$P158  <- as.factor(bd$P158)
bd$P159  <- as.factor(bd$P159)
bd$P160  <- as.factor(bd$P160) 
bd$CodP160 <- as.factor(bd$CodP160)
bd$Nse <- as.factor(bd$Nse) 

bd$P36A[bd$P36A==9] <- NA
bd$P36A  <- as.factor(bd$P36A)
bd$P129A[bd$P129A==9] <- NA
bd$P129A <- as.factor(bd$P129A)
bd$P130A[bd$P130A==9] <- NA
bd$P130A <- as.factor(bd$P130A)
bd$P131A[bd$P131A==9] <- NA
bd$P131A <- as.factor(bd$P131A)
bd$P132A[bd$P132A==9] <- NA
bd$P132A <- as.factor(bd$P132A)
bd$P133A[bd$P133A==9] <- NA
bd$P133A <- as.factor(bd$P133A)
bd$P134[bd$P134==9] <- NA
bd$P134  <- as.factor(bd$P134)
bd$P135[bd$P135==9] <- NA
bd$P135  <- as.factor(bd$P135)
bd$P136[bd$P136==9] <- NA
bd$P136  <- as.factor(bd$P136)
bd$P137[bd$P137==9] <- NA
bd$P137  <- as.factor(bd$P137)
bd$P138[bd$P138==9] <- NA
bd$P138  <- as.factor(bd$P138)
bd$P139[bd$P139==9] <- NA
bd$P139  <- as.factor(bd$P139)
bd$P140[bd$P140==9] <- NA
bd$P140  <- as.factor(bd$P140)
bd$P14[bd$P14==9]  <- NA
bd$P30[bd$P30==99] <- NA
bd$P34[bd$P34==99] <- NA


# Etiquendo Variables 
# position: 3
bd$P2 <- as.factor(bd$P2)
labels <- c("Si", "No" )
levels(bd$P2) <- labels
# position: 4
bd$P3 <- as.factor(bd$P3)
labels <- c("Hace 7 días o menos", "Entre 8 a 15 días", "Entre 16 a 30 días", "Hace más de 30 días" )
levels(bd$P3) <- labels
# position: 5
bd$P1A <- as.factor(bd$P1A)
labels <- c("No existe el servicio en el barrio/localidad", "El costo del servicio alto", 
            "No tiene equipos para conectarse", "No sabe qué es Internet", "No sabe cómo usar Internet", 
            "No está interesado en Internet", "El contenido de Internet es malo", "Otro" )
levels(bd$P1A) <- labels

# position: 6
bd$P1B <- as.factor(bd$P1B)
levels(bd$P1B) <- labels
table(bd$P1B)
# position: 7
bd$P1C <- factor(bd$P1C)
levels(bd$P1C) <- list(5="No sabe cómo usar Internet", 6 = "No está interesado en Internet", 7 = "El contenido de Internet es malo" )
list( 5 ="No sabe cómo usar Internet", 6 = "No está interesado en Internet", 7 = "El contenido de Internet es malo" )

table(bd$P1C)
# position: 8
labels <- list(1="No existe el servicio en el barrio/localidad", 2 = "El costo del servicio alto", 3  = "No tiene equipos para conectarse", 4 = "No sabe qué es Internet", 5 =  "No sabe cómo usar Internet", 6 = "No está interesado en Internet", 7 = "El contenido de Internet es malo", 8 = "Otro" )
bd$P1D <- as.factor(bd$P1D)
levels(bd$P1D) <- list(1="No existe el servicio en el barrio/localidad", 2 = "El costo del servicio alto", 3  = "No tiene equipos para conectarse", 4 = "No sabe qué es Internet", 5 =  "No sabe cómo usar Internet", 6 = "No está interesado en Internet", 7 = "El contenido de Internet es malo", 8 = "Otro" )

# position: 10

bd$P2A <- as.factor(bd$P2A)
labels <- c("Obtener información", "Descargar/imprimir documentos", "Obtener algún formulario", "Enviar/recibir correo electrónico", "Contactar a algún familiar/amigo", "Comprar/pagar artículos/servicios por Internet", "Revisar alguna red social (Facebook, WhatsApp, etc)", "Otro", "Ninguno" )
levels(bd$P2A) <- labels
# position: 11
bd$P2B <- as.factor(bd$P2B)
labels <- c("Obtener información", "Descargar/imprimir documentos", "Obtener algún formulario", "Enviar/recibir correo electrónico", "Contactar a algún familiar/amigo", "Comprar/pagar artículos/servicios por Internet", "Revisar alguna red social (Facebook, WhatsApp, etc)", "Otro", "Ninguno" )
levels(bd$P2B) <- labels
# position: 12
bd$P2C <- as.factor(bd$P2C)
labels <- c("Obtener información", "Descargar/imprimir documentos", "Obtener algún formulario", "Enviar/recibir correo electrónico", "Contactar a algún familiar/amigo", "Comprar/pagar artículos/servicios por Internet", "Revisar alguna red social (Facebook, WhatsApp, etc)", "Otro", "Ninguno" )
levels(bd$P2C) <- labels
# position: 13
bd$P2D <- as.factor(bd$P2D)
labels <- c("Obtener información", "Descargar/imprimir documentos", "Obtener algún formulario", "Enviar/recibir correo electrónico", "Contactar a algún familiar/amigo", "Comprar/pagar artículos/servicios por Internet", "Revisar alguna red social (Facebook, WhatsApp, etc)", "Otro", "Ninguno" )
levels(bd$P2D) <- labels
# position: 16
bd$P4A <- as.factor(bd$P4A)
labels <- c("Muy interesado", "M^s o menos interesado", "Poco interesado", "Nada interesado", "NS/NR" )
levels(bd$P4A) <- labels
# position: 17
bd$P4 <- as.factor(bd$P4)
labels <- c("Si", "No" )
levels(bd$P4) <- labels
# position: 19
bd$P6 <- as.factor(bd$P6)
labels <- c("Si", "No" )
levels(bd$P6) <- labels
# position: 20
#bd$P7A <- as.factor(bd$P7A)
#labels <- c("NS/NR" )
#levels(bd$P7A) <- labels
# position: 22
bd$P8 <- as.factor(bd$P8)
labels <- c("Computadora de escritorio", "Computadora portátil", "Tablet", "NS/NR" )
levels(bd$P8) <- labels
# position: 24
bd$P10 <- as.factor(bd$P10)
labels <- c(1,2,3,4,5,6,7, "Menos de una vez por semana", "No usa estos equipos" )
levels(bd$P10) <- labels
# position: 25
bd$P11A <- as.factor(bd$P11A)
labels <- c("Herramienta de trabajo", "Escuchar música o ver videos", "Juegos", "Conectarse a Internet", "Estudio", "Otro" )
levels(bd$P11A) <- labels
# position: 26
bd$P11B<- as.factor(bd$P11B)
labels <- c("Herramienta de trabajo", "Escuchar música o ver videos", "Juegos", "Conectarse a Internet", "Estudio", "Otro" )
levels(bd$P11B) <- labels
# position: 27
bd$P11C <- as.factor(bd$P11C)
labels <- c("Herramienta de trabajo", "Escuchar música o ver videos", "Juegos", "Conectarse a Internet", "Estudio", "Otro" )
levels(bd$P11C) <- labels
# position: 28
bd$P11F <- as.factor(bd$P11F)
labels <- c("Herramienta de trabajo", "Escuchar música o ver videos", "Juegos", "Conectarse a Internet", "Estudio", "Otro" )
levels(bd$P11F) <- labels
# position: 30
bd$P12 <- as.factor(bd$P12)
labels <- c("Si", "No" )
levels(bd$P12) <- labels
# position: 31
bd$P13A <- as.factor(bd$P13A) # Meses
# position: 32
bd$P13B <- as.factor(bd$P13B) # Años

# position: 34
bd$P15A <- as.factor(bd$P15A)
labels <- c("Noticias", "Deportes", "Salud y educación", "Entretenimiento (series, novelas, películas, farándula, moda, concursos)", "Otro", "No mira televisión" )
levels(bd$P15A) <- labels
# position: 35
bd$P15B <- as.factor(bd$P15B)
labels <- c("Noticias", "Deportes", "Salud y educación", "Entretenimiento (series, novelas, películas, farándula, moda, concursos)", "Otro", "No mira televisión" )
levels(bd$P15B) <- labels
# position: 36
bd$P15C <- as.factor(bd$P15C)
labels <- c("Noticias", "Deportes", "Salud y educación", "Entretenimiento (series, novelas, películas, farándula, moda, concursos)", "Otro", "No mira televisión" )
levels(bd$P15C) <- labels
# position: 37
bd$P15E <- as.factor(bd$P15E)
labels <- c("Noticias", "Deportes", "Salud y educación", "Entretenimiento (series, novelas, películas, farándula, moda, concursos)", "Otro", "No mira televisión" )
levels(bd$P15E) <- labels
# position: 39
bd$P16 <- as.factor(bd$P16)
labels <- c("Si", "No" )
levels(bd$P16) <- labels
# position: 40
bd$P17 <- as.factor(bd$P17)
labels <- c("Si", "No" )
levels(bd$P17) <- labels
# position: 43
bd$P20 <- as.factor(bd$P20)
labels <- c("Si", "No" )
levels(bd$P20) <- labels
# position: 44
bd$P21A <- as.factor(bd$P21A) # MESES
# position: 45
bd$P21B <- as.factor(bd$P21B) # AÑOS

# position: 47
bd$P23 <- as.factor(bd$P23)
labels <- c("Internet fijo", "Módem", "Ninguno" )
levels(bd$P23) <- labels
# position: 49
bd$P25 <- as.factor(bd$P25)
labels <- c("128 kbps", "256 kbps", "512 kbps", "1 a 2 Mbps", "De + de 2 a 3 Mbps", "De + de 3 Mbps", "NS/NR" )
levels(bd$P25) <- labels
# position: 51
bd$P27A <- as.factor(bd$P27A)
# position: 53
bd$P27B <- as.factor(bd$P27B)
# position: 54
bd$P27C <- as.factor(bd$P27C)
# position: 55
bd$P27F <- as.factor(bd$P27F)
# position: 56
bd$P27G <- as.factor(bd$P27G)

labels <- c("Contactar amigos o familiares", "Buscar información", "Comprar, vender o permutar", "Fines académicos", "Trabajo", "Ver noticias", "Descargar archivos", "Jugar", "Escuchar música o ver videos", "Redes sociales", "Correo electrónico", "Otro" )
levels(bd$P27A) <- labels
levels(bd$P27B) <- labels
levels(bd$P27C) <- labels
levels(bd$P27F) <- labels
levels(bd$P27G) <- labels

# position: 58
bd$P28 <- as.factor(bd$P28)
labels <- c("Si", "No" )
levels(bd$P28) <- labels


# position: 63
bd$P31B <- as.factor(bd$P31B)
# position: 64
bd$P31C <- as.factor(bd$P31C)
# position: 65
bd$P31D <- as.factor(bd$P31D)
# position: 66
bd$P31E <- as.factor(bd$P31E)
# position: 67
bd$P31G <- as.factor(bd$P31G)
# position: 68
bd$P31H <- as.factor(bd$P31H)
labels <- c("Llamadas telefónicas", "Mensajes de texto SMS", "Escuchar música", "Ver videos", "Juegos", "Conectarse a Internet", "Sacar fotos", "Otro" )
levels(bd$P31B) <- labels
levels(bd$P31C) <- labels
levels(bd$P31D) <- labels
levels(bd$P31E) <- labels
levels(bd$P31G) <- labels
levels(bd$P31H) <- labels

# position: 70
bd$P32 <- as.factor(bd$P32)
labels <- c("Prepago", "Pospago", "Ambos" )
levels(bd$P32) <- labels
# position: 71
bd$P33 <- as.factor(bd$P33)
labels <- c("Tigo", "Entel", "Viva", "Mio (Comteco)" )
levels(bd$P33) <- labels
# position: 74
bd$P36A <- as.factor(bd$P36A)
labels <- c("Android", "IOS (iPhone)", "Windows", "Otro")
levels(bd$P36A) <- labels
# position: 76
bd$P37 <- as.factor(bd$P37)
labels <- c("Si", "No" )
levels(bd$P37) <- labels
# position: 79
bd$P39B <- as.factor(bd$P39B) #Cantidad
labels <- c("Día", "Semana", "Mes" )
levels(bd$P39B) <- labels


# position: 81
bd$P41A <- as.factor(bd$P41A)
# position: 82
bd$P41B <- as.factor(bd$P41B)
# position: 83
bd$P41C <- as.factor(bd$P41C)
# position: 84
bd$P41D <- as.factor(bd$P41D)
# position: 85
bd$P41E <- as.factor(bd$P41E)
# position: 86
bd$P41G <- as.factor(bd$P41G)
# position: 87
bd$P41H <- as.factor(bd$P41H)
# position: 88
bd$P41I <- as.factor(bd$P41I)
# position: 89
bd$P41J <- as.factor(bd$P41J)
labels <- c("Contactar amigos o familiares", "Buscar información", "Compras en Internet", "Fines académicos", "Trabajo", "Ver noticias", "Descargar archivos", "Jugar", "Escuchar música o ver videos", "Redes sociales", "Correo electrónico", "Otro" )
levels(bd$P41A) <- labels
levels(bd$P41B) <- labels
levels(bd$P41C) <- labels
levels(bd$P41D) <- labels
levels(bd$P41E) <- labels
levels(bd$P41G) <- labels
levels(bd$P41H) <- labels
levels(bd$P41I) <- labels
levels(bd$P41J) <- labels



# position: 91
bd$P42A <- as.factor(bd$P42A)
# position: 92
bd$P42B <- as.factor(bd$P42B)
# position: 93
bd$P42C <- as.factor(bd$P42C)
# position: 94
bd$P42E <- as.factor(bd$P42E)
# position: 95
bd$P42F <- as.factor(bd$P42F)

labels <- c("En el trabajo", "En el Colegio", "En la Universidad", "Telecentro", "Cafe Internet", "Restaurantes, cafes", "Espacios públicos", "Casa de amigos/familiares", "Otro", "Ninguno" )
levels(bd$P42A) <- labels
levels(bd$P42B) <- labels
levels(bd$P42C) <- labels
levels(bd$P42E) <- labels
levels(bd$P42F) <- labels


# position: 146
bd$P46A <- as.factor(bd$P46A)
# position: 147
bd$P46B <- as.factor(bd$P46B)
# position: 148
bd$P46C <- as.factor(bd$P46C)
# position: 149
bd$P46E <- as.factor(bd$P46E)
# position: 150
bd$P46F <- as.factor(bd$P46F)
# position: 151
bd$P46G <- as.factor(bd$P46G)
# position: 152
bd$P46H <- as.factor(bd$P46H)
# position: 153
bd$P46I <- as.factor(bd$P46I)
labels <- c("Contactar amigos o familiares", "Buscar información", "Compras en Internet", "Fines académicos", "Trabajo", "Ver noticias", "Descargar archivos", "Jugar", "Escuchar música o ver videos", "Redes sociales", "Correo electrónico", "Otro" )
levels(bd$P46A) <- labels
levels(bd$P46B) <- labels
levels(bd$P46C) <- labels
levels(bd$P46E) <- labels
levels(bd$P46F) <- labels
levels(bd$P46G) <- labels
levels(bd$P46H) <- labels
levels(bd$P46I) <- labels


# position: 155
bd$P47A <- as.factor(bd$P47A)
# position: 156
bd$P47B <- as.factor(bd$P47B)
# position: 157
bd$P47C <- as.factor(bd$P47C)
# position: 158
bd$P47D <- as.factor(bd$P47D)
# position: 159
bd$P47E <- as.factor(bd$P47E)
# position: 160
bd$P47G <- as.factor(bd$P47G)
# position: 161
bd$P47H <- as.factor(bd$P47H)
# position: 162
bd$P47I <- as.factor(bd$P47I)
labels <- c("Noticias (periódicos, etc.)", "Oferta de bienes o servicios", "Oportunidades laborales", "Servicios de salud", "Ciencia y/o tecnología", "Servicios del gobierno", "Entretenimiento/farándula", "Turismo", "Educativa", "Negocios", "Política", "Arte y cultura", "Otro" )
levels(bd$P47A) <- labels
levels(bd$P47B) <- labels
levels(bd$P47C) <- labels
levels(bd$P47D) <- labels
levels(bd$P47E) <- labels
levels(bd$P47G) <- labels
levels(bd$P47H) <- labels
levels(bd$P47I) <- labels


# position: 170
bd$P50 <- as.factor(bd$P50)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P50) <- labels
# position: 171
bd$P51 <- as.factor(bd$P51)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P51) <- labels
# position: 172
bd$P52 <- as.factor(bd$P52)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P52) <- labels
# position: 173
bd$P53 <- as.factor(bd$P53)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P53) <- labels
# position: 174
bd$P54 <- as.factor(bd$P54)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P54) <- labels
# position: 175
bd$P55A <- as.factor(bd$P55A)
labels <- c("Claridad de la información", "Privacidad", "Trato cordial", "Rapidez", "Cantidad de requisitos", "Otro", "NS/NR" )
levels(bd$P55A) <- labels
# position: 179
bd$P58A <- as.factor(bd$P58A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P58A) <- labels
# position: 180
bd$P58B <- as.factor(bd$P58B)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P58B) <- labels
# position: 181
bd$P59A <- as.factor(bd$P59A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P59A) <- labels
# position: 182
bd$P59B <- as.factor(bd$P59B)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P59B) <- labels
# position: 183
bd$P60A <- as.factor(bd$P60A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P60A) <- labels
# position: 184
bd$P60B <- as.factor(bd$P60B)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P60B) <- labels
# position: 185
bd$P61A <- as.factor(bd$P61A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P61A) <- labels
# position: 186
bd$P60C <- as.factor(bd$P60C)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P60C) <- labels
# position: 187
bd$P62A <- as.factor(bd$P62A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P62A) <- labels
# position: 188
bd$P62B <- as.factor(bd$P62B)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P62B) <- labels
# position: 189
bd$P63A <- as.factor(bd$P63A)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P63A) <- labels
# position: 190
bd$P63B <- as.factor(bd$P63B)
labels <- c("Si", "No", "NS/NR" )
levels(bd$P63B) <- labels

# position: 191
bd$P64A <- as.factor(bd$P64A)
# position: 192
bd$P64B <- as.factor(bd$P64B)
# position: 193
bd$P64C <- as.factor(bd$P64C)
# position: 194
bd$P64D <- as.factor(bd$P64D)

labels <- c("Datos estadísticos", "Información general sobre la institución y su organización interna", "Actividades y proyectos de la institución", "Libros, memorias, boletines y otras publicaciones", "Requerimiento de personal", "Compras y contrataciones", "Trómites y requisitos", "Auditoría interna", "Información sobre las actividades de las autoridades", "Normativas que rigen las funciones de la entidad", "Otro", "NS/NR" )
levels(bd$P64A) <- labels
levels(bd$P64B) <- labels
levels(bd$P64C) <- labels
levels(bd$P64D) <- labels

# position: 197
bd$P66 <- as.factor(bd$P66)
labels <- c("Sí", "No" , "NS/NR")
levels(bd$P66) <- labels
# position: 207
bd$P71 <- as.factor(bd$P71)
labels <- c("En línea", "Presencial" )
levels(bd$P71) <- labels


# position: 208
bd$P72A <- as.factor(bd$P72A)
# position: 209
bd$P72B <- as.factor(bd$P72B)
# position: 210
bd$P72C <- as.factor(bd$P72C)
# position: 211
bd$P72E <- as.factor(bd$P72E)
# position: 212
bd$P72F <- as.factor(bd$P72F)
labels <- c("Es más cómodo", "Permite profundizar en la materia", "Se acomoda a mis horarios", "Tiene mayor validez", "Me ahorra tiempo", "Se aprende mejor", "Otro", "NS/NR" )
levels(bd$P72A) <- labels
levels(bd$P72B) <- labels
levels(bd$P72C) <- labels
levels(bd$P72E) <- labels
levels(bd$P72F) <- labels

# position: 214
bd$P73 <- as.factor(bd$P73)
labels <- c("Si", "No" )
levels(bd$P73) <- labels
# position: 215
bd$P74A <- as.factor(bd$P74A)
labels <- c("Mucho", "Más o menos", "Muy poco", "No usa", "Otro" )
levels(bd$P74A) <- labels
# position: 217
bd$P75 <- as.factor(bd$P75)
labels <- c("Si", "No" )
levels(bd$P75) <- labels
# position: 218
bd$P76 <- as.factor(bd$P76)
labels <- c("Si", "No" )
levels(bd$P76) <- labels
# position: 219
bd$P77 <- as.factor(bd$P77)
labels <- c("Si", "No" )
levels(bd$P77) <- labels

# position: 220
bd$P78A <- as.factor(bd$P78A)
# position: 221
bd$P78B <- as.factor(bd$P78B)
# position: 222
bd$P78C <- as.factor(bd$P78C)
labels <- c("No poseo una tarjeta de débito o crédito", "No confío en las ofertas en Internet", "El costo del transporte y/o impuestos son altos", "No existen suficientes ofertas en/para Bolivia", "Pago elevado de comisiones", "Otro", "NS/NR" )
levels(bd$P78A) <- labels
levels(bd$P78B) <- labels
levels(bd$P78C) <- labels


# position: 224
bd$P79 <- as.factor(bd$P79)
labels <- c("Nacionales", "Internacionales", "Ambas" )
levels(bd$P79) <- labels

# position: 225
bd$P80A <- as.factor(bd$P80A)
# position: 226
bd$P80B <- as.factor(bd$P80B)
# position: 227
bd$P80C <- as.factor(bd$P80C)
labels <- c("Facebook", "Whatsapp", "Sitios web", "Plataforma de compra/venta (Amazon/ebay)", "Otro", "NS/NR" )
levels(bd$P80A) <- labels
levels(bd$P80B) <- labels
levels(bd$P80C) <- labels


# position: 229
bd$P81A <- as.factor(bd$P81A)
# position: 230
bd$P81B <- as.factor(bd$P81B)
# position: 231
bd$P81C <- as.factor(bd$P81C)
labels <- c("Tarjeta de crédito propia", "Tarjeta de crédito de amigo/familiar", "Tarjeta de débito", "Paypal", "Billetera electrónica", "Dep^sito/giro", "Pago en efectivo", "Otro" )
levels(bd$P81A) <- labels
levels(bd$P81B) <- labels
levels(bd$P81C) <- labels

# position: 233
bd$P82A <- as.factor(bd$P82A)
# position: 234
bd$P82B <- as.factor(bd$P82B)
# position: 235
bd$P82C <- as.factor(bd$P82C)
labels <- c("Equipos electrónicos", "Libros físicos/electrónicos", "Viajes (pasajes, hoteles)", "Entradas a eventos", "Cursos", "Ropa y accesorios", "Artículos para el hogar", "Artículos para el cuidado personal", "Software", "Juegos", "Música o películas", "Servicios de streaming (netflix, spotify)", "Pago de anuncios publicitarios", "Servicios de hosting y dominio", "Otro" )
levels(bd$P82A) <- labels
levels(bd$P82B) <- labels
levels(bd$P82C) <- labels

# position: 237
bd$P83A <- as.factor(bd$P83A)
# position: 238
bd$P83B <- as.factor(bd$P83B)
# position: 239
bd$P83C <- as.factor(bd$P83C)
labels <- c("Casa/oficina", "Punto de encuentro", "Establecimiento/tienda", "Oficina de correos", "Otro" )
levels(bd$P83A) <- labels
levels(bd$P83B) <- labels
levels(bd$P83C) <- labels

# position: 242
bd$P85A <- as.factor(bd$P85A)
# position: 243
bd$P85B <- as.factor(bd$P85B)
# position: 244
bd$P85C <- as.factor(bd$P85C)
labels <- c("Falta de tiempo", "No existe el producto localmente", "Mejor precio", "Guardar anonimato", "Prefiero hacer mis compras en línea", "Comodidad", "Otro" )
levels(bd$P85A) <- labels
levels(bd$P85B) <- labels
levels(bd$P85C) <- labels

# position: 246
bd$P86A <- as.factor(bd$P86A)
labels <- c("Si", "No" )
levels(bd$P86A) <- labels
# position: 247
bd$P87 <- as.factor(bd$P87)
labels <- c("Local", "Internacional", "Ambas" )
levels(bd$P87) <- labels

# position: 248
bd$P88A <- as.factor(bd$P88A)
# position: 249
bd$P88B <- as.factor(bd$P88B)
# position: 250
bd$P88C <- as.factor(bd$P88C)
labels <- c("Facebook", "Whatsapp", "Sitios web", "Plataforma de compra/venta (ebay)", "Otro" )
levels(bd$P88A) <- labels
levels(bd$P88B) <- labels
levels(bd$P88C) <- labels

# position: 252
bd$P89A <- as.factor(bd$P89A)
# position: 253
bd$P89B <- as.factor(bd$P89B)
# position: 254
bd$P89C <- as.factor(bd$P89C)
labels <- c("Tarjeta de crédito", "Tarjeta de débito", "Paypal", "Billetera electrónica", "Pago en efectivo", "Depósito, giro", "Otro" )
levels(bd$P89A) <- labels
levels(bd$P89B) <- labels
levels(bd$P89C) <- labels


# position: 256
bd$P90A <- as.factor(bd$P90A)
# position: 257
bd$P90B <- as.factor(bd$P90B)
# position: 258
bd$P90C <- as.factor(bd$P90C)
labels <- c("Equipos electrónicos", "Libros", "Viajes (pasajes, hoteles)", "Cursos", "Ropa y accesorios", "Artículos para el hogar", "Artículos para el cuidado personal", "Software", "Juegos", "M^sica/películas", "Servicios de hosting y dominio", "Otro" )
levels(bd$P90A) <- labels
levels(bd$P90B) <- labels
levels(bd$P90C) <- labels

# position: 260
bd$P91A <- as.factor(bd$P91A)
# positP91Aion: 261
bd$P91B <- as.factor(bd$P91B)
# position: 262
bd$P91C <- as.factor(bd$P91C)
labels <- c("En un punto de encuentro", "En una tienda perteneciente al negocio", "Env^a por correo o courrier", "Entrega a domicilio", "Otro" )
levels(bd$P91A) <- labels
levels(bd$P91B) <- labels
levels(bd$P91C) <- labels

# position: 265
bd$P93A <- as.factor(bd$P93A)
# position: 266
bd$P93B <- as.factor(bd$P93B)
# position: 267
bd$P93C <- as.factor(bd$P93C)
labels <- c("Mayor alcance de compradores", "Facilidad de administración", "Mejores precios", "Rapidez", "Anonimato", "Otro" )
levels(bd$P93A) <- labels
levels(bd$P93B) <- labels
levels(bd$P93C) <- labels

# position: 269
bd$P94 <- as.factor(bd$P94)
labels <- c("Si", "No" )
levels(bd$P94) <- labels
# position: 270
bd$P95 <- as.factor(bd$P95)
labels <- c("Si", "No" )
levels(bd$P95) <- labels
# position: 271
bd$P96 <- as.factor(bd$P96)
labels <- c("Si", "No" )
levels(bd$P96) <- labels
# position: 272
bd$P97 <- as.factor(bd$P97)
labels <- c("Si", "No" )
levels(bd$P97) <- labels
# position: 273
bd$P98A <- as.factor(bd$P98A)
# position: 274
bd$P98B <- as.factor(bd$P98B)
# position: 275
bd$P98C <- as.factor(bd$P98C)
# position: 276
bd$P98E <- as.factor(bd$P98E)
labels <- c("Servicios de telefonía o Internet", "Luz, tv cable, agua", "Créditos bancarios", "Pasajes aéreos", "Entradas a espectáculos", "Estudios, pensión escolar", "Impuestos", "Otros" )
levels(bd$P98A) <- labels
levels(bd$P98B) <- labels
levels(bd$P98C) <- labels
levels(bd$P98E) <- labels

# position: 278
bd$P99 <- as.factor(bd$P99)
labels <- c("Si", "No" )
levels(bd$P99) <- labels

# position: 279
bd$P100A <- as.factor(bd$P100A)
# position: 280
bd$P100B <- as.factor(bd$P100B)
# position: 281
bd$P100C <- as.factor(bd$P100C)
# position: 282
bd$P100D <- as.factor(bd$P100D)
# position: 283
bd$P100G <- as.factor(bd$P100G)
# position: 284
bd$P100H <- as.factor(bd$P100H)
# position: 285
bd$P100I <- as.factor(bd$P100I)
# position: 286
bd$P100E <- as.factor(bd$P100E)
labels <- c("Facebook", "Twitter", "Whatsapp", "Telegram", "Instagram", "Youtube", "Skype", "Snapchat", "Line", "Otra" )
levels(bd$P100A) <- labels
levels(bd$P100B) <- labels
levels(bd$P100C) <- labels
levels(bd$P100D) <- labels
levels(bd$P100E) <- labels
levels(bd$P100G) <- labels
levels(bd$P100H) <- labels
levels(bd$P100I) <- labels

# position: 288
bd$P101A <- as.factor(bd$P101A)
# position: 289
bd$P101B <- as.factor(bd$P101B)
# position: 290
bd$P101C <- as.factor(bd$P101C)
# position: 291
bd$P101D <- as.factor(bd$P101D)
labels <- c("Conectarse con amigos/familiares", "Conocer gente", "Informarse sobre temas políticos", "Informarse de otras noticias", "Compartir contenidos (noticias, memes, links, mensajes)", "Participar en debates", "Ver videos/fotos", "Hacer negocios", "Apoyar alguna causa o demanda social", "Comprar o vender productos", "Seguir cuentas o paginas", "Otro" )
levels(bd$P101A) <- labels
levels(bd$P101B) <- labels
levels(bd$P101C) <- labels
levels(bd$P101D) <- labels

# position: 326
bd$P105 <- as.factor(bd$P105)
labels <- c("Si", "No" )
levels(bd$P105) <- labels

# position: 327
bd$P106A <- as.factor(bd$P106A)
# position: 328
bd$P106B <- as.factor(bd$P106B)
# position: 329
bd$P106C <- as.factor(bd$P106C)
labels <- c("Ecologista", "Derechos humanos", "Derechos de los animales", "Derechos de las mujeres", "Campaña electoral", "Otro" )
levels(bd$P106A) <- labels
levels(bd$P106B) <- labels
levels(bd$P106C) <- labels

# position: 331
bd$P107A <- as.factor(bd$P107A)
# position: 332
bd$P107B <- as.factor(bd$P107B)
# position: 333
bd$P107C <- as.factor(bd$P107C)
labels <- c("Amigos", "Familia", "Compañeros de trabajo/estudio", "Público en general", "Periodistas", "Organizaciones políticas", "Gobierno", "No influyen", "Otro" )
levels(bd$P107A) <- labels
levels(bd$P107B) <- labels
levels(bd$P107C) <- labels

# position: 336
bd$P108 <- as.factor(bd$P108)
labels <- c("Mucho", "Más o menos", "Poco", "No confía", "NS/NR" )
levels(bd$P108) <- labels
# position: 337
bd$P109 <- as.factor(bd$P109)
labels <- c("Si", "No" )
levels(bd$P109) <- labels
# position: 338
bd$P110 <- as.factor(bd$P110)
labels <- c("Si", "No" )
levels(bd$P110) <- labels
# position: 339
bd$P111 <- as.factor(bd$P111)
labels <- c("Mucho", "Más o menos", "Poco", "No influyó", "Otro" )
levels(bd$P111) <- labels
# position: 341
bd$P112 <- as.factor(bd$P112)
labels <- c("Sí", "No", "NS/NR" )
levels(bd$P112) <- labels
# position: 342
bd$P113 <- as.factor(bd$P113)
labels <- c("Sí", "No" )
levels(bd$P113) <- labels

# position: 345
bd$P115A <- as.factor(bd$P115A)
# position: 346
bd$P115B <- as.factor(bd$P115B)
# position: 347
bd$P115C <- as.factor(bd$P115C)
labels <- c("Amistad", "Relación amorosa", "Laboral", "Política", "Otro" )
levels(bd$P115A) <- labels
levels(bd$P115B) <- labels
levels(bd$P115C) <- labels

# position: 349
bd$P116 <- as.factor(bd$P116)
labels <- c("Si", "No" )
levels(bd$P116) <- labels
# position: 350
bd$P117 <- as.factor(bd$P117)
labels <- c("Si", "No" )
levels(bd$P117) <- labels

# position: 351
bd$P118A <- as.factor(bd$P118A)
# position: 352
bd$P118B <- as.factor(bd$P118B)
# position: 353
bd$P118C <- as.factor(bd$P118C)
# position: 354
bd$P118D <- as.factor(bd$P118D)
# position: 355
bd$P118E <- as.factor(bd$P118E)
# position: 356
bd$P118G <- as.factor(bd$P118G)
# position: 358
bd$P118H <- as.factor(bd$P118H)
# position: 356
bd$P118I <- as.factor(bd$P118I)
labels <- c("Videojuegos", "Animé, manga o comics", "Software libre", "Tecnología", "Actividades políticas", "Cultura", "Actividad profesional o de estudio", "Deportes", "Educación", "Ecología/ medio ambiente", "Música", "Otros" )
levels(bd$P118A) <- labels
levels(bd$P118B) <- labels
levels(bd$P118C) <- labels
levels(bd$P118D) <- labels
levels(bd$P118E) <- labels
levels(bd$P118G) <- labels
levels(bd$P118H) <- labels
levels(bd$P118I) <- labels

# position: 360
# bd$P119 <- as.factor(bd$P119)
# labels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, "Menos de 1 vez" )
# levels(bd$P119) <- labels

# position: 361
bd$P120 <- as.factor(bd$P120)
labels <- c("Si", "No" )
levels(bd$P120) <- labels

# position: 362
bd$P121A <- as.factor(bd$P121A)
# position: 363
bd$P121B <- as.factor(bd$P121B)
# position: 364
bd$P121C <- as.factor(bd$P121C)
# position: 365
bd$P121E <- as.factor(bd$P121E)
# position: 366
bd$P121F <- as.factor(bd$P121F)
# position: 367
bd$P121G <- as.factor(bd$P121G)
labels <- c("Correo electrónico", "Facebook", "Whatsapp", "Telegram", "Twitter", "Sitios web", "Otro" )
levels(bd$P121A) <- labels
levels(bd$P121B) <- labels
levels(bd$P121C) <- labels
levels(bd$P121E) <- labels
levels(bd$P121F) <- labels
levels(bd$P121G) <- labels

# position: 369
bd$P122 <- as.factor(bd$P122)
labels <- c("Si", "No" )
levels(bd$P122) <- labels


##################################3

# position: 371
bd$P124 <- as.factor(bd$P124)
labels <- c("No realiza actividad", "NS/NR" )
levels(bd$P124) <- labels
# position: 372
bd$P124 <- as.factor(bd$P124)
labels <- c("No realiza actividad", "NS/NR" )
levels(bd$P124) <- labels
# position: 373
bd$P124 <- as.factor(bd$P124)
labels <- c("No realiza actividad", "NS/NR" )
levels(bd$P124) <- labels
# position: 374
bd$P124 <- as.factor(bd$P124)
labels <- c("No realiza actividad", "NS/NR" )
levels(bd$P124) <- labels
# position: 375
bd$P124 <- as.factor(bd$P124)
labels <- c("No realiza actividad", "NS/NR" )
levels(bd$P124) <- labels


# position: 376
bd$P125 <- as.factor(bd$P125)
labels <- c("Si", "No" )
levels(bd$P125) <- labels
# position: 377
bd$P126 <- as.factor(bd$P126)
labels <- c("Mismo día", "Ns/Nr" )
levels(bd$P126) <- labels
# position: 378
bd$P127 <- as.factor(bd$P127)
labels <- c("Si", "No" )
levels(bd$P127) <- labels


# position: 379
bd$P128A <- as.factor(bd$P128A)
# position: 380
bd$P128B <- as.factor(bd$P128B)
# position: 381
bd$P128C <- as.factor(bd$P128C)
labels <- c("Promocionar su actividad económica", "Compartir información", "Compartir música/videos", "Publicar artículos o actividades propias", "Otro" )
levels(bd$P128A) <- labels
levels(bd$P128B) <- labels
levels(bd$P128C) <- labels

# position: 383
bd$P129A <- as.factor(bd$P129A)
# position: 384
bd$P129B <- as.factor(bd$P129B)
# position: 385
bd$P129C <- as.factor(bd$P129C)
labels <- c("Tv", "Radio", "Periódicos impresos", "Periódicos digitales", "Páginas web", "Redes sociales", "Charlas con amigos", "Otro", "NS/NR" )
levels(bd$P129A) <- labels
levels(bd$P129B) <- labels
levels(bd$P129C) <- labels

# position: 386
bd$P130A <- as.factor(bd$P130A)
# position: 387
bd$P130B <- as.factor(bd$P130B)
# position: 388
bd$P130C <- as.factor(bd$P130C)
labels <- c("Tv", "Radio", "Periódicos impresos", "Periódicos digitales", "Páginas web", "Redes sociales", "Charlas con amigos", "Otro", "NS/NR" )
levels(bd$P130A) <- labels
levels(bd$P130B) <- labels
levels(bd$P130C) <- labels

# position: 389
bd$P131A <- as.factor(bd$P131A)
# position: 390
bd$P131B <- as.factor(bd$P131B)
# position: 391
bd$P131C <- as.factor(bd$P131C)
labels <- c("Tv", "Radio", "Periódicos impresos", "Periódicos digitales", "Páginas web", "Redes sociales", "Charlas con amigos", "Otro", "NS/NR" )
levels(bd$P131A) <- labels
levels(bd$P131B) <- labels
levels(bd$P131C) <- labels


# position: 392
bd$P132A <- as.factor(bd$P132A)
# position: 393
bd$P132B <- as.factor(bd$P132B)
# position: 394
bd$P132C <- as.factor(bd$P132C)
labels <- c("Tv", "Radio", "Periódicos impresos", "Periódicos digitales", "Páginas web", "Redes sociales", "Charlas con amigos", "Otro", "NS/NR" )
levels(bd$P132A) <- labels
levels(bd$P132B) <- labels
levels(bd$P132C) <- labels

# position: 395
bd$P133A <- as.factor(bd$P133A)
labels <- c("Tv", "Radio", "Periódicos impresos", "Periódicos digitales", "Páginas web", "Redes sociales", "Charlas con amigos", "Otro", "NS/NR" )
levels(bd$P133A) <- labels







############## Análisis de valores faltantes 

# Separando las bases de datos
Internautas = bd[bd$Tipo == 1]
NoInter = bd[bd$Tipo == 2]


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

#Considerando aquellas con solamente 0.05 de NaN
var_consideradas <- missing_data[missing_data$Percent<=0.00,1]
var_consideradas

# Nueva Filtrada Base de Datos para Internautas 
Inter_f <- Internautas[,var_consideradas]
missmap(obj = Inter_f, y.labels = NULL,
        y.at=NULL, x.cex = 0.4,
        main = "Mapa de valores faltantes")



