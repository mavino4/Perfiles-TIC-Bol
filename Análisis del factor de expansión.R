library(readr)
require(data.table)

bd <- read_delim("bases de datos/base-5536-bdfinalcorregido.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)
bd <- as.data.table(bd)
bd$Ponderador5033[5034:5536] <- 0
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

colSums(bd[,449:457])


#expandidoTotal <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5536_3),]
#expandidoInternautas <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5033_3),]

bd$Ponderador5033_3
expandidoTotal <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5536_4),]
expandidoInternautas <- bd[rep(seq_len(nrow(bd)), bd$Ponderador5033_4),]
