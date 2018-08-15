library(XML)
library(reshape2)
library(ggplot2)
library(plyr)


get_data <- function(country, year) {
  c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="  
  c2 <- "&R=-1&C="
  url <- paste0(c1, year, c2, country)
  df <- data.frame(readHTMLTable(url))
  keep <- c(2, 4, 5)
  df <- df[,keep]  
  names(df) <- c("Age", "Male", "Female")
  cols <- 2:3
  df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df <- df[df$Age != 'Total', ]  
  df$Male <- -1 * df$Male
  df$Age <- factor(df$Age, levels = df$Age, labels = df$Age)
  
  df.melt <- melt(df, 
                  value.name='Population', 
                  variable.name = 'Gender', 
                  id.vars='Age' )
  
  return(df.melt)
}

nigeria <- get_data("GM", 2014)

n1 <- ggplot(nigeria, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
  geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

n1
