
library(rtry)
library(dplyr)
library(maps)

file_name <- 'C:\\Dados\\SinBiAM\\data\\arvores amazonicas FB2020-Cardoso-Arieira.csv'
sp_amazon <- readr::read_csv(file_name, 
                       locale = readr::locale(encoding = "UTF-8"),
                       show_col_types = FALSE)

NROW(sp_amazon)

sp_amazon <- sp_amazon %>% dplyr::filter(wcvp_taxon_status=='Accepted')

NROW(sp_amazon)


library(dplyr)


name_file <- 'C:\\Dados\\try\\36338.txt'
try_raw <- readr::read_csv(name_file, 
                           locale = readr::locale(encoding = "UTF-8"),
                           show_col_types = FALSE)

try_raw <- readr::read_delim(name_file, 
                             delim = '\t',
                             locale = readr::locale(encoding = "UTF-8"),
                             show_col_types = FALSE)

remove(try_raw)

try_amazon <- try_raw %>% 
  dplyr::filter(SpeciesName %in% sp_amazon$Binomio)

file.name.res <- 'C:\\Dados\\try\\try_amazon_raw.csv'
write.csv(try_amazon, 
          file.name.res, 
          row.names = FALSE, 
          fileEncoding = "UTF-8", 
          na = "")


