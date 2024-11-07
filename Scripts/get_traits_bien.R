
library(BIEN)
library(dplyr)
library(maps)

# obtenção traits por lista de espécie
{
  file_name <- 'C:\\Dados\\SinBiAM\\data\\arvores amazonicas FB2020-Cardoso-Arieira.csv'
spp <- readr::read_csv(file_name, 
                       locale = readr::locale(encoding = "UTF-8"),
                       show_col_types = FALSE)

NROW(spp)

spp <- spp %>% dplyr::filter(wcvp_taxon_status=='Accepted')

NROW(spp)


trait_sp <- {}
ok <- rep(FALSE,NROW(spp))
i=1
for(i in 1:NROW(spp))
{
  
  if (ok[i]==TRUE){next}
  
  print(paste0(i, ' - ',spp$Binomio[i]))
  trait_tmp <- BIEN_trait_species(spp$Binomio[i])  
  
  if(NROW(trait_tmp)>0)
  {
    print(NROW(trait_tmp))
    trait_sp <- rbind(trait_sp, trait_tmp)
  }
  ok[i] <- TRUE
}


file.name.res <- 'C:\\Dados\\SinBiAM\\data\\bien_data_traits_raw.csv'
write.csv(trait_sp, file.name.res, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

# selecao traits
{
  bien_sel_traits <- c('stem wood density',
                     'whole plant growth form',
                     'minimum whole plant height',
                     'maximum whole plant height',
                     'fruit type',
                     'whole plant height',
                     'diameter at breast height (1.3 m)',
                     'whole plant vegetative phenology',
                     'leaf thickness',
                     'vessel lumen area',
                     'vessel number',
                     'leaf area',
                     'leaf area per leaf dry mass',
                     'leaf dry mass per leaf fresh mass',
                     'leaf phosphorus content per leaf dry mass',
                     'leaf stomatal conductance for H2O per leaf area',
                     'leaf photosynthetic rate per leaf area',
                     'leaf photosynthetic rate per leaf dry mass',
                     'leaf life span',
                     'leaf stomatal conductance per leaf area',
                     'maximum whole plant longevity')

file.name <- 'C:\\Dados\\SinBiAM\\data\\bien_data_traits_raw.csv'

bien_data_traits <- readr::read_csv(file.name, 
                                          locale = readr::locale(encoding = "UTF-8"),
                                          show_col_types = FALSE)


bien_sel_data_traits <- bien_data_traits %>% dplyr::filter(trait_name %in% bien_sel_traits)


file.name.res <- 'C:\\Dados\\SinBiAM\\data\\bien_data_traits_selecao.csv'

write.csv(bien_sel_data_traits, file.name.res, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}


# selecao shape
{
  
  library(sf)
  library(sp)
  library(ggplot2)
  
  file.name <- 'C:\\Dados\\SinBiAM\\data\\bien_data_traits_selecao.csv'
  
  bien_sel_data_traits <- readr::read_csv(file.name, 
                                          locale = readr::locale(encoding = "UTF-8"),
                                          show_col_types = FALSE)
  
  colnames(bien_sel_data_traits)
  
  amazon_shp <- st_read("C:\\Dados\\SinBiAM\\shapes\\Limites2020\\Lim_Biogeografico.shp")
  
  projecao <- st_crs(amazon_shp)
  
  occ_tmp$decimalLongitude <- occ_tmp$longitude %>% as.numeric()
  occ_tmp$decimalLatitude <- occ_tmp$latitude %>% as.numeric()
  
  occ_tmp <- bien_sel_data_traits %>%
    dplyr::mutate(temCoordenadas =  CoordinateCleaner::cc_val(bien_sel_data_traits,
                                          lon='decimalLongitude',
                                          lat='decimalLatitude', 
                                          value = 'flagged'))
  
  occ_in_shape = occ_tmp %>%
    dplyr::filter(temCoordenadas==TRUE) 
  
  pontos <- occ_in_shape %>% 
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = projecao$wkt)
  
  pontos_in <- sf::st_intersection(pontos, amazon_shp)

  plot(amazon_shp$geometry)
  plot(pontos, add = TRUE, pch = ".")
  plot(pontos_in, add = TRUE, pch = "+")
  
  ggplot() + 
    geom_sf(data = amazon_shp, size = 1.5, color = "black", fill = "cyan1") + 
    ggtitle("Limites Biogeográfios da Amazônia") + 
    geom_sf(data = pontos_in, size = 1.5, color = "black", fill = "cyan1") +
    coord_sf()

  
  file.name.res <- 'C:\\Dados\\SinBiAM\\data\\BIEN_SinBiAM_in_Amazon.csv'
  write.csv(pontos_in, 
            file.name.res, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")
  
  
  
  }

