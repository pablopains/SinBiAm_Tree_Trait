
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
  trait_tmp <- BIEN::BIEN_trait_species(spp$Binomio[i])  
  
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


# geoespacial e sumario
# Bien
{
  
  file.name <- 'C:\\Dados\\SinBiAM\\data\\Try e Bien\\BIEN_SinBiAM_all_spp.csv'
  spp_all <- readr::read_csv(file.name, 
                             locale = readr::locale(encoding = "UTF-8"),
                             show_col_types = FALSE)
  
  projecao <- st_crs(amazon_shp)
  
  spp_all$decimalLongitude <- spp_all$longitude %>% as.numeric()
  spp_all$decimalLatitude <- spp_all$latitude %>% as.numeric()
  
  spp_all <- spp_all %>%
    dplyr::mutate(temCoordenadas =  CoordinateCleaner::cc_val(spp_all,
                                                              lon='decimalLongitude',
                                                              lat='decimalLatitude', 
                                                              value = 'flagged'))
  # total de registros
  index <- spp_all$temCoordenadas==TRUE
  
  # sem lat/long
  spp_sem_coord <- spp_all$scrubbed_species_binomial[index==FALSE] %>% unique()
  
  traids_sem_coord <- spp_all$trait_name[index==FALSE] %>% unique()
  
  # com lat/long
  spp_com_coord <- spp_all$scrubbed_species_binomial[index==TRUE] %>% unique()
  
  traids_com_coord <- spp_all$trait_name[index==TRUE] %>% unique()
  
  # spp in amazon
  pontos <- spp_all[index==TRUE,] %>% 
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = projecao$wkt)
  
  pontos_in <- sf::st_intersection(pontos, amazon_shp)
  
  
  #
  spp_amazon <- pontos_in$scrubbed_species_binomial %>% unique()
  
  traids_amazon <- pontos_in$trait_name %>% unique()
  
  # # mapa
  # plot(pontos[1], pch = "*")
  # plot(amazon_shp$geometry, , add = TRUE)
  # plot(pontos_in, add = TRUE, pch = "+")
  
  
  #resultados
  print(paste0('Núerodo total de registros BIEN: ',NROW(spp_all)))
  print(paste0('Núerodo de registros sem lat/long: ',sum(index==FALSE)))
  print(paste0('Núerodo de espécies sem lat/long: ',NROW(spp_sem_coord)))
  print(paste0('Núerodo de traids sem lat/long: ',NROW(traids_sem_coord)))
  print(traids_sem_coord)
  print(paste0('Núerodo de registros com lat/long: ',sum(index==TRUE)))
  print(paste0('Núerodo de espécies com lat/long: ',NROW(spp_com_coord)))
  print(paste0('Núerodo de traids com lat/long: ',NROW(traids_com_coord)))
  print(traids_com_coord)
  print(paste0('Núerodo de registros na AmaZõnia: ',NROW(pontos_in)))
  print(paste0('Núerodo de espécies na AmaZõnia: ',NROW(spp_amazon)))
  print(paste0('Núerodo de traids na AmaZõnia: ',NROW(traids_amazon)))
  print(traids_amazon)
  
  ggplot() + 
    geom_sf(data = amazon_shp, size = 1.5, color = "black", fill = "cyan1") + 
    ggtitle("Limites Biogeográfios da Amazônia e Dados BIEN") + 
    geom_sf(data = pontos_in, size = 1.5, color = "black", fill = "cyan1") +
    coord_sf()
  
  # salvar no formato SinBiAM
  
  file.name <- 'C:\\Dados\\SinBiAM\\data\\Try e Bien\\BIEN_SinBiAM_in_Amazon_source.csv'
  spp_in_amazon <- readr::read_csv(file.name,
                                   locale = readr::locale(encoding = "UTF-8"),
                                   show_col_types = FALSE)
  colnames(spp_in_amazon)
  
  data <- data.frame(
    traitID	= spp_in_amazon$trait_name,
    unitOfMeasure	= spp_in_amazon$unit,
    methodsID	= spp_in_amazon$method,
    value	= spp_in_amazon$trait_value,
    family	= rep('',NROW(spp_in_amazon)),
    species	= spp_in_amazon$scrubbed_species_binomial,
    sourceID	= paste0('BIEN: ', spp_in_amazon$id, ', project_pi: ',spp_in_amazon$project_pi, ' (', spp_in_amazon$project_pi_contact, '), DOI: ', spp_in_amazon$url_source ),
    initialYearSampling	= rep('',NROW(spp_in_amazon)),
    initialMonthSampling	= rep('',NROW(spp_in_amazon)),
    initialDaySampling	= rep('',NROW(spp_in_amazon)),
    finalYearSampling	= rep('',NROW(spp_in_amazon)),
    finalMonthSampling	= rep('',NROW(spp_in_amazon)),
    finalDaySampling	= rep('',NROW(spp_in_amazon)),
    seasons	= rep('',NROW(spp_in_amazon)),
    weatherAnomaly	= rep('',NROW(spp_in_amazon)),
    lifeStage	= rep('',NROW(spp_in_amazon)),
    country	= rep('',NROW(spp_in_amazon)),
    stateProvince = rep('',NROW(spp_in_amazon)),	
    municipality = rep('',NROW(spp_in_amazon)),	
    localidade	= rep('',NROW(spp_in_amazon)),
    decimalLatitude	= spp_in_amazon$latitude,
    decimalLongitude	= spp_in_amazon$longitude,
    altitude	= spp_in_amazon$elevation_m,
    typeVegatation = rep('',NROW(spp_in_amazon)),
    stringsAsFactors = FALSE)
  
  
  file.name.res <- 'C:\\Dados\\SinBiAM\\data\\Try e Bien\\BIEN_SinBiAM_in_Amazon_final.csv'
  write.csv(data, file.name.res, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  
  
}


# analises
{
  file_name <- 'C:\\SinBiAm_Tree_Trait - github.com\\SinBiAm_Tree_Trait\\Data\\BIEN_SinBiAM_in_Amazon_final.csv'
  BEIN_Tree_Trait <- readr::read_csv(file_name, 
                                     locale = readr::locale(encoding = "UTF-8"),
                                     show_col_types = FALSE)
  
  NROW(BEIN_Tree_Trait)
  colnames(BEIN_Tree_Trait)
  
  BEIN_Tree_Trait$sourceID
  
  x <- sqldf::sqldf("SELECT DISTINCT traitID, COUNT(sourceID)
                        FROM BEIN_Tree_Trait
                        GROUP BY traitID
                        ORDER BY COUNT(sourceID)")
  file.name.res <- 'C:\\SinBiAm_Tree_Trait - github.com\\SinBiAm_Tree_Trait\\Data\\BIEN_SinBiAM_sumario_traits.csv'
  write.csv(x, 
            file.name.res, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")
  
 
  x <- sqldf::sqldf("SELECT DISTINCT species, COUNT(sourceID)
                        FROM BEIN_Tree_Trait
                        GROUP BY species
                        ORDER BY COUNT(sourceID)")
  file.name.res <- 'C:\\SinBiAm_Tree_Trait - github.com\\SinBiAm_Tree_Trait\\Data\\BIEN_SinBiAM_sumario_species.csv'
  write.csv(x, 
            file.name.res, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")
  
}