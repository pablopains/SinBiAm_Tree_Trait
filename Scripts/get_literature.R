require(dplyr)

data <- data.frame(
  traitID	= '',
  unitOfMeasure	= '',
  methodsID	= '',
  value	= '',
  family	= '',
  species	= '',
  sourceID	= '',
  initialYearSampling	= '',
  initialMonthSampling	= '',
  initialDaySampling	= '',
  finalYearSampling	= '',
  finalMonthSampling	= '',
  finalDaySampling	= '',
  seasons	= '',
  weatherAnomaly	= '',
  lifeStage	= '',
  country	= '',
  stateProvince = '',	
  municipality = '',	
  localidade	= '',
  decimalLatitude	= '',
  decimalLongitude	= '',
  altitude	= '',
  typeVegatation = '',
  stringsAsFactors = FALSE)[-1,] 

# Tavares_et_al_2023
{
  name_file <- 'C:\\Dados\\SinBiAM\\Artigos\\Tavares et all 2023 - Data_package_Tavares_et_al_2023\\Data_package_Tavares_et_al_2023\\Data\\Hydraulic_traits_dataset_TAVARES_et_al_2023.csv'
  spp <- readr::read_csv(name_file, 
                         locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  
  loc <- spp$Cluster %>% unique()
  
  col <- colnames(spp)
  
  col_ini <- 7
  
  sourceID <- 'Tavares_et_al_2023'
  
  i=1
  for(i in 1:NROW(loc))
  {
    
    x <- spp %>% dplyr::filter(Cluster == loc[i])
    
    ii = 7
    for ( ii in col_ini:NROW(col))
    {
      value <- x[,col[ii]] %>% as.vector() %>% data.frame()
      
      colnames(value) <- 'value'
      
      
      data <- rbind(data, 
                    data.frame(traitID	= col[ii],
                               unitOfMeasure	= '',
                               methodsID	= rep('',NROW(x)),
                               
                               value	= value[,1],
                               
                               family	= x$Family,
                               species	= x$Species,
                               sourceID	= rep(sourceID, NROW(x)),
                               initialYearSampling	= rep('',NROW(x)),
                               initialMonthSampling	= rep('',NROW(x)),
                               initialDaySampling	= rep('',NROW(x)),
                               finalYearSampling	= rep('',NROW(x)),
                               finalMonthSampling	= rep('',NROW(x)),
                               finalDaySampling	= rep('',NROW(x)),
                               seasons	= rep('',NROW(x)),
                               weatherAnomaly	= rep('',NROW(x)),
                               lifeStage	= rep('adult',NROW(x)),
                               country	= rep('',NROW(x)),
                               stateProvince = rep('',NROW(x)),
                               municipality = rep('',NROW(x)),
                               localidade	= rep(loc[i],NROW(x)),
                               decimalLatitude	= rep('',NROW(x)),
                               decimalLongitude	= rep('',NROW(x)),
                               altitude	= rep('',NROW(x)),
                               typeVegatation = x$Forest,
                               stringsAsFactors = FALSE
                    ))
      
      
    } 
    
    
  }
  View(data)
}


# Brum_et_al_2018_Average_Hydraulic_Traits
{
  name_file <- 'C:\\Dados\\SinBiAM\\Artigos\\Brum_et_al_2018_Average_Hydraulic_Traits.csv'
  spp <- readr::read_csv(name_file, 
                         locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  
  loc <- spp$site %>% unique()
  
  col <- colnames(spp)
  
  col_ini <- 4
  
  sourceID <- 'Brum_et_al_2018'
  
  i=1
  for(i in 1:NROW(loc))
  {
    
    x <- spp %>% dplyr::filter(site == loc[i])
    
    ii = 4
    for ( ii in col_ini:NROW(col))
    {
      value <- x[,col[ii]] %>% as.vector() %>% data.frame()
      
      colnames(value) <- 'value'
      
      
      data <- rbind(data, 
                    data.frame(traitID	= col[ii],
                               unitOfMeasure	= '',
                               methodsID	= rep('',NROW(x)),
                               
                               value	= value[,1],
                               
                               family	= x$family,
                               species	= x$species,
                               sourceID	= rep(sourceID, NROW(x)),
                               initialYearSampling	= rep('',NROW(x)),
                               initialMonthSampling	= rep('',NROW(x)),
                               initialDaySampling	= rep('',NROW(x)),
                               finalYearSampling	= rep('',NROW(x)),
                               finalMonthSampling	= rep('',NROW(x)),
                               finalDaySampling	= rep('',NROW(x)),
                               seasons	= rep('',NROW(x)),
                               weatherAnomaly	= rep('',NROW(x)),
                               lifeStage	= rep('adult',NROW(x)),
                               country	= rep('',NROW(x)),
                               stateProvince = rep('',NROW(x)),
                               municipality = rep('',NROW(x)),
                               localidade	= rep(loc[i],NROW(x)),
                               decimalLatitude	= rep('',NROW(x)),
                               decimalLongitude	= rep('',NROW(x)),
                               altitude	= rep('',NROW(x)),
                               typeVegatation = rep('',NROW(x)),
                               stringsAsFactors = FALSE
                    ))
      
      
    } 
    
    
  }
  View(data)
}


# Garcia_et_al_2023
{
  name_file <- 'C:\\Dados\\SinBiAM\\Artigos\\Garcia et all 2023 - mg.6.2.csv'
  spp <- readr::read_csv(name_file, 
                         locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  
  loc <- spp$site.name %>% unique()
  
  col <- colnames(spp)
  
  col_ini <- 6
  
  sourceID <- 'Garcia_et_al_2023'
  
  i=1
  for(i in 1:NROW(loc))
  {
    
    x <- spp %>% dplyr::filter(site.name == loc[i])
    
    ii = 6
    for ( ii in col_ini:NROW(col))
    {
      value <- x[,col[ii]] %>% as.vector() %>% data.frame()
      
      colnames(value) <- 'value'

      data <- rbind(data, 
                    data.frame(traitID	= col[ii],
                               unitOfMeasure	= '',
                               methodsID	= rep('',NROW(x)),
                               
                               value	= value[,1],
                               
                               family	= x$family,
                               species	= x$species.name,
                               
                               sourceID	= rep(sourceID, NROW(x)),
                               initialYearSampling	= rep('',NROW(x)),
                               initialMonthSampling	= rep('',NROW(x)),
                               initialDaySampling	= rep('',NROW(x)),
                               finalYearSampling	= rep('',NROW(x)),
                               finalMonthSampling	= rep('',NROW(x)),
                               finalDaySampling	= rep('',NROW(x)),
                               seasons	= rep('',NROW(x)),
                               weatherAnomaly	= rep('',NROW(x)),
                               lifeStage	= rep('adult',NROW(x)),
                               country	= rep('',NROW(x)),
                               stateProvince = rep('',NROW(x)),
                               municipality = rep('',NROW(x)),
                               localidade	= rep(loc[i],NROW(x)),
                               decimalLatitude	= x$Latitude,
                               decimalLongitude	= x$Longtitude,
                               altitude	= rep('',NROW(x)),
                               typeVegatation = rep('',NROW(x)),
                               stringsAsFactors = FALSE
                    ))
      
      
    } 
    
    
  }
  View(data)
}

file.name.res <- 'C:\\Dados\\SinBiAM\\data\\data_traits.csv'
write.csv(data, file.name.res, row.names = FALSE, fileEncoding = "UTF-8", na = "")
