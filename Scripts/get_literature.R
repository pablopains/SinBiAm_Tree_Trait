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
  notes = '',
  stringsAsFactors = FALSE)[-1,] 

path_data <- 'C:\\SinBiAm_Tree_Trait - github.com\\SinBiAm_Tree_Trait\\Data\\bibliografia'

# Araujo et all 2024
{
  # email
}

# Barros et all 2015
{
  # igual à Barros et all 2019
}


# Barros et all 2019
{
  name_file <- paste0(path_data, '\\Barros et all 2019.csv')
  spp <- readr::read_csv(name_file, 
                         locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  
  loc <- spp$Site %>% unique()
  
  col <- colnames(spp)
  
  col_ini <- 17
  
  sourceID <- 'Barros et all 2019'
  
  i=1
  for(i in 1:NROW(loc))
  {
    
    x <- spp %>% dplyr::filter(Site == loc[i])
    
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
                               initialYearSampling	= ifelse(col[ii]=='ΨnonENSO', x$ano_nonenso, x$ano_enso),
                               initialMonthSampling	= ifelse(col[ii]== "ΨnonENSO", x$mes_nonenso, x$mes_enso),
                               initialDaySampling	= rep('',NROW(x)),
                               finalYearSampling	= rep('',NROW(x)),
                               finalMonthSampling	= rep('',NROW(x)),
                               finalDaySampling	= rep('',NROW(x)),
                               seasons	= x$season,
                               weatherAnomaly	= rep('',NROW(x)),
                               lifeStage	= rep('adult',NROW(x)),
                               country	= x$Country,
                               stateProvince = x$State_Province,
                               municipality = x$County,
                               localidade	= x$Localidade,
                               decimalLatitude	= x$decimalLatitude,
                               decimalLongitude	= x$decimalLongitude,
                               altitude	= rep('',NROW(x)),
                               typeVegatation = x$typeVegatation,
                               notes = paste0('mean, n:',x$n),
                               stringsAsFactors = FALSE
                    ))
      
      
    } 
    
    
  }
  
  View(data)
}


# Bittencourt et all 2020
{
  # csv
}

# ok - Brum et all 2018
{
  
  name_file <- paste0(path_data, '\\Brum et all 2018.csv')
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


# Brum et all 2023
{
  # in Brum et all 2018 (2019)
}


# Garcia et all 2021
{
  # Flávia
}


# Garcia et all 2023
{
  name_file <- paste0(path_data, '\\Garcia et all 2023.csv')
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

# Mattos el all 2023
{
  # Barros et all 2019 & Oliveira et all 2019
}

# Oliveira et all 2019
{
  # tabela media csv
}

# Powell et all 2017
{
  # email
}

# Rowland et all 2015
{
  # email
}

# Tavares et all 2023
{
  name_file <- paste0(path_data, '\\Tavares et all 2023.csv')
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

Ziegler et all 2023






# Tavares_et_al_2023





file.name.res <- paste0(path_data,'\\compilation model table.csv')
write.csv(data, file.name.res, row.names = FALSE, fileEncoding = "UTF-8", na = "")
