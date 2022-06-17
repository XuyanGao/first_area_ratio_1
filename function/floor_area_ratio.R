library(sf)
library(rgdal)
library(dplyr)
library(readr)
library(raster)
library(fasterize)
library(rstudioapi)
library(rnaturalearth)

# Set working directory to path of the script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Download country boundaries from Natural Earth
countries <- ne_download(scale = 10, returnclass = 'sf') %>%
  dplyr::select(ADM0_A3, ADMIN, UN_A3)

# Load population density in 2015 from GHSL

pop_density<-raster(file.path('..','data',
  'GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif'))

# Load GDPC in each countries
GDPC <- readr::read_csv(file.path('..','data', 'GDPC_2015.csv'), show_col_types = F)
# Load residential and commercial coefficient
Coefficient <- readr::read_csv(file.path('..','data', 'R_C_coefficient.csv'), show_col_types = F)
# List of countries available in urban land projections
lst_countries <- unique(GDPC$REGION)
lst_countries <- c('CHN')

# Loop through countries
for (iso in lst_countries) {
  # Print country name
  print(paste('Processing', iso))
  
  # Select and re-project country polygons
  country <- countries %>% 
    dplyr::filter(ADM0_A3==iso) %>%
    sf::st_transform(
      crs='+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
  
  # Output folder
  path_ctry <- file.path('..', 'results', iso)
  
  if(nrow(country)>0 & !dir.exists(path_ctry)) {
    
    # Create mask for the country
    mask_ctry <- country %>%
      fasterize::fasterize(pop_density) %>%
      raster::crop(country)
    # Crop the global population density layer to the country
    pop_ctry_density <- pop_density %>%
      raster::crop(mask_ctry) %>%
      raster::mask(mask_ctry)
    
    a<- 'CHN'
    r_GDPC=function(b){
      r=grep(b, GDPC$REGION)
      result=GDPC[r,2]
      return(result)
    }
    c<-r_GDPC(a)
    #r_UPD<-149.7054472

    R_FAC=function(a){
      r=grep(a, Coefficient$Code)
      Coefficient_1=Coefficient[r,2]
      #Coefficient_2=data[r,6]
      #r_UPD=data[r,3]
      #r_GDPC=data[r,4]
      #R_result=Coefficient_1-3.42*log10(r_UPD)+0.00074*r_GDPC
      R_result=Coefficient_1-3.42*log10(r_UPD)+0.00074*c
      return(R_result)
    }
    
    C_FAC=function(a){
      r=grep(a, Coefficient$Code)
      #Coefficient_1=data[r,5]
      Coefficient_2=Coefficient[r,3]
      #r_UPD=data[r,3]
      #r_GDPC=data[r,4]
      C_result=Coefficient_2-1.59*log10(r_UPD)+0.00011*c
      return(C_result)
    }

    R_FAC(a)
    C_FAC(a)
    paste(a,"Residential_FAC",R_FAC(a))
    paste(a,"Commercial_FAC",C_FAC(a))
  
  }
}


