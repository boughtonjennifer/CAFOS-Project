library(sf) 
library(sp)
library(geosphere)
library(rgdal)
library(rgeos)
library(readr)
library(tidyverse)

#functions
SPATIALIZEcafos <- function(df, crs) { 
  df.post <- SpatialPointsDataFrame(cbind(df$lon, df$lat), as.data.frame(cbind(df$cafo_state, df$cafo_fid)), proj4string=CRS("+proj=longlat")) %>%
    st_as_sf(.) %>%
    st_transform(., crs)
  colnames(df.post) <- c("statename", "cafo_fid", "geometry")
  df.post
}

SPATIALIZEcensus <- function(df, crs) {   
  df.post <- df %>% 
    st_transform(., crs)
}


#set wd
setwd("~/University of Chicago/Fall 2018/EPIC/CAFOs")

#read in census track shape file
shape <- st_read("gz_2010_19_140_00_500k.shp")
shape <- tibble::rowid_to_column(shape, "row.id")

#read in CAFOS csv
cafos <- read.csv("IA_Merge_CAFOs_Coords.csv", stringsAsFactors = FALSE)
cafos2 <- na.omit(cafos)
cafos2$ID <- parse_number(cafos2$cafo_fid)



#spatialize census track shapefile and CAFOs to correct grid
cafos_spatialized <- SPATIALIZEcafos(cafos2, 102003)
cents <- SPATIALIZEcensus(shape, 102003)

#find out how many CAFOS are within each census track TOTAL 

mat.df <- st_intersects(cents, cafos_spatialized, sparse = TRUE)

#create data frame

cafos_by_census <- as.data.frame(mat.df)

#match row.id from cafos_by_census to row.id from cents
