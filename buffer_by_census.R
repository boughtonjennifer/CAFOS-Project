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


CREATEbuffers <- function(df, distance) { 
  # distance: in miles
  st_buffer(df, units::set_units(distance, miles))
}   

#setwd
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

#create buffers
buffers_0.01 <- CREATEbuffers(cents, 0.01)
buffers_0.05 <- CREATEbuffers(cents, 0.05)
buffers_0.1 <- CREATEbuffers(cents, 0.1)
buffers_0.5 <- CREATEbuffers(cents, 0.5)
buffers_1 <- CREATEbuffers(cents, 1)

#find intersections
mat.df = st_intersects(buffers_0.01, cafos_spatialized, sparse = TRUE)
mat.df2 = st_intersects(buffers_0.05, cafos_spatialized, sparse = TRUE)
mat.df3 = st_intersects(buffers_0.1, cafos_spatialized, sparse = TRUE)
mat.df4 = st_intersects(buffers_0.5, cafos_spatialized, sparse = TRUE)
mat.df5 = st_intersects(buffers_1, cafos_spatialized, sparse = TRUE)

#convert to dataframes 

within_0.01 <- as.data.frame(mat.df)
within_0.05 <- as.data.frame(mat.df2)
within_0.1 <- as.data.frame(mat.df3)
within_0.5 <- as.data.frame(mat.df4)
within_1 <- as.data.frame(mat.df5)

#match row.id from within_dfs to row.id from cents
