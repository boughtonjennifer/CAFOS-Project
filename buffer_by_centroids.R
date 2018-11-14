library(sf) 
library(sp)
library(geosphere)
library(rgdal)
library(rgeos)
library(readr)
library(tidyverse)
library(Matrix)

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
    st_transform(., crs) %>%
    st_centroid()
}

CREATEbuffers <- function(df, distance) { 
  # distance: in miles
  st_buffer(df, units::set_units(distance, miles))
} 

OUTPUTresults <- function(d, d_inner, centroids, cafos, statename) { 
  
  buffers_inner <- CREATEbuffers(centroids, d_inner)
  buffers_outer <- CREATEbuffers(centroids, d) 
  
  sf.pts <- st_sf(geometry = centroids)
  
  if (d_inner == 0) {
  mat.df = st_intersects(buffers_outer, cafos_spatialized, sparse = TRUE)
  } else {
  sep_rings = st_sfc(map2(buffers_outer$geometry, buffers_inner$geometry, ~ st_difference(.x, .y)), crs = 102003)
  sf_opt1 <- sf.pts %>%
    st_set_geometry(sep_rings)
  mat.df = st_intersects(sf_opt1, cafos, sparse = TRUE)
  }
  
  
  # -- convert results to data.frame (via sparse matrix)
  n.ids <- sapply(mat.df, length)
  vals <- unlist(mat.df)
  
  # -- catch when returns no values 
  if (length(vals) != 0) {
    out <- sparseMatrix(vals, rep(seq_along(n.ids), n.ids))
    out.summ <- summary(out)
    out.df <- data.frame(census_tract = cents[out.summ$j,]$TRACT, cafo_fid = cafos_spatialized[out.summ$i,]$cafo_fid)
    d.rev <- sub("[.]", "p", d)
    out.file <- paste0("CensusTracts_", statename, "_CAFO_BufferMatches_", d_inner, "_", d.rev,  ".csv")
    write.csv(out.df, file = out.file)
  } else {
    print(paste0("NO CAFOS IN THIS ZONE"))
  }
  print(paste0("ALL DONE")) 
}


#read in census track shape file
shape <- st_read("gz_2010_19_140_00_500k.shp")

#read in CAFOS csv
cafos <- read.csv("IA_Merge_CAFOs_Coords.csv", stringsAsFactors = FALSE)
cafos2 <- na.omit(cafos)

#spatialize census track shapefile and CAFOs to correct grid
cafos_spatialized <- SPATIALIZEcafos(cafos2, 102003)
cents <- SPATIALIZEcensus(shape, 102003)


#buffer of 0.05 
OUTPUTresults(0.05, 0, cents, cafos_spatialized, statename="IA")

#buffer of 0.25
OUTPUTresults(0.25, 0.05, cents, cafos_spatialized, statename="IA")

#buffer of 0.5 
OUTPUTresults(0.5, 0.25, cents, cafos_spatialized, statename="IA")


#buffer of 0.75
OUTPUTresults(0.75, 0.5, cents, cafos_spatialized, statename="IA")

#buffer of 1.0 
OUTPUTresults(1, 0.75, cents, cafos_spatialized, statename="IA")

#buffer of 1.5
OUTPUTresults(1.5, 1, cents, cafos_spatialized, statename="IA")

#buffer of 2.0
OUTPUTresults(2, 1.5, cents, cafos_spatialized, statename="IA")

#buffer of 2.5
OUTPUTresults(2.5, 2, cents, cafos_spatialized, statename="IA")

#buffer of 3.0
OUTPUTresults(3, 2.5, cents, cafos_spatialized, statename="IA")

#buffer of 3.5
OUTPUTresults(3.5, 3, cents, cafos_spatialized, statename="IA")

#buffer of 4.0
OUTPUTresults(4, 3.5, cents, cafos_spatialized, statename="IA")

#buffer of 4.5
OUTPUTresults(4.5, 4, cents, cafos_spatialized, statename="IA")

