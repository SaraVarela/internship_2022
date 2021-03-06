library(raster)
library(sp)
library(rgdal)



models <- c("Scotese2",  #PALEOMAP latest version
            "Matthews",  
            "Golonka",
            "Seton")

MaxTime <- c("Scotese2" = 540,
             "Matthews" = 410,
             "Golonka" = 540,
             "Seton" = 200)  #the maximum time we want to reach, we basically go as far as the model goes
                              #rounded to 540 (instead of 544) for Golonka



setwd('C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/extracted_paleocoordinates')


#Our first goal is to georeference the coordinates of the grid with the spatial polygons of each rotation model
#This would enable us to attribute a plateID to each point.
#In the case of the models that only consider continental plates motion, the points in the oceans will just be attributed to no plateID

#BUILD THE GRID AGAIN

r <- raster(res = 1) #start with a 1x1 raster
pos <- xyFromCell(object = r, cell = 1:ncell(r))  #extract coordinates as a df
xy <- data.frame(pos)

xy$Beginning <- 500 #500Ma ago, beginning of rotation
xy$End <- 0

xy.df <- SpatialPointsDataFrame(coords = xy[,1:2], data = xy)
#we further specify spatial data points CRS in 

#IMPORT MODELS' POLYGONS AS SHAPEFILES AND PROCEED TO THE GEOREFERENCING

#Target Oceans
#In a first time, as 3 out of the 4 models studied only consider continental plates' motion, we'll draw our analysis on the continents only


get_na_pos <- function(i){
  #returns indexes of the pixels associated with non attributed plateIDs (plateID = NA)
  #In the terrestrial-only models, these points are the ocean plates. However, as not all model have the same plate boundaries, there might be discrepancies between the models
  #i is the index of the model in the "model" vector

  if(i == 1){
    proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84") #assign coord system to the SpatialPointsDataFrame
    
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Scotese2/Scotese PaleoAtlas_v3/PALEOMAP Global Plate Model/PALEOMAP_PlatePolygons/reconstructed_0.00Ma/reconstructed_0.00Ma_polygon.shp"
    shape <- shapefile(dir) #we open the corresponding Gplates shapefile
    georef <- over(xy.df, shape)$PLATEID1  #georeferencing the spatial data points with the Gplates shp
    merged <- cbind.data.frame(xy.df, georef)  #merging both  
  }
  
  else if(i == 2){
    proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84") #assign coord system to the SpatialPointsDataFrame
    
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Matthews/ContinentalPolygons/Shapefile/Matthews_etal_GPC_2016_ContinentalPolygons.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
    merged <- cbind.data.frame(xy.df, georef)
  }

  else if(i == 3){
    proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84") #assign coord system to the SpatialPointsDataFrame
    
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Golonka/Golonka_2007_PresentDay_ContinentalPolygons.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
  }
  
  else if(i == 4){ #FOR SOME REASONS, DOESN'T WRK
    proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84 +no_defs") #change spatial data points df CRS so it matches with the one of the shapefile
    
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Seton_etal_2012_ESR/Coastlines/Seton_etal_ESR2012_Coastline_2012.1_polyline.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
    merged <- cbind.data.frame(xy.df, georef)
  }
  
  write.csv(merged, file = paste0("./georeferenced/", models[i],".csv"))  #get df with the assigned plate IDs
  
  indexes <- which(is.na(georef) == TRUE)

  return(indexes)
}


#we get the initial coordinates of the spatial data points (will be used after as substracting to df makes them = 0, which is annoying for the rest of the work)
coords_ref <- read.csv('C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/extracted_paleocoordinates/Scotese2.csv')[,2:3]



#COMPARISON
#in the case of the models that actually cover marine plates, we'll make sure to erase the same points as for the one we compare it with  


assess_diff <- function(mdl1, mdl2){
  #this function evaluates how different the reconstructions of mdl1 are with respect to mdl2 (mdl1 - mdl2)
  
  df1 <- read.csv(file = paste0(mdl1, '.csv'))[,-c(1)] #open the datasets containing the paleocoordinates over time of the corresponding models
  df2 <- read.csv(file = paste0(mdl2, '.csv'))[,-c(1)] #and erase the first column, residual indexes with no interest
  
  i1 <- which(models == mdl1) #position of the models in "models"
  i2 <- which(models == mdl2)
  index_oceans1 <- get_na_pos(i1)  #target the indexes of the data points associated with no plate ID that we attribute as oceans in the case of the continental models
  index_oceans2 <- get_na_pos(i2)
  t1 <- MaxTime[[i1]]
  t2 <- MaxTime[[i2]]
  
  #select the temporal coverage of the model that has the minimal one
  chosen_time <- min(t1, t2)
  
  if(length(index_oceans1) < length(index_oceans2)){
    for(i in 1:chosen_time/10){
      df1[index_oceans2, i] = NA
      df2[index_oceans2, i] = NA
    }
  }
  
  if(length(index_oceans1) >= length(index_oceans2)){
    for(i in 1:chosen_time/10){
      df1[index_oceans1, i] = NA
      df2[index_oceans1, i] = NA
    }
  }
  
  difference <- df1-df2
  difference[,1:2] = coords_ref
  return(difference)
}


#running the functions to compare the outputs of each model 2 by 2 (avoiding to compare twice the same models and also not comparing a model with itself)
#outputs saved in the "comparison" folder

i = 1
models_copy = models

while(i <= length(models)){
  mdl1 <- models[[i]]
  for(mdl2 in models_copy){
    if(mdl1 != mdl2){
      difference <- assess_diff(mdl1, mdl2)
      write.csv(difference, 
                file = paste0("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/comparison/", mdl1, '_', mdl2, 'diff.csv'))
    }
  }
  models_copy = models_copy[-1]  #we get rid of the new first element
  i = i+1
}



mdl1 <- "Seton"
for(mdl2 in models_copy){
  if(mdl1 != mdl2){
    difference <- assess_diff(mdl1, mdl2)
    write.csv(difference, 
              file = paste0("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/comparison/", mdl1, '_', mdl2, 'diff.csv'))
  }
}

