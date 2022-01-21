library(raster)
library(sp)
library(rgdal)



models <- c("Scotese1",  #Scotese 2008, earlier version of PALEOMAP
            "Scotese2",  #PALEOMAP latest version
            "Matthews",  
            "Golonka")



#Our first goal is to georeference the coordinates of the grid with the spatial polygons of each rotation model
#This would enable us to attribute a plateID to each point.
#In the case of the models that only consider continental plates motion, the points in the oceans wil just be attributed to no plateID

      #BUILD THE GRID AGAIN

r <- raster(res = 1) #start with a 1x1 raster
pos <- xyFromCell(object = r, cell = 1:ncell(r))  #extract coordinates as a df
xy <- data.frame(pos)

xy$Beginning <- 500 #500Ma ago, beginning of rotation
xy$End <- 0

xy.df <- SpatialPointsDataFrame(coords = xy[,1:2], data = xy)
proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84") #assign coord system to the SpatialPointsDataFrame


    #IMPORT MODELS' POLYGONS AS SHAPEFILES AND PROCEED TO THE GEOREFERENCING

        #Target Oceans
        #In a first time, as 3 out of the 4 models studied only consider continental plates' motion, we'll draw our analysis on the continents only


get_oc_pos <- function(i){
  #returns indexes of the pixels associated with oceans in the terrestrial-only models (i.e plateID = NA)
  #i is the index of the model in the "model" vector
  indexes <- c()
  
  if(i == 1){
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Scotese1/Scotese_2008_PresentDay_ContinentalPolygons.shp"
    shape <- shapefile(dir)  #we open the corresponding Gplates shapefile
    georef <- over(xy.df, shape)$PLATEID1 #georeferencing the spatial data points with the Gplates shp
    merged <- cbind.data.frame(xy.df, georef)  #merging both
  }
  
  else if(i == 2){
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Scotese2/Scotese PaleoAtlas_v3/PALEOMAP Global Plate Model/PALEOMAP_PlatePolygons/reconstructed_0.00Ma/reconstructed_0.00Ma_polygon.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
    merged <- cbind.data.frame(xy.df, georef)
  }
  
  else if(i == 3){
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Matthews/ContinentalPolygons/Shapefile/Matthews_etal_GPC_2016_ContinentalPolygons.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
    merged <- cbind.data.frame(xy.df, georef)
  }
  
  else if(i == 4){
    dir <- "C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/Rotation_models/Golonka/Golonka_2007_PresentDay_ContinentalPolygons.shp"
    shape <- shapefile(dir)
    georef <- over(xy.df, shape)$PLATEID1
    merged <- cbind.data.frame(xy.df, georef)
  }

  for(k in 1:nrow(merged)){
    if(is.na(merged$georef[[k]]) == TRUE){  #if the element we are having a look has no plateID attribute, which means that it belongs to the oceans
      indexes <- c(indexes, k)  #it is stored in the index list
    }
  }
  return(indexes)
}


#we get the initial coordinates of the spatial data points (will be used after as substracting to df makes them = 0, which is annoying for the rest of the work)
coords_ref <- read.csv('C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/extracted_paleocoordinates/Scotese1.csv')[,2:3]



      #COMPARISON
        #in the case of the models that actually cover marine plates, 
        #we'll make sure to erase the same points as for the one we compare it with  



setwd('C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/extracted_paleocoordinates')


assess_diff <- function(mdl1, mdl2){
  #this function assess how different the reconstructions of mdl2 are with respect to mdl1 (mdl1 - mdl2)
  
  df1 <- read.csv(file = paste0(mdl1, '.csv')) #open the datasets containing the paleocoordinates over time of the corresponding models
  df2 <- read.csv(file = paste0(mdl2, '.csv'))
  
  i1 <- which(models == mdl1) #position of the models in "models"
  i2 <- which(models == mdl2)
  index_oceans1 <- get_oc_pos(i1)  #target the indexes of the data points associated with no plate ID => oceans in the case of the continental models (otherwise, the output list is null)
  index_oceans2 <- get_oc_pos(i2)
  
  if(length(index_oceans1) < length(index_oceans2)){
    for(i in 1:ncol(df1)){ #no matters the df, they all have the same column numbers
      df1[index_oceans2, i] = NA
      df2[index_oceans2, i] = NA
    }
  }
  
  else if(length(index_oceans1) > length(index_oceans2)){
    for(i in 1:ncol(df1)){
      df1[index_oceans1, i] = NA
      df2[index_oceans1, i] = NA
    }
  }
  difference <- df1-df2
  difference[,2:3] = coords_ref
  return(difference)
}


#running the functions to compare the outputs of each model 2 by 2 (avoiding to compare twice the same models and also not comparing a model with itself)
#outputs saved in the "comparison" folder

i = 1
models_copy = models

while(i < length(models)){
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



