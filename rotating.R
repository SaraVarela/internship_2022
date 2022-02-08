library(raster)
library(sp)
library(rgdal)

setwd('C:/Users/lucas/OneDrive/Bureau/Internship_2022/project')


#### First, build a 1x1° meshgrid and convert it to shapefile that will further be opened in Gplates ####

r <- raster(res = 1) #start with a 1x1 raster
pos <- xyFromCell(object = r, cell = 1:ncell(r))  #extract coordinates as a df
xy <- data.frame(pos)

xy$Beginning <- 544 #500Ma ago, beginning of rotation, max time
xy$End <- 0

xy.df <- SpatialPointsDataFrame(coords = xy[,1:2], data = xy)
proj4string(xy.df)<- CRS("+proj=longlat +datum=WGS84") #assign coord system to the SpatialPointsDataFrame

raster::shapefile(xy.df, paste0(getwd(),"/meshgrid.shp"), overwrite = TRUE)  #create shapefile from the SpatialPtsDF and save it with the same command




#### Then read the data and extract the paleocoordinates model per model ####

models <- c("Scotese2",  #PALEOMAP latest version
            "Matthews",  
            "Golonka",
            "Seton")

MaxTime <- c("Scotese2" = 540,
             "Matthews" = 410,
             "Golonka" = 540,
             "Seton" = 200)  #the maximum time we want to reach, we basically go as far as the model goes
                               #rounded to 540 (instead of 544) for Golonka and Wright

#extraction loop
for(mdl in models){
  coords_over_time <- data.frame(lon_init = xy.df$x,
                                 lat_init = xy.df$y)   #in this dataframe, we'll store the evolution of the coordinates of the spatial points over time, given a model
  maxTime <- MaxTime[[mdl]]
  Timeframe <- seq(from = 10, to = maxTime, by = 10)   #from 10 to the maximal duration covered with a timestep of 10My (we don't consider 0 as we initialise our storing dataframe with initial coordianates, corresponding to 0)
  
  #create the name of the output datasets' columns (lon and lat for any time in Timeframe)
  names <- c()
  for(t in seq(from = 0, to = maxTime, by = 10) ){
    names <- c(names, paste0("lon_", t), paste0("lat_", t))
  }
  
  for(t in Timeframe){
    dir <- paste0("./rotated_shapefiles_10My_intervals/", mdl, "/meshgrid/reconstructed_", t, ".00Ma.shp")
    shape <- shapefile(x = dir) #shapefile of the corresonding model at the corresponding time
    df <- as.data.frame(shape)  #we convert the shapefile into a dataframe, therefore containing the paleocoordinates of the spatial data points
    
    index <- 2*t/10 #the column index (t/10 as we have a 10My step, multiplied by 2 as two features per step: lat and lon)
    coords_over_time[,index+1] <- df$coords.x1 #we store these paleocoordinates in coords_over_time
    coords_over_time[,index+2] <- df$coords.x2
  }
  colnames(coords_over_time) <- names
  path <- "./extracted_paleocoordinates/"
  write.csv(x = coords_over_time, 
            file = paste0(path, mdl, ".csv")) #we finally export the coordinates over time as .csv file
}

