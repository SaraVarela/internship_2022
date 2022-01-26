library(raster)
library(sp)
library(rgdal)
library(maps)


setwd("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/comparison")

models <- c("Scotese1",
            "Scotese2",
            "Matthews",  
            "Golonka")



####################################### LATITUDE DEVIATION ####################################################

#nb: odds => lons, evens => lats


#set the color palette
pal <- colorRampPalette(c("blue4", "blue2", "cornflowerblue", "grey88", "indianred", "indianred2", "indianred4"))


plot_lat_difference <- function(mdl1, mdl2){  
  #function to produce a timeseries of plots showing the deviation in the reconstructed temporal latitudes of the spatial data points according to the two models (mdl1 and mdl2)
  #be careful with the order, otherwise, the file won't be recognized
  
  filename <- paste0(mdl1, '_', mdl2, 'diff.csv')
  df <- read.csv(filename)[,-c(1)]
  
  for(k in seq(from = 4, to = ncol(df), by = 2)){ #we start with the latitude of the -10My point (4th column)
    true_time <- (k-2)*5 #for the plot title ( = ((k-2)/2)*10 )
    xyz <- df[, c(1,2,k)] #select the corresponding latitude deviation
    r <- rasterFromXYZ(xyz, 
                       crs = "+proj=longlat +datum=WGS84")  #write the raster file with the UTM projection coord sys
    
    if(true_time < 100){  #add a zero in front of true_time in the name of the file so that the program used to compile the plot as a GIF could sort them properely
      png(paste0("./visualisation/", mdl1,"_vs_", mdl2, "/", mdl1,"_v.s_", mdl2, '_', 0, true_time, ".png"))
    }
    else{
      png(paste0("./visualisation/", mdl1,"_vs_", mdl2, "/", mdl1,"_v.s_", mdl2, '_', true_time, ".png"))
    }
    
    plot(r, 
         col = pal(50),
         main = paste0("Latitude discrepancies hotspots between ", mdl1, " and ", mdl2, " (", true_time ,"Ma)"),
         legend.args = list(text = 'Latitude deviation (°)', side = 4, font = 2, line = 2.5, cex = 0.8),
         zlim = c(-60, 60))
    dev.off()
  }
  return()
}



#computing it for all the models

i = 1
models_copy = models

while(i <= length(models)){
  mdl1 <- models[[i]]
  for(mdl2 in models_copy){
    if(mdl1 != mdl2){
      plot_lat_difference(mdl1, mdl2)
    }
  }
  models_copy = models_copy[-1]  #we get rid of the new first element
  i = i+1
}


### FOR THE SCRIPT TO WRITE THE GIFS, SEE THE PYTHON NOTEBOOK "make_GIFs.ipynb"



######################################## PLATEIDs ASSIGNEMENT #################################################

setwd("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/extracted_paleocoordinates/georeferenced")
store <- read.csv('./Scotese1.csv')[,-c(1,4:8)]
i = 1
while(i < length(models)){
  i = i+1
  store[,i+2] <- read.csv(paste0(models[i], '.csv'))$georef
}

colnames(store) <- c("lon_0", "lat_0", "Scotese1_ID", "Scotese2_ID", "Matthews_ID", "Golonka_ID")

#Nothing was working, it made me angry, hence I wrote this ugly combination of loops
#The point is to get rid of any line with at list one "NA" in the plate IDs... to be improved by maybe getting rid of the NAs when all non-terrestrial models agree, plate boundaries otherwise...

to_drop <- c()
for(index in 1:nrow(store)){
  for(k in 3:6){
    if(is.na(store[index,k]) == T){
      to_drop <- c(to_drop, index)
    }
  }
}

store <- store[-unique(to_drop),]

    # Assess the ID_weight, a metric quantifying the number of different plate IDs a point may have been assigned to


store$ID_weight <- 0

for(id in 1:nrow(store)){
  store$ID_weight[id] <- length(unique(as.numeric(store[id,3:6])))  #the length of the plateID row without duplicates
}

write.csv(store, file = "data_pts_plate_IDs_according_to_the_four_models.csv")


#Building the raster

store <- read.csv("data_pts_plate_IDs_according_to_the_four_models.csv")

r <- rasterFromXYZ(store[,c(2,3,8)], crs = "+proj=longlat +datum=WGS84")
plot(r)

proj_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0"  #mollweide
p <- projectRaster(r, crs = proj_moll)


plot(p, col = c('grey', 'yellow', 'red'),  axes = FALSE, add = FALSE)
map(interior = FALSE, add = TRUE, col = "red", fill = TRUE, wrap = TRUE) #for some obscure reasons, the map doesn't want to display...

##TRY USING rnaturalearth
