library(raster)
library(sp)
library(rgdal)
library(RColorBrewer)


setwd("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/comparison")


#I'm sure some libraries returning the alphabet already exist, but at this time of the day, j'ai trop la flemme

alphabet <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
              "T", "U", "V", "W", "X", "Y", "Z")

#nb: odds => lons, evens => lats


#set the color palette
pal <- colorRampPalette(c("blue4", "blue2", "cornflowerblue", "grey88", "indianred", "indianred2", "indianred4"))


plot_lat_difference <- function(mdl1, mdl2){  
  #function to produce a timeseries of plots showing the deviation in the reconstructed temporal latitudes of the spatial data points according to the two models (mdl1 and mdl2)
  #be careful with the order, otherwise, the file won't be recognized
  
  filename <- paste0(mdl1, '_', mdl2, 'diff.csv')
  df <- read.csv(filename)[,-c(1,2)]  #erase the 2 first columns, which are residual indexes with no interest
  
  for(k in 2:21){
    Time <- 2*k   #corresponding even, hence latitude
    true_time <- (k-1)*10 #for the plot title
    xyz <- df[, c(1,2,Time)] #select the corresponding latitude deviation
    r <- rasterFromXYZ(xyz, 
                       crs = "+proj=longlat +datum=WGS84")  #write the raster file
    
    png(paste0("./visualisation/", alphabet[k-1], '_', mdl1,"_v.s_", mdl2, '_', true_time, ".png"))
    plot(r, 
         col = pal(50),
         main = paste0("Latitude discrepancies hotspots between ", mdl1, " and ", mdl2, " (", true_time ,"Ma)"),
         legend.args = list(text = 'Latitude deviation (°)', side = 4, font = 2, line = 2.5, cex = 0.8),
         zlim = c(-60, 60))
    dev.off()
  }
  return()
}


#example

plot_lat_difference(mdl1 = "Scotese2", mdl2 = "Matthews")


