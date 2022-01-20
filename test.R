library(raster)

setwd("C:/Users/lucas/OneDrive/Bureau/Internship_2022/project/paleomaps")


r <- raster(res = 1,
            xmn = -180, 
            xmx = 180, 
            ymn = -90, 
            ymx = 90) #raster with a resolution of 1°

###### MESHGRID ATTEMPT #######

spatial_coord <- xyFromCell(object = r, cell = 1:64800)  #create a meshgrid of 1x1° (2*180 + 2*90 = 64800 cells)

png('meshgrid.png', height=nrow(r), width=ncol(r)) 
plot(spatial_coord, maxpixels=ncell(r))
dev.off()

cde

plot(r)


###### Battle plan #######
# export reconstructed geometries in Gplates as .shp
# rasterize the resulting polygons with a 1x1° resolution
# extract the coordinates of the pixels

library(sf)
library(raster)

setwd('C:\Users\lucas\OneDrive\Bureau\Internship_2022\Gplates_tuto\Short_Course_Tutorial_Data\9.2-Plate_Reconstructions\Seton_etal_ESR2012_Coastlines_2012.1_Polygon')

r <- raster(res = 1,
            xmn = -180, 
            xmx = 180, 
            ymn = -90, 
            ymx = 90) #raster with a resolution of 1°

sf_init <- read_sf("reconstructed_0.00Ma.shp")
sf_init1 <- read_sf("reconstructed_5.00Ma.shp")

df <- as.data.frame(sf_init)
df1 <- as.data.frame(sf_init1)



object <- rasterize(sf_init, r) #rasterized map if a 1° resolution
object1 <- rasterize(sf_init1, r) #rasterized map if a 1° resolution


plot(object)


coords <- rasterToPoints(object)
coords1 <- rasterToPoints(object1)
head(coords1)



#wait, seems to be a trouble with the "layers" (plate ID?)... 

layer0 <- unique(coords[,3])
layer1 <- unique(coords1[,3]) #different lengths

common <- 0
for(cat in layer0){
  if(cat %in% layer1){
    common <- common + 1
  }
}
common # != 364... different categories then?





