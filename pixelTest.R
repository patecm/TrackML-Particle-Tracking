setwd("/Users/cassandrapate/Documents/TrackML")

library(dplyr)
library(data.table)
library(ggplot2)

event_id <- "event000001000"
hits <- fread(file="./train_100_events/event000001000-hits.csv", header=TRUE)
cells <- fread(file="./train_100_events/event000001000-cells.csv", header=TRUE)
particles <- fread(file="./train_100_events/event000001000-particles.csv", header=TRUE)
truth <- fread(file="./train_100_events/event000001000-truth.csv", header=TRUE)

#view hit 19144 and the corresponding information
#value is how much charge a particle has deposited in the cell ()
h_id <- 19144
pixel_cluster <- filter(cells, hit_id == "19144")

#ch0 is measured in u direction, ch1 in v direction, w direction is the thickness of the module
min0 <- min(pixel_cluster$ch0)
max0 <- max(pixel_cluster$ch0)
min1 <- min(pixel_cluster$ch1)
max1 <- max(pixel_cluster$ch1)

pixel_cluster[,"ch0diff"] <- NA
pixel_cluster[,"ch1diff"] <- NA

for (pixel in 1:nrow(pixel_cluster)) {
  i0 = as.integer(pixel_cluster$ch0[pixel]- min0 )
  i1 = as.integer(pixel_cluster$ch1[pixel]- min1 )
  pixel_cluster$ch0diff[pixel] <- i0
  pixel_cluster$ch1diff[pixel] <- i1
  }
  
ggplot(data = pixel_cluster, aes(x=ch0diff, y=ch1diff)) + 
    geom_tile(aes(fill = value))

ggplot(data = pixel_cluster, aes(x=ch0diff, y=ch1diff)) + 
  geom_tile(aes(fill = value))

#the edge pixels which are not fully traversed by the particle have only little charge, 
#which is what you expect, because the particle did traverse less of Silicon and thus induces less charge.
pixel_hit <- filter(hits, hit_id == h_id)
truth_hit <- filter(truth, hit_id == h_id)

# translation given by the module center position (cx,cy,cz)
# rotation with reference to the global coordinate system (rot_xy, ..., rot_zw)
# module thicknes  thickness = 2 * module_t
# module dimension (rectangular): 2 * module_minh in local u and 2 * mondule_hv in locl v
# measurement segmentation, i.e. the pixel size pitch_u in u and pitch_v in v

detector_hit <- filter(detectors, 
                       volume_id == pixel_hit$volume_id & 
                         layer_id == pixel_hit$layer_id & 
                         module_id  == pixel_hit$module_id )

#compare the expected and measure cluster cluster size

# nomralize direction vector from the build the direction vector 
p <- sqrt(truth_hit$tpx^2 + truth_hit$tpy^2 + truth_hit$tpz^2)
truth_directionV <- vector()
for (count in 1:3)  {
  truth_directionV[count] <- (truth_hit[count + 2] / p )
}
  
p2 <- sqrt(pixel_hit$x^2 + pixel_hit$y^2 + pixel_hit$z^2)
globalStart_directionV <- vector()
for (count in 1:3)  {
  
  globalStart_directionV[count] <- (pixel_hit[count + 1] / p )
}

#global coordinates, the polar angle
#dr  <- sqrt(dx*dx+dy*dy)
#phi <- arctan2(dy,dx)
#theta <- arctan2(dr,dz)