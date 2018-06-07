setwd("/Users/cassandrapate/Documents/TrackML")

library(dplyr)
library(data.table)
library(ggplot2)
library(lattice)
library(plotly)

event_id <- "event000001000"
hits <- fread(file="./train_100_events/event000001000-hits.csv", header=TRUE)
cells <- fread(file="./train_100_events/event000001000-cells.csv", header=TRUE)
particles <- fread(file="./train_100_events/event000001000-particles.csv", header=TRUE)
truth <- fread(file="./train_100_events/event000001000-truth.csv", header=TRUE)

#view hit 19144 and the corresponding information
#value is how much charge a particle has deposited in the cell ()
h_id <- 19144
pixel_cluster <- filter(cells, hit_id == "19144")

particle_truths <- arrange(select(filter(truth, particle_id == 968296184995119104), -particle_id), hit_id)

detector_IDs <- c(particle_truths$hit_id)
detector_info <- filter(hits, hit_id %in% detector_IDs)
total <- merge(particle_truths, detector_info, by="hit_id")

#coordinates conversion
#pos_xyz = rotation_matrix * pos_uvw + translation
particle_truths[,"r"]  <-  NA
particle_truths[,"rt"]  <-  NA
particle_truths[,"a0"]  <-  NA
particle_truths[,"z1"]  <-  NA
particle_truths[,"z2"]  <-  NA

for (i  in 1:nrow(particle_truths)) {
  particle_truths$r[i] <- sqrt(particle_truths$tx[i]^2 +  particle_truths$ty[i]^2 + particle_truths$tz[i]^2 )
  particle_truths$rt[i] <- sqrt(particle_truths$tx[i]^2 +  particle_truths$ty[i]^2)
  particle_truths$a0[i] <- atan2(particle_truths$ty[i],particle_truths$tx[1])
  particle_truths$z1[i] <- particle_truths$tz[i]/particle_truths$rt[i] 
  particle_truths$z2[i] <- particle_truths$tz[i]/particle_truths$r[i]  
}           


p <- plot_ly(particle_truths, x = ~tx, y = ~ty, z = ~tz, type = 'scatter3d', mode = 'lines+markers',
             line = list(width = 6, color = ~c, colorscale = 'Viridis'),
             marker = list(size = 3.5, color = ~c, colorscale = 'Greens', cmin = -20, cmax = 50))
#sum of charges in each cell
total_charge <- sum()