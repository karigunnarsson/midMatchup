library(grid)
library(gtable)
library(png)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(car)
library(rgl)

gameData <- readRDS("matches.RDS")
gameDataClean <- unique(select(gameData, matchID, radiantWin, heroID, gold, xp, meanX, meanY, avgDistMean, side))

radiantData <- subset(gameDataClean, gameDataClean$side == "Radiant")
direData <- subset(gameDataClean, gameDataClean$side == "Dire")

map.colors <- colorRampPalette(c("green","yellow","red"))

# We set seed to make this reproducable:
set.seed(322)
################## Radiant ####################################
# I want to have a 2D density plot / heatmap of the minimap to show where the points are on the dota map
densityPlot <- ggplot(sample_n(radiantData, 10000), aes(x = meanX, y = ,meanY)) + 
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) +
  scale_alpha(range = c(0, 1.0)) + scale_fill_gradientn(colours = map.colors(5)) + 
  xlim(70,185) + ylim(70,185)

minimap <- readPNG('images/minimap.png')
densityPlot2 <- densityPlot + annotation_raster(minimap, ymin = 70 ,ymax=185 ,xmin = 70,xmax = 185) + 
  stat_density2d(geom="raster", aes(fill=..density.., alpha=10*sqrt(..density..)), contour=FALSE, n=100)
densityPlot2

# Using the plot, we eyeball the starting X and Y coordinates for the KMeans clustering
# use 4 clusters (top, mid, bot and jungle).
initX <- c(80,110,140,160)
initY <- c(140,120,100,90)
initClust <- cbind(initX, initY)

radiantCluster <- kmeans(select(radiantData, meanX, meanY), initClust)
radiantData$cluster <- radiantCluster$cluster

r3dDefaults$windowRect <- c(0,50, 800, 800)
# Plotting the 3D scatterplot
open3d()

plot3d( select(radiantData, meanX, meanY, avgDistMean), type="p", radius=0.1, axes=F, 
        xlim = c(70,185), ylim = c(70,185), col = as.factor(radiantData$cluster),
        expand = 0, xlab = "Mean X position", ylab = "Mean Y position", zlab = "Average distance")

#legend3d("topright", paste('cluster', c(1,2,3,4,5,6)), col = as.factor(radiantData$newClust) ,pch = 16)

axes3d(c("x","y","z") )
show2d({                  # show2d uses 2D plot function's output as a texture on a box.
  grid.draw(gtable_filter(ggplotGrob(densityPlot2), "panel"))
},
expand = 1 , texmipmap = F )

# This gives pretty bad fitting for jungle as you can see, but it?s fine for mid, which is all we want
# for this analysis.

################## Dire ####################################
# Do the same for dire data
densityPlot <- ggplot(sample_n(direData, 10000), aes(x = meanX, y = ,meanY)) + 
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) +
  scale_alpha(range = c(0, 1.0)) + scale_fill_gradientn(colours = map.colors(5)) + 
  xlim(70,185) + ylim(70,185)

minimap <- readPNG('~/yasp/minimap.png')
densityPlot2 <- densityPlot + annotation_raster(minimap, ymin = 70 ,ymax=185 ,xmin = 70,xmax = 185) + 
  stat_density2d(geom="raster", aes(fill=..density.., alpha=10*sqrt(..density..)), contour=FALSE, n=100)
densityPlot2

# Using the plot, we eyeball the starting X and Y coordinates for the KMeans clustering
# use 4 clusters (top, mid, bot and jungle).
initX <- c(90,130,105,170)
initY <- c(165,135,155,110)
initClust <- cbind(initX, initY)

direCluster <- kmeans(select(direData, meanX, meanY), initClust)
direData$cluster <- direCluster$cluster

r3dDefaults$windowRect <- c(0,50, 800, 800)
# Plotting the 3D scatterplot
open3d()

plot3d( select(direData, meanX, meanY, avgDistMean), type="p", radius=0.1, axes=F, 
        xlim = c(70,185), ylim = c(70,185), col = as.factor(direData$cluster),
        expand = 0, xlab = "Mean X position", ylab = "Mean Y position", zlab = "Average distac")

#legend3d("topright", paste('cluster', c(1,2,3,4,5,6)), col = as.factor(radiantData$newClust) ,pch = 16)

axes3d(c("x","y","z") )
show2d({                  # show2d uses 2D plot function's output as a texture on a box.
  grid.draw(gtable_filter(ggplotGrob(densityPlot2), "panel"))
},
expand = 1 , texmipmap = F )

# We see similar problem here, good mid lane accuracy but not great on other lanes.

combined <- rbind(direData,radiantData)
scatterPlot <- ggplot(combined, aes(x = meanX, y = meanY, color = as.factor(cluster))) + geom_point(alpha = 0.02)+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + xlim(70,185) + ylim(70,185)

minimap <- readPNG('~/yasp/minimap.png')
scatterPlot2 <- scatterPlot + annotation_raster(minimap, ymin = 70 ,ymax=185 ,xmin = 70,xmax = 185) +
  geom_point(alpha = 0.02) + xlim(70,185) + ylim(70,185) + guides(colour = guide_legend(override.aes = list(alpha = 1)))
scatterPlot2

# So we see that 2 is mid cluster for both sides.

# For some reason some have mean zero, we remove those, as well as only looking at the mid lane
combinedClean <- subset(combined, combined$meanX > 70 & combined$meanY > 70 & combined$cluster == 2)

# Clearly there can be more than one hero mid, so to limit this to only ONE hero, we will
# define that the hero with the lowest spread in distance is the "real" mid hero.

combinedClean <- arrange(combinedClean, matchID, side, avgDistMean)

# This should create a numbering sequence in which the mid player in each game/side combination, with
# the lowest average distance, gets the number 1, and everyuone else gets a higher number.
# Thought being a roamer or support might have average distance at mid, but would be rotating a lot more
# so low avg distance gets priority if two are classified as mid.
combinedClean <- combinedClean %>% group_by(matchID, side) %>%
  mutate(id = seq_along(avgDistMean))

combinedFinal <- subset(combinedClean, combinedClean$id == 1)

saveRDS(combinedFinal, "combinedFinal.RDS")
