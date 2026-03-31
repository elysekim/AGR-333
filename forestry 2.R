rm(list = ls())

# install packages into console
install.packages("terra")
install.packages("tmap")
install.packages("sf")

# load
library(terra)
library(tmap)
library(sf)

# set working directory
getwd()
setwd("C:/Users/ekima/OneDrive/Documents/Documents/AGRLabMaterial")

filepath <- ("C:/Users/ekima/OneDrive/Documents/Documents/AGRLabMaterial")
dem <- rast("unit2.img")
dem

# extract slope
slope <- terrain(dem, v="slope", unit= "degrees", neighbors=8)

aspect <- terrain(dem, v="aspect", unit="degrees")

# visualize slope and aspect
ttm()
tm_shape(slope, alpha=0.5) +
  tm_raster(style = "cont", alpha=0.6, title="Slope (deg)")

tm_shape(aspect) +
  tm_raster(style = "cont")

# the degree range of each direction is 0-50.

# create aspect classification matrix
asp_class <- matrix(c(
  0, 45, 1,
  45, 90, 2,
  90, 175, 2,
  175, 180, 3,
  180, 225, 3,
  225, 270, 4,
  270, 315, 4,
  315, 360, 1), ncol = 3, byrow = TRUE)

asp <- classify(aspect, asp_class)

# visualize reclassified aspect
ttm()
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"))

# screenshot of output image

# read summary table and shapefile
sum_u2 <- read.csv("sum_u2.csv")

# visualize sample forestry inventory plots
svy_pts <- st_read("HEE_Overstory_Survey_Points_2017 - Copy.shp")
svy_pts <- st_transform(svy_pts, 32616)
survey_pts <- subset(svy_pts, Unit =='2')
sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)

# merge summary table with plot locations
unique(sum_u2$Plot)
unique(survey_pts$Plot)

# convert to sf format
sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
sum_u2

# create circular plots
sf_plot <- st_buffer(sum_u2, dist = 17.83)

# unify coordinate systems
crs(sf_plot, proj = T)
crs(asp, proj = T)

# transform crs
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# visualization by dominant species
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
                                       showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West"))+
              tm_shape(sf_plot) +
              tm_polygons('Common.name')+
              tm_layout(legend.outsides = TRUE, legend.outside.size = 0.2) +
              tm_text("Plot", ymod = -0.9)

# dominant tree species

# dominant species by slope 
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominan trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

# dominant by slope

# basal area distriubtion
ttm()
tm_shape(sf_plot) + 
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette = "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# pattern

# trees per acre distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# patterm 

# biomass distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette = "brewer.spectral") +
  tm_layout(title = "Biomass Distribution", 
                                            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()
