# CartesianBehaviour - Points to Behaviour <img src="man/figures/logo.png" align="right" width="120"/>

[![R-CMD-check](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml) ![CRAN](https://www.r-pkg.org/badges/version/CartesianBehaviour) ![CRANLOGS](https://cranlogs.r-pkg.org/badges/CartesianBehaviour) [![DOI:10.2139/ssrn.5123233](https://img.shields.io/badge/DOI-10.2139/ssrn.5123233-blue)](https://doi.org/10.2139/ssrn.5123233)

## Description

This package simplifies the conversion of Cartesian tabular data from various sources into behavioral endpoints. Originally developed for analyzing zebrafish (*Danio rerio*) behavior within zones of interest, it draws inspiration from [TrackR](https://swarm-lab.github.io/trackR/) and [trajr](https://github.com/JimMcL/trajr) packages. Future development plans include:
  
  - Speed improvements for zone-free data processing (Short Term)
  - Shiny app integration (Medium Term)
  - Native tracking software integration (Long Term)

## Installation

Install via [remotes](https://cran.r-project.org/package=remotes):
  
```
install.packages("remotes")
remotes::install_github("Faccco/CartesianBehaviour")
```

## Basic Usage
Trajectory Generation with [trajr](https://github.com/JimMcL/trajr)

```
library(trajr)
library(CartesianBehaviour)

# Generate sample trajectories
trajectories <- lapply(1:10, function(i) {
  TrajGenerate(5000, random = TRUE, stepLength = 0.01, angularErrorSd = 0.8, fps = 30)
})

# Define zones of interest (predefined example)
zones <- list(
  list(data.frame(
    x = c(-1000, 1000, 1000, -1000),
    y = c(-1000, -1000, 1000, 1000)
  )))
```

### Interactive Zone Definition

```
# Uncomment to interactively define zones. You MUST CLICK in the plot tab to get and output.
# zones <- Zone_int(
#   nzonesd2 = 3, # Number of zones of nterest
#   maxX = 10, minX = -10, # Maximum and minimum X positions of the arena
#   maxY = 5, minY = -5, # Maximum and minimum Y positions of the arena
#   npts = 4, # Number of vertices that define the polygon representing the zone of interest.
#   Circ_Arena = FALSE # A logical indicating whether do draw a circular boundary to position the point.
# )
```

## Behavioral Analysis
<br>
*NOTE:* To analyze your behavioral data using the functions above, your dataset must include a list of data frames (trajectories), each containing at minimum the X, Y, and time columns representing spatial coordinates and temporal frames, respectively. <br><br>
Before running the core metric functions, make sure to carefully define your zones of interest (Zones = zones) based on your specific video setup. The configuration of zones—such as their shape, position, and number of polygon points—needs to be tailored to your arena layout and experimental design. Refer to the n.pts, Zones, and Circ_Arena arguments for proper setup.
```
# Calculate core metrics
analysis_data <- g_Allbasics(
  list_subjects = trajectories, # Dataframe with the track
  Xaxi = "x", # Name of the x column or a vector with the x positions
  Yaxi = "y", # Name of the y column or a vector with the y positions
  frames = "time", # Name of the frames column or a vector with the frames
  fps = 30, # Frame rate of the video
  #n.zonesd2,          # Number of zones (optional)
  #faceZ = 0,          # Zone facing direction (optional)
  #maxX = NA,          # Maximum X boundary (optional)
  #minX = NA,          # Minimum X boundary (optional)
  #maxY = NA,          # Maximum Y boundary (optional)
  #minY = NA,          # Minimum Y boundary (optional)
  #npts = 4,           # Number of polygon points (optional)
  Zones = zones, # A data frame crated containing the zones of interest.
  Circ_Arena = FALSE, # Logical, plot a circle the size f the arena if Zones is NA.
  threshold = 0.59,  # Immobility threshold (zebrafish: <0.59 cm/s)
  Dist.reg = FALSE, # Vector, can be the distance to a "point", "line" or "plane". NA for no measure.
  reg =  "NOT" # A data frame with the x,y axis. "NOT" by default to pin the point/line/plane manually in the plot tab.
)

# Extract specific metrics
distance <- g_dist_zone(analysis_data)
View(distance)
turn_angle <- g_angle_zone(analysis_data)
View(turn_angle)
max_speed <- g_MMspeed(analysis_data)
View(max_speed)
immobility <- g_under_sepisods(analysis_data, time_threshold = 1)
View(immobility)
zone_time <- g_times_zone(analysis_data)
View(zone_time)
transitions <- g_transitions(analysis_data)
View(transitions)
avg_speed <- mean_speed_zone(distance, zone_time)
View(avg_speed)
```

## Recommended Complementary Tools

For those interested in another solutions we tested and recommend

For tracking:<br>
  - [DeepLabCut](https://github.com/DeepLabCut/DeepLabCut) (Neural network based)<br>
  - [TrackR](https://swarm-lab.github.io/trackR/) (Package that inspired this project)<br><br>
  For evaluation:<br>
  - [trajr](https://github.com/JimMcL/trajr) (Some functions are borrowed from this one)<br>
  - [MoveR](https://github.com/qpetitjean/MoveR)<br>
  - [celltrackR](https://github.com/ingewortel/celltrackR)<br><br>
  For both:<br>
  - [ToxTrac](https://sourceforge.net/projects/toxtrac/)<br>
  - [AnimalTA](http://vchiara.eu/index.php/animalta)<br>
