# CartesianBehaviour - Points to behaviour <img src="man/figures/logo.png" align="right" width="120"/>

[![R-CMD-check](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml) ![CRAN](https://www.r-pkg.org/badges/version/CartesianBehaviour) ![CRANLOGS](https://cranlogs.r-pkg.org/badges/CartesianBehaviour) [![DOI:\<ssrn.5123233\>](http://img.shields.io/badge/DOI-%3Cssrn.5123233%3E-%3C#800080%3E.svg)](http://dx.doi.org/10.2139/ssrn.5123233)

## Description

This project seeks to simplify the conversion of Cartesian tabular data provided from any source into behavioral endpoints. It get inspired by the packages <a href="https://swarm-lab.github.io/trackR/">TrackR</a> and <a href="https://github.com/JimMcL/trajr">trajr</a>, check them for options of tracking already implemented. The staring point was to measure zebrafish (*Danio rerio*) behavior within zones of interest. Future updates aim to: - Process data without zones (Speed improvement) [SHORT TERM] - Implement a shiny app [MEDIUM TERM] - Integrate with a proper tracking app [LONG TERM]

## Instalation

Instillation is possible both with [remotes](https://cran.r-project.org/web/packages/remotes/index.html) install_github()

```         
install.packages("remotes") # 2.5.0
library(remotes)
install_github("Faccco/CartesianBehaviour")
```

### Create some tracks
For this propose we will use [trajr](https://github.com/JimMcL/trajr) to generate some trajectory's

```   
#Load packages
install.packages("trajr") # 1.5.1
library(trajr) 
library(CartesianBehaviour)

#Create trajectorys
for(i in 1:10){Traj[[i]] <- TrajGenerate(5000, random = TRUE, stepLength = .01, angularErrorSd = .8, fps = 30)}

##Create zones of interest
#For this you can run bellow as a preset zones or draw them using Zone_int()
Zones <- list(list(data.frame(x = c(-1000,1000,1000,-1000), y = c(-1000,-1000,1000,1000))))

#The function Zone_int uses the Plot tab and YOU NEED TO CLICK in the zone vertices. No Click no output.
#Zones <- Zones_int(nzonesd2 = 3, maxX = 10, minX = -10, maxY = 5, minY = -5, npts = 4, Circ_Arena = F)

# Calculation of Metrics
Dados_2 <- g_Allbasics(TRAJ,
                       Xaxi = "x",          # X-axis coordinate column
                       Yaxi = "y",          # Y-axis coordinate column
                       #time = "time",      # Uncomment if using time column
                       frames = "time",     # Specify if time is in frames or seconds
                       fps = 30,            # Frames per second for conversion
                       Zones = Zones,       # Zone configuration list
                       #            n.zonesd2,          # Number of zones (optional)
                       #            faceZ = 0,          # Zone facing direction (optional)
                       #            maxX = NA,          # Maximum X boundary (optional)
                       #            minX = NA,          # Minimum X boundary (optional)
                       #            maxY = NA,          # Maximum Y boundary (optional)
                       #            minY = NA,          # Minimum Y boundary (optional)
                       #            npts = 4,           # Number of polygon points (optional)
                       Circ_Arena = F,      # Circular arena flag
                       threshold = 0.59,    # Immobility threshold (zebrafish: <0.59 cm/s)
                       Dist.reg = F,        # Enable distance tracking from reference
                       reg = NA             # Reference coordinates (point/line/plane)
)


# Total distance traveled
Dist <- g_dist_zone(Dados_2)
Dist

# Absolute turn angle calculation
ATA <- g_angle_zone(Dados_2)
ATA

# Maximum speed analysis
Max <- g_MMspeed(Dados_2)
Max

# Immobility analysis (1-second threshold)
Im <- g_under_sepisods(Dados_2, time_threshold = 1)
Im

# Time spent in each zone
Tm <- g_times_zone(Dados_2)
Tm

# Zone transitions count
Tran <- g_transitions(Dados_2)
Tran

# Average speed calculation
Vel <- mean_speed_zone(dist_zone = Dist, time_zones = Tm)
Vel
```
For those interested in another solutions we tested and recommend

For tracking:<br>
  - [DeepLabCut](https://github.com/DeepLabCut/DeepLabCut) (Neural network based)<br>
  - [TrackR](https://swarm-lab.github.io/trackR/) (Package who inspired this project)<br><br> 
  For evaluation:<br> - [trajr](https://github.com/JimMcL/trajr) (Some functions are borrowed from this one)<br> - [MoveR](https://github.com/qpetitjean/MoveR)<br> - [celltrackR](https://github.com/ingewortel/celltrackR)<br><br> For both:<br> - [ToxTrac](https://sourceforge.net/projects/toxtrac/)<br> - [AnimalTA](http://vchiara.eu/index.php/animalta)<br>
