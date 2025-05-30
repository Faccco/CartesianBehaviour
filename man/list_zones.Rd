% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_zones.R
\name{list_zones}
\alias{list_zones}
\title{Split the trajectory into zones of interest}
\usage{
list_zones(dft, Zones, Zplane = NA, remove2D = FALSE)
}
\arguments{
\item{dft}{A data frame crated by \link[CartesianBehaviour]{standardized_trj} containing the positions and time of an subject.}

\item{Zones}{A data frame crated by \link[CartesianBehaviour]{Zones_int} containing the zones of interest.}

\item{Zplane}{The complementary axis used in \link[CartesianBehaviour]{Zones_int}. Input can be "xz", "zx", "yz" or "zy".}

\item{remove2D}{To remove the 2D zones from the final data. FALSE by default.}
}
\value{
A list containing the zones, witch are lists containing the data frames were the subject was inside the zone.
}
\description{
Split the trajectory into zones of interest
}
\examples{
#Creating an trajectory using the function from the package trajr
if("trajr" \%in\% rownames(installed.packages()) == FALSE){
install.packages("trajr")}else{library(trajr)}
Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
                     angularErrorSd = .8, fps = 30)

#Export the trajectory.
TrajEXP <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", time = "time",
                    id = 1, fps = 30)

#Create the zones of interest
#Normally by Zones_in() but in this case by code directly
Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
ZN <- list(Z1, Z2, Z3, Z4)
Zonas <- list(ZN)
rm(Z1, Z2, Z3, Z4, ZN)

#Split by the zones

TRAJ2D <- list_zones(TrajEXP, Zones = Zonas)

#Run the code below only to see a 3D example, almost the same as the 2D
#Export the trajectory considering
#the Y axis both as Y and Z axis at the same time.
#TrajEXP3D <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", Zaxi = "y",
#                       time = "time", id = 2, fps = 30)
#
#Create your own zones, if you don't know how, go ?Zone_int
#zons3D <- Zones_int(nzonesd2 = 2, nzonesd3 = 4,
#                   faceZ = 1, maxX = 10, minX = -10,
#                   maxY = 5, minY = -5, maxZ = 7,
#                   minZ = -7, npts = 4)
#
#Split the zones
#TRAJ3D <- list_zones(TrajEXP, Zones = zons3D, Zplane = "xz", , remove2D = TRUE)
}
