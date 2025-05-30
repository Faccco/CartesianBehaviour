% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_times_zone.R
\name{g_times_zone}
\alias{g_times_zone}
\title{Time in the zones for a list of samples}
\usage{
g_times_zone(list_group, tm.scale = "sec")
}
\arguments{
\item{list_group}{The output of \link[CartesianBehaviour]{list_zones}.}

\item{tm.scale}{Will divide the time parameter to re-scale it. If the time is in seconds -  as the data is derived from frames per second - it can be converted by "minute", "hour", "day" input.}
}
\value{
A data frame containing the time spent in the zones of interest of one subject.
}
\description{
As the title says, this function exports the total time spent inside a defined zone of interest.
}
\examples{
#Creating an trajectory using the function from the package trajr
if("trajr" \%in\% rownames(installed.packages()) == FALSE){
install.packages("trajr")}else{library(trajr)}

TRAJ <- list()
for(i in 1:10){
TRAJ[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
                     angularErrorSd = .8, fps = 30)
}

#Create the zones of interest
#Can be manually drawn by setting Zones = NA,
#but in this case is made by code directly)
Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
ZN <- list(Z1, Z2, Z3, Z4)
Zonas <- list(ZN)
rm(Z1, Z2, Z3, Z4, ZN, i)

#Perform all the measures
TRAJ2D <- g_Allbasics(TRAJ, Xaxi = "x", Yaxi = "y", time = "time",
                    Zones = Zonas, threshold = 3,
                    Dist.reg = "point",
                    reg = data.frame(x = 0, y = 0))

#Create the data frame of
#the time inside of the zones for a group of subjects
Group.Time.Zones <- g_times_zone(TRAJ2D)
#View(Group.Time.Zones)
}
