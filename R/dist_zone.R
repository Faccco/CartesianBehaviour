#' Total distance traveled
#'
#' The total distance traveled and total distance in each zone for one subject.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{distances_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return A data frame with the mean distances by each zone and the total distance traveled.
#' @export dist_zone
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn by setting Zones = NA,
#' #but in this case is made by code directly)
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN, i)
#'
#' #Perform all the measures
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the distance traveled
#' Distance.Zones <- dist_zone(TRAJ2D)
#' #View(Distance.Zones)
dist_zone <- function(list_zones){
  AA <- base::data.frame(0)
  list_zone <- list_zones[[1]]
  for (i in 1:base::length(list_zone)) {
    zonedist <- list_zone[[i]]

    if(i == base::length(list_zone)){
      zonedist <- base::list(list_zone[[i]])
    }

    if(base::length(zonedist) > 0){
      BB <- base::data.frame()
      for (j in 1:base::length(zonedist)) {
        distancezon <- base::sum(zonedist[[j]]$dist)
        BB <- base::rbind(BB, base::as.numeric(distancezon))
        BB
      }
    }else{
      BB <- 0
    }
    sumdist <- base::sum(BB[[1]])
    AA <- base::cbind(AA, sumdist)
    AA

  }
  AA <- AA[,2:(base::length(list_zone)+1)]
  namezon <- base::vector()
  for (i in 1:nrow(list_zones[[2]])) {
    namezon <- base::append(namezon, base::paste("Distance in Zone", list_zones[[2]][i,], sep=""))
  }
  namezon <- base::append(namezon, "Total")
  base::colnames(AA) <- namezon
  AA
}
