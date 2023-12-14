#' Time in the zones
#'
#' A data frame containing the time spent in the zones of interest of one subject. If the time is in seconds -  as the data is derived from frames per second - it can be converted by "minute", "hour", "day" input.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones} or \link[CartesianBehaviour]{Allbasics}.
#' @param time.scale Will divide the time parameter to re-scale it.
#'
#' @return A data frame with the total time in each zone and the total time of the analysis.
#' @export times_zone
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn by setting Zones = NA,
#' #but in this case is made by code directly
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Perform all the measures
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", frames = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the time inside each zone of interest
#' Time.Zones <- times_zone(TRAJ2D)
#' #View(Time.Zones)
times_zone <- function(list_zones, time.scale = "sec"){
AA <- base::data.frame()
list_zone <- list_zones[[1]]
for (i in 1:base::length(list_zone)){

  if(i != base::length(list_zone)){
    Ani <- list_zone[[i]]
  }else{
    Ani <- base::list(list_zone[[i]])
  }

  spentime <- base::sum(base::as.numeric(TIME_ZONE(Ani), time.scale = time.scale))
  meantime <- base::mean(base::as.numeric(TIME_ZONE(Ani), time.scale = time.scale))
  BB <- base::cbind(spentime, meantime)
  AA <- base::rbind(AA, BB)
  AA
}
namezon <- base::vector()
for (i in 1:base::nrow(list_zones[[2]])) {
  namezon <- base::append(namezon, base::paste("Zone", list_zones[[2]][i,], sep=""))
}
namezon <- base::append(namezon, "Total")
base::row.names(AA) <- namezon
base::colnames(AA) <- base::c("Total time in zone", "Mean time in zone")
AA
}
