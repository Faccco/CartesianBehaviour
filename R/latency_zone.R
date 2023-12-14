#' The latency to first enter a zone
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return A data frame of the latency to first enter a zone of interest. if the subject starts in the zone the latency is 0. If it never reaches the zone the latency is the total time of observation.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
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
#' #Create the data frame of the latency of the subject to first enter a zone
#' Latency.Zones <- latency_zone(TRAJ2D)
#' #View(Latency.Zones)
latency_zone <- function(list_zones){
  list_zone <- list_zones[[1]]

  ifsum <- list_zone[[base::length(list_zone)]]
  JJ <- base::data.frame(0)

  for (j in 1:(base::length(list_zone)-1)) {
    WW <- base::data.frame(0)
    k <-  list_zone[[j]]

    if(base::length(k) > 0){

      if(base::as.numeric(ifsum[[1]]$frame[1]) != base::as.numeric(k[[1]]$frame[1])){

        time <- base::min(k[[1]]$time) - base::min(ifsum[[1]]$time)
        WW <- base::rbind(WW, time)
        WW
      }else{
        time <- base::c(0)
        WW <- base::rbind(WW, time)
      }
    }else{

      time <- base::c(base::max(ifsum[[base::length(ifsum)]]$time) - base::min(ifsum[[1]]$time))
      WW <- base::rbind(WW, time)
      WW

    }


    BB <- base::sum(WW)
    JJ <- base::cbind(JJ, BB)
    JJ
  }

  JJ <- JJ[,2:base::length(JJ)]

  namezon <- base::vector()
  for (i in 1:base::nrow(list_zones[[2]])) {
    namezon <- base::append(namezon, base::paste("Latency to enter Zone", list_zones[[2]][i,], sep=""))
  }
  base::colnames(JJ) <- namezon
  JJ


}
