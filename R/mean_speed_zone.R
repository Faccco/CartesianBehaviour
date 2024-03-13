#' The mean speed for each zone
#'
#' @param dist_zone The output of \link[CartesianBehaviour]{dist_zone} or \link[CartesianBehaviour]{g_dist_zone}
#' @param time_zones The output of \link[CartesianBehaviour]{times_zone} or \link[CartesianBehaviour]{g_times_zone}
#' @param time_reescale Will divide the time to re-scale it.
#' @param dist_reescale Will divide the distances to re-scale it.
#'
#' @return A data frame with the mean speeds for total trajectory and for zones of interest.
#' @export mean_speed_zone
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' TRAJ <- list()
#'
#' for(i in 1:10){
#' TRAJ[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                           angularErrorSd = .8, fps = 30)
#' }
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
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Perform all the mesures
#'
#'   #For one subject
#'   TRJ <- Allbasics(dado = TRAJ, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3, Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#'    #For multiple subjects
#'
#'    #create an empty list to be filed with the trajectories
#'
#'    TRAJS <- g_Allbasics(TRAJ, Xaxi = "x", Yaxi = "y", time = "time",
#'                          Zones = Zonas, threshold = 3,
#'                          Dist.reg = "point",
#'                          reg = data.frame(x = 0, y = 0))
#'
#'
#' rm(Zonas, TRAJ) #remove extra information
#'
#' #Create the data frame of the mean speed in the zones for
#' #A group of subjects
#' Group.Time.Zones <- g_times_zone(TRAJS)
#' Group.Distance.Zones <- g_dist_zone(TRAJS)
#'
#'G.Mean.Speed.Zones <- mean_speed_zone(Group.Distance.Zones,
#'                                      Group.Time.Zones)
#' #View(G.Mean.Speed.Zones)
#'
#' #Create the data frame of the mean speed in the zones for one subject
#' Time.Zones <- times_zone(TRJ)
#' Distance.Zones <- dist_zone(TRJ)
#'
#' Mean.Speed.Zones <- mean_speed_zone(Distance.Zones, Time.Zones)
#' #View(Mean.Speed.Zones)
mean_speed_zone <- function(dist_zone, time_zones, time_reescale = 1, dist_reescale = 1){
  if("Animal" %in% base::colnames(dist_zone)){

    www <- base::data.frame()
    for(i in  1:base::nrow(dist_zone)){
      AAA <- dist_zone[dist_zone$Animal == base::paste("Animal_", i, sep = ""),]
      AAA <- AAA[,1:(base::ncol(AAA)-1)]
      BBB <- time_zones[time_zones$Animal == base::paste("Animal_", i, sep = ""),]
      BBB <- base::data.frame(base::t(BBB[,1]))
      CCC <- (base::data.frame(AAA)/dist_reescale)/(base::data.frame(BBB)/time_reescale)
      www <- base::rbind(www, CCC)
    }
    namezon <- base::c(base::paste("Mean speed in Zone", 1:(base::ncol(www)-1), sep=""), "Mean speed")
    base::colnames(www) <- namezon
    www$Animal <- base::c(base::paste("Animal_", 1:base::nrow(www), sep = ""))
    www
  }else{
    distzon <- base::data.frame(dist_zone[1,])
    timezon <- base::c(time_zones[,1])
    meanspeed <- (distzon/dist_reescale)/(timezon/time_reescale)
    namezon <- base::c(base::paste("Mean speed in Zone", 1:(base::ncol(distzon)-1), sep=""), "Mean speed")
    base::colnames(meanspeed) <- namezon
    meanspeed
  }
}
