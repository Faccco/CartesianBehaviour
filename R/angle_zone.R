#' The absolute turn angle
#'
#' The absolute turn angle of one subject for the complete trajectory and the zones of interest.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{angles_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return A data frame with the absolute turn angle for the total trajectory and by zones.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
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
#' rm(Z1, Z2, Z3, Z4, ZN, i)
#'
#' #Perform all the measures
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the absolut turn angles
#' Angle.Zones <- angle_zone(TRAJ2D)
#' #View(Angle.Zones)
angle_zone <- function(list_zones){
  list_zone <- list_zones[[1]]
  AA <- base::data.frame(0)
  for (i in 1:base::length(list_zone)) {

    zoneangle <- list_zone[[i]]
    BB <- base::data.frame()
    if(i == base::length(list_zone)){
      zoneangle <- base::list(list_zone[[i]])
    }

    if(base::length(zoneangle) > 0){
      for (j in 1:base::length(zoneangle)) {
        Angleszon <- base::sum(base::abs(base::as.numeric(zoneangle[[j]]$Angles)), na.rm = T)
        BB <- base::rbind(BB, base::as.numeric(Angleszon))
      }
    }else{

      BB <- base::rbind(BB, 0)

    }
    sumangles <- base::sum(base::abs(BB[[1]]))
    AA <- base::cbind(AA, sumangles)
    AA
  }
  AA <- AA[,2:(base::length(list_zone)+1)]
  namezon <- base::c(base::paste("Absolut turn angle in Zone", 1:(base::length(list_zone)-1), sep=""), "Total")
  base::colnames(AA) <- namezon
  AA
}
