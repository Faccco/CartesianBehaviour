#' Trajectory angle variation by each frame
#'
#' Add the \link[CartesianBehaviour]{list_zones} output with the angle variation by each frame, necessary to calculate: \link[CartesianBehaviour]{meandering_by_frame}, \link[CartesianBehaviour]{Allbasics}, \link[CartesianBehaviour]{meandering_zone}, \link[CartesianBehaviour]{angle_zone} and \link[CartesianBehaviour]{g_angle_zone}.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones}.
#' @param deg Convert into degrees. If FALSE will generate the angles in radians.
#'
#' @return  A list of splitted data frames added with the turn angles by each frame.
#' @export angles_by_frame
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- expdados(Traj, Xaxi = "x", Yaxi = "y",
#'                      time = "time",id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Normally by Zones_in() but in this case by code directly
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Split by the zones
#'
#' TRAJ2D <- list_zones(TrajEXP, Zones = Zonas)
#'
#' #Add the turn angles by each frame
#' TRAJ2D <- angles_by_frame(TRAJ2D)
angles_by_frame <- function(list_zones, deg = TRUE){
  list_zone <- list_zones[[1]]
  for (i in 1:base::length(list_zone)) {

    baba <- list_zone[[i]]

    if(base::length(baba) > 0){
      for (j in 1:base::length(baba)) {

        Angles <- base::atan2((baba[[j]]$y - dplyr::lag(baba[[j]]$y)), baba[[j]]$x - dplyr::lag(baba[[j]]$x))

        if(deg){
          Angles <- Angles * 57.2958
        }

        Angles <- base::abs(dplyr::lead(Angles)) - base::abs(Angles)

        baba[[j]]$Angles <- Angles
        baba
      }
    }else{
      baba
    }
    list_zone[[i]] <- baba
    list_zone
  }
  list_zones[[1]] <- list_zone
  list_zones
}
