#' Speed by each frame
#'
#'Add the \link[CartesianBehaviour]{distances_by_frame} output with the Speeds traveled each frame, necessary to perform: \link[CartesianBehaviour]{threshold_speed_by_frame}, \link[CartesianBehaviour]{mean_speed_zone}, \link[CartesianBehaviour]{MMspeed}. Note that you must perform \link[CartesianBehaviour]{distances_by_frame} to use this function properly.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones}.
#'
#' @return A list containing a splitted trajectory data frames with the speeds by each frame.
#' @export speeds_by_frame
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- expdados(Traj, Xaxi = "x", Yaxi = "y", frames = "time",
#'                     id = 1, fps = 30)
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
#' #Add the distances by each frame
#' TRAJ2D <- distances_by_frame(TRAJ2D)
#'
#' #Add the speeds by each frame
#' TRAJ2D <- speeds_by_frame(TRAJ2D)
#' #View(TRAJ2D[[1]][["Total"]][[1]])
speeds_by_frame <- function(list_zones){
  list_zone <- list_zones[[1]]
  for (i in 1:base::length(list_zone)) {

    baba <- list_zone[[i]]

    if(base::length(baba) > 0){
      for (j in 1:base::length(baba)) {

        Speeds <- baba[[j]]$dist/(baba[[j]]$time - dplyr::lag(baba[[j]]$time))

        baba[[j]]$Speeds <- Speeds
        baba
      }
    }else {
      baba
    }
    list_zone[[i]] <- baba
    list_zone
  }
  list_zones[[1]] <- list_zone
  list_zones
}
