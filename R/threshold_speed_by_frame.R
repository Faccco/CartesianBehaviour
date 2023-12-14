#' Trace the frames above or below a defined speed
#'
#' Add the \link[CartesianBehaviour]{speeds_by_frame} output with a column which 0 stand for below the defined speed and 1 above, necessary to perform: \link[CartesianBehaviour]{under_sepisods} and \link[CartesianBehaviour]{g_under_sepisods}.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{speeds_by_frame}.
#' @param threshold A defined speed to be tested. Measure of speed depends of the input measure of time and distances in XYZ.
#'
#' @return A list of splitted data frames added with a marker to a speed limit by each frame.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                       angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- expdados(Traj, Xaxi = "x", Yaxi = "y", frames = "time",
#'                     id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #(normally by Zones_in() but in this case by code directly)
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
#'
#' #Add the speeds marker column by each frame by
#' #defining the considered speed
#' TRAJ2D <- threshold_speed_by_frame(TRAJ2D, threshold = 3)
#' #View(TRAJ2D[[1]][["Total"]][[1]])
threshold_speed_by_frame <- function(list_zones, threshold = 5){
  list_zone <- list_zones[[1]]
  for (j in 1:base::length(list_zone)) {

    www <- list_zone[[j]]

    if(length(www) > 0){
      for (i in 1:base::length(www)) {

        www[[i]]$Abovespeed <-  base::c(data.table::fifelse(www[[i]]$Speeds >= threshold, 1, 0, na = 0))
        www

      }
    }else{
      www
    }
    list_zone[[j]] <- www
    list_zone
  }

  list_zones[[1]] <- list_zone
  list_zones

}
