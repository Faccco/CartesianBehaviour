#' Sample speed by each frame
#'
#'Add the \link[CartesianBehaviour]{g_distances_by_frame} output or multiple \link[CartesianBehaviour]{distances_by_frame} outputs listed with the Speeds traveled each frame, necessary to perform: \link[CartesianBehaviour]{threshold_speed_by_frame}, \link[CartesianBehaviour]{mean_speed_zone}, \link[CartesianBehaviour]{MMspeed} and \link[CartesianBehaviour]{g_MMspeed}. Note that you must perform \link[CartesianBehaviour]{distances_by_frame} to use this function properly.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{g_distances_by_frame} or multiple \link[CartesianBehaviour]{distances_by_frame} outputs listed.
#'
#' @return A list containing lists of splitted trajectory data frames with the speeds by each frame.
#' @export
#'
#' @examples #Create an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' Traj <- list()
#' for(i in 1:10){
#' Traj[[i]] <- TrajGenerate(5000, random = TRUE, stepLength = .01,
#'                           angularErrorSd = .8, fps = 30)
#'
#'}
#'
#' #Export the trajectory.
#' TrajEXP <- g_expdados(Traj, Xaxi = "x", Yaxi = "y",
#'                       time = "time", id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn but setting Zones = NA,
#' #but in this case is made by code directly)
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN, i, Traj)
#'
#' #Split by the zones
#' TRAJ2D <- g_list_zones(TrajEXP, Zones = Zonas)
#'
#' #Add the distances by each frame
#' TRAJ2D <- g_distances_by_frame(TRAJ2D)
#'
#' #Add the speeds by each frame
#' TRAJ2D <- g_speeds_by_frame(TRAJ2D)
#' #View(TRAJ2D[[1]][[1]][["Total"]][[1]])
g_speeds_by_frame <- function(list_zones){
  for (i in 1:base::length(list_zones)) {
    list_zone <- list_zones[[i]]
    list_zone <- speeds_by_frame(list_zone)
    list_zones[[i]] <- list_zone
  }
  list_zones
}
