#' Sample distance by each frame
#'
#'Add the \link[CartesianBehaviour]{g_list_zones} or multiple \link[CartesianBehaviour]{list_zones} outputs listed with the distances traveled each frame, necessary to perform: \link[CartesianBehaviour]{speeds_by_frame}, \link[CartesianBehaviour]{dist_zone} and \link[CartesianBehaviour]{g_dist_zone}.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{g_list_zones} or multiple \link[CartesianBehaviour]{list_zones} outputs listed.
#'
#' @return A list containing lists of splitted trajectory data frames with the distances by each frame.
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
#' TrajEXP <- g_standardized_trj(Traj, Xaxi = "x", Yaxi = "y",
#'                       time = "time", id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn but setting Zones = NA,
#' #but in this case is made by code directly
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
#' #View(TRAJ2D[[1]][[1]][["Total"]][[1]])
g_distances_by_frame <- function(list_zones){
  for (i in 1:base::length(list_zones)) {
    list_zone <- list_zones[[i]]
    list_zone <- distances_by_frame(list_zone)
    list_zones[[i]] <- list_zone
  }
  list_zones
}
