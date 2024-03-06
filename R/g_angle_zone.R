#' The absolute turn angle for a list of subjects
#'
#' @param list_group A list of the outputs of \link[CartesianBehaviour]{angles_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return The absolute turn angle of multiple subjects for the complete trajectory and the zones of interest.
#' @export g_angle_zone
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#'
#' TRAJ <- list()
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
#' ##create an empty list to be filed with the trajectories
#' TRAJS2D <- list()
#' for (i in 1:length(TRAJ)) {
#'
#'   TRAJS <- Allbasics(TRAJ[[i]], dadoz = NULL, Xaxi = "x", Yaxi = "y",
#'                      time = "time", Zones = Zonas, threshold = 3,
#'                      Dist.reg = "point", reg = data.frame(x = 0, y = 0))
#'   Tj <- list(TRAJS)
#'   TRAJS2D <- append(TRAJS2D, Tj)
#'
#'   }
#'   rm(Tj, Zonas, TRAJS, i, TRAJ) #remove extra information
#'
#'   #Create the data frame of the transitions
#'   Group.Angles.Zones <- g_angle_zone(TRAJS2D)
#'   #View(Group.Angles.Zones)
g_angle_zone <- function(list_group){
  AnGrou <- data.table::rbindlist(lapply(list_group, angle_zone), use.names=TRUE)
  AnGrou$Animal <- base::c(base::paste("Animal_", 1:base::length(list_group), sep = ""))
  AnGrou
}
