#' Total distance traveled
#'
#' @param list_group The output of \link[CartesianBehaviour]{distances_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return The total distance traveled and total distance in each zone for multiple subjects.
#' @export g_dist_zone
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' TRAJ <- list()
#' for(i in 1:10){
#' TRAJ[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                       angularErrorSd = .8, fps = 30)
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
#' rm(Z1, Z2, Z3, Z4, ZN, i)
#'
#' #Perform all the measures
#' TRAJ2D <- g_Allbasics(TRAJ, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#'
#' #Create the data frame of the transitions
#' Group.Distance.Zones <- g_dist_zone(TRAJ2D)
#' #View(Group.Distance.Zones)
g_dist_zone <- function(list_group){
  DGrou <- data.table::rbindlist(base::lapply(list_group, dist_zone), use.names=TRUE)
  DGrou$Animal <- base::c(base::paste("Animal_", 1:base::length(list_group), sep = ""))
  DGrou
}
