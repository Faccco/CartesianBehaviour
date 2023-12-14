#' Number of transitions to enter a zone for a list of samples
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones} for a list of samples.
#'
#' @return A data frame containing the number of entrances in a defined zone of interest for a group of samples.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' TRAJ <- list()
#' for(i in 1:10){
#' TRAJ[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
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
#' #Perform all the measures
#' TRAJS2D <- g_Allbasics(TRAJ, Xaxi = "x", Yaxi = "y", frames = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#'  #Create the data frame of the transitions of multiple subjects
#'  Group.Transitions <- g_transitions(TRAJS2D)
#'  #View(Group.Transitions)
g_transitions <- function(list_zones){
  TrGrou <- data.table::rbindlist(base::lapply(list_zones, transitions), use.names=TRUE)
  TrGrou$Animal <- base::c(base::paste("Animal_", 1:base::length(list_zones), sep = ""))
  TrGrou
}
