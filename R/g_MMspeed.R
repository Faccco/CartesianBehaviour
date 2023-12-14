#' The maximum and minimum speeds for a list of subjects
#'
#' @param list_group A list with the outputs of  \link[CartesianBehaviour]{speeds_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return The maximum and minimum speeds for the zones of interest and total trajectory for multiple subject.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
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
#' rm(Z1, Z2, Z3, Z4, ZN, i)
#'
#' #Perform all the measures
#' TRAJS2D <- g_Allbasics(TRAJ, Xaxi = "x",
#'                      Yaxi = "y", frames = "time", Zones = Zonas,
#'                      threshold = 3, Dist.reg = "point",
#'                      reg = data.frame(x = 0, y = 0))
#'
#'  #Create the data frame of the
#'  #maximum and minimum speeds of multiple subjects
#'  Group.MaxMinSpeed <- g_MMspeed(TRAJS2D)
#'  #View(Group.MaxMinSpeed)
g_MMspeed <- function(list_group){
  list_groups <- list_group[[1]]
  MMGrou <- data.table::rbindlist(base::lapply(list_group, MMspeed), use.names=TRUE)
  MMGrou$Animal <- base::c(base::rep(base::paste("Animal_", 1:base::length(list_group), sep = ""), each = base::length(list_groups[[1]])))
  MMGrou$Zone <- base::c(base::rep(base::c(base::paste("Zone_", 1:(base::length(list_groups[[1]])-1), sep = ""), "Total"), times = base::length(list_group)))
  MMGrou
}
