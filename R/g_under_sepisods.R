#' Group episodes & time under a defined speed and above a time
#'
#' @param list_group The output of \link[CartesianBehaviour]{threshold_speed_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#' @param time_threshold The minimum time for an episode to be counted.
#'
#' @return The time the episodes lasted and the number of episodes completely in the zones and for total track for multiple subjects. Note that even if the zones are tangents the sum of that can be greater or lesser than the number of episodes in the total trajectory.
#' @export g_under_sepisods
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' TRAJ <- list()
#' for(i in 1:10){
#' TRAJ[[i]] <- TrajGenerate(5000, random = TRUE, stepLength = .01,
#'                           angularErrorSd = .8, fps = 30)
#' }
#'
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
#' #create an empty list to be filed with the trajectories
#' TRAJS2D <- list()
#' for (i in 1:length(TRAJ)) {
#'
#'   TRAJS <- Allbasics(TRAJ[[i]], Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'   Tj <- list(TRAJS)
#'   TRAJS2D <- append(TRAJS2D, Tj)
#'
#'   }
#'   rm(Tj, Zonas, TRAJS, i, TRAJ) #remove extra information
#'
#'   #Create the data frame of
#'   #the the episodes under a defined speed of multiple subjects
#'   Group.Under.Speed.Episods <- g_under_sepisods(TRAJS2D)
#'   #View(Group.Under.Speed.Episods)
g_under_sepisods <- function(list_group, time_threshold = 5){
  list_groups <- list_group[[1]]
  list_groupa <- list()
  for (i in 1:base::length(list_group)) {
    list_groupi <-  list_group[[i]]
    list_groupa[[i]] <- under_sepisods(list_groupi, time_threshold = time_threshold)
  }

  UsGrou <- data.table::rbindlist(list_groupa, use.names=TRUE)
  UsGrou$Animal <- base::c(base::rep(base::paste("Animal_", 1:base::length(list_group), sep = ""), each = base::length(list_groups[[1]])))
  UsGrou$Zone <- base::c(base::rep(base::c(base::paste("Zone_", 1:(base::length(list_groups[[1]])-1), sep = ""), "Total"), times = base::length(list_group)))
  UsGrou
}
