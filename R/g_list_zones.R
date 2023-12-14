#' Split the trajectory in a list into zones of interest
#'
#' This function is just a wrap around \link[CartesianBehaviour]{list_zones} with lapply. Check \link[CartesianBehaviour]{list_zones} for more info.
#'
#' @param list_subjects A list of data frames crated by \link[CartesianBehaviour]{g_expdados} or multiple \link[CartesianBehaviour]{expdados} listed containing the positions and time of an subject.
#' @param Zones A data frame crated by \link[CartesianBehaviour]{Zones_int} containing the zones of interest.
#' @param Zplane The complementary axis used in \link[CartesianBehaviour]{Zones_int}. Input can be "xz", "zx", "yz" or "zy".
#' @param remove2D To remove the 2D zones from the final data. FALSE by default.
#'
#' @return A list containing lists of splitted trajectory data frames.
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
#'                       frames = "time", id = 1, fps = 30)
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
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Split by the zones
#'
#' TRAJ2D <- g_list_zones(TrajEXP, Zones = Zonas)
#' #View(TRAJ2D)
g_list_zones <- function(list_subjects, Zones, Zplane = NA, remove2D = F){
  list_zones <- base::lapply(list_subjects, list_zones, Zones = Zones, Zplane = Zplane, remove2D = remove2D)
  list_zones
}
