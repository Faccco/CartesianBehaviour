#' Sample analisis over time
#'
#' @param list_group The output of \link[CartesianBehaviour]{Allbasics} or the corresponding desired by frame analysis.
#' @param time.ana The time frames to be analyzed as means. "mean" to analyze the total mean (i.e. the mean measure considering each frame).
#' @param type The desired analysis to be done over time. Can be "point", "line", "plane", "distance", "speed", "angle", "meandering", "underspeed".
#' @param time_threshold The time threshold if the analysis is the under speed episodes.
#' @param t.rounding The number of decimals to be considered in the time analysis if you are using a decimal measure of time.
#' @param time.length The maximum time to be analysed
#'
#' @return A data frame with distance the means for each time section to a defined endpoint of interest.
#' @export g_over_time_ana
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- list()
#' for(i in 1:5){
#' Traj[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)}
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
#' TRAJ2D <- g_Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point", reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of
#' #the distance to a point of interest of the subject
#' Distance.Point <- g_over_time_ana(TRAJ2D, time.ana = 2, type = "point")
#' #View(Distance.Point)
g_over_time_ana <- function(list_group, time.ana = "mean", type = "point", time_threshold = 1, time.length = NA, t.rounding = 0){
  list_groups <- list_group[[1]]
  OVtime <- over_time_ana(list_zones = list_group[[1]], time.ana = time.ana, type = type,
                          time_threshold = time_threshold, t.rounding = t.rounding,
                          time.length = time.length)
  OVtm <- base::data.frame(OVtime[,1])
  for (i in 2:base::length(list_group)) {
  OVtime <- over_time_ana(list_group[[i]], time.ana = time.ana, type = type,
                          time_threshold = time_threshold, t.rounding = t.rounding,
                          time.length = time.length)
  OVtm <- base::cbind(OVtm, OVtime[,1])
  OVtm <- base::data.frame(OVtm)
}

  colnames(OVtm) <- base::c(base::paste("Animal_", 1:base::length(list_group),
                                                 sep = ""))
  OVtm$time <- OVtime[,2]
  OVtm
}
