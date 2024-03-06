#' By frame distances to a Point/Line/Plane for ultiple subjects
#'
#' Add the \link[CartesianBehaviour]{list_zones} output with the distances to a defined point/line/plane in each frame, necessary to perform \link[CartesianBehaviour]{over_time_ana}.
#'
#' @param listzone The output of \link[CartesianBehaviour]{g_list_zones}.
#' @param Dist.reg The definition to evaluate the distance to a "point", "line" or "plane". A points define by a singular location a line by two point and a plane by three points. In 3D you need to point it in XY and (X/Y)Z axis, so point is 2 times, line is 4 and plane is 6.
#' @param faceZ An integer number indicating the dimension to consider for distance calculation (1 for 'x' or 2 for 'y').
#' @param minX The minimum coordinate in the X-axis defining the arena.
#' @param maxX The maximum coordinate in the X-axis defining the arena.
#' @param minY The minimum coordinate in the Y-axis defining the arena .
#' @param maxY The maximum coordinate in the Y-axis defining the arena.
#' @param minZ The minimum coordinate in the Z-axis defining the arena .
#' @param maxZ The maximum coordinate in the Z-axis defining the arena.
#' @param Circ_Arena A logical indicating whether do draw a circular boundary to position the point  of interest to be considered. FALSE by default.
#' @param reg A data frame with the coordinates to a point/line/plane. "NOT" by default to manually pin the point(s) of the point/line/plane.
#'
#' @return A list with the prior data and a column with the distance to a point/line/plane
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- list()
#' for(i in 1:5){
#' Traj[[i]] <- TrajGenerate(5000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'}
#' #Export the trajectory.
#' TrajEXP <- g_expdados(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                    id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Normally by Zones_in() but in this case by code directly
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
#'
#' #Add the distance to a point of interest by each frame this example
#' #pre-define the point but you can turn the Dist.reg parameter to
#' #"line" or "plane" and reg to "NOT", set the arena measures and
#' #manually pin the  point in the Plot tab
#' TRAJ2D <- g_reg_dist_by_frame(TRAJ2D, Dist.reg = "point",
#'                             reg = data.frame(x = 0, y = 0))
#' #View(TRAJ2D[[1]][["Total"]][[1]])
g_reg_dist_by_frame <- function(listzone, Dist.reg = "plan", reg = "NOT", faceZ = 0, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena = F){
 for (i in 1:base::length(listzone)) {
   listzones <- reg_dist_by_frame(list_sample = listzone[[i]], Dist.reg, reg, faceZ, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena)
   listzone[[i]] <- listzones
 }
  listzone
}
