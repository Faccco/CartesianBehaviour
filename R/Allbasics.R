#' Perform all basic measurements by frame
#'
#' Include all analysis by-frame necessary to calculate the means and by-time measures for one sample.
#'
#' @param dado A Data Frame containing tracking information, including X, Y and, optionally, Z coordinates.
#' @param frames Column name in dados representing the frame number or time stamp.
#' @param threshold A defined speed to be tested. Measure of speed depends on the input measure of time and distances in XYZ coordinates.
#' @param dadoz A second data frame that contains the z observations and other identifiers.
#' @param Xaxi Column name in the dado data frame that includes the X coordinates .
#' @param Yaxi Column name in the dado data frame that includes the Y coordinates.
#' @param Zaxi Column name in dadoz, or a vector of the type double  containing the z observations.
#' @param id Identifier of the subject.
#' @param fps The frame rate or another value to convert the time marker into a desired one (the value of time will be divided by this).
#' @param id_col Column name in dados that contain the unique subject identifier.
#' @param Dist.reg Logical, if TRUE will add the distance to a point of interest in the data frame.
#' @param n.zonesd2 Number of zones for 2D analysis.
#' @param n.zonesd3 Number of zones for 3D analysis.
#' @param faceZ The complementary dimension to Z, which will be used to draw the zones. Input 1 for x, 2 for y.
#' @param maxX The maximum coordinate in the X-axis defining the arena.
#' @param minX The minimum coordinate in the X-axis defining the arena.
#' @param maxY The maximum coordinate in the Y-axis defining the arena.
#' @param minY The minimum coordinate in the Y-axis defining the arena.
#' @param maxZ If considered, the maximum coordinate in the Z-axis defining the arena
#' @param minZ If considered, the minimum coordinate in the Z-axis defining the arena
#' @param npts Number of vertices that define the polygon representing the zone of interest.
#' @param Circ_Arena A logical indicating whether do draw a circular boundary to position the point . FALSE by default.
#' @param Zones A data frame crated by \link[CartesianBehaviour]{Zones_int} containing the zones of interest. NA by  default to create the zones within the analysis.
#' @param reg A data frame with the x,y (and optionally z) axis. "NOT" by default to pin the point/line/plane manually in the plot tab.
#' @param time The column name in dados representing the time stamp in integers.
#' @param deg Convert into degrees. If FALSE will generate the angles in radians.
#' @param bodyarch If you have any kind of of 3 point tracking select a type of data input. "unique" by default. See more in \link[CartesianBehaviour]{expdados} description for Gtype.
#' @param center The name of the center point input.
#' @param head The name of the head columns or a data frame containing it.
#' @param tail The name of the tail columns or a data frame containing it.
#' @param sidesum Define if the interest is the absolute angle for each side of the arc of the three points or the minimum and maximum angles. TRUE for minimum and maximum, FALSE by default.
#'
#' @return A list containing the data frames witch the subject was inside the zone of interest and all to perform all the 1 subject measures of this package.
#' @export Allbasics
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
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
#' #Perform all the measures
#' TRAJ2D <- Allbasics(dado = Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3, Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#' #View(TRAJ2D)
Allbasics <- function(dado, dadoz = NULL, Xaxi = "x_cm", Yaxi = "y_cm", Zaxi = NA, time = NA, frames = NA, fps = 30,
                      id_col = NULL, id = 1, Zones = NA, n.zonesd2, n.zonesd3 = 0, faceZ = 0,
                      maxX = NA, minX = NA, maxY = NA, minY = NA, maxZ = NA, minZ = NA, npts = 4, deg = T,  Circ_Arena = F, threshold = 5,
                      Dist.reg = NA, reg = "NOT", bodyarch = NA, center = NA, head = NA, tail = NA, sidesum = F){
  if(T %in% base::is.na(Zones)){
    Zones <- CartesianBehaviour::Zones_int(n.zonesd2, n.zonesd3, faceZ, maxX, minX, maxY, minY, maxZ, minZ, npts, Circ_Arena)
  }

  if(faceZ == 1){
    zplane <- "zx"
  }else if(faceZ == 2){
    zplane <- "yz"
  }else{
    zplane <- NA
  }

  if(!(is.na(bodyarch))){
    dados <- CartesianBehaviour::expdados(dado = dado, dadoz = dadoz, Xaxi = Xaxi, Yaxi = Yaxi,
    Zaxi = Zaxi, frames = frames, time = time, id = id, id_col = id_col,
    fps = fps, Gtype = bodyarch, center = center, head = head, tail = tail)

    dados <- body_arch_by_frame(list_zones = dados, sidesum = sidesum)

    if(base::grepl("2", bodyarch)){

      Xaxi <- paste(Xaxi, center, sep = "_")
      Yaxi <- paste(Yaxi, center, sep = "_")

    }

  }else{

    dados <- CartesianBehaviour::expdados(dado = dado, dadoz = dadoz, Xaxi = Xaxi, Yaxi = Yaxi,
    Zaxi = Zaxi, time = time, frames = frames, id = id, id_col = id_col,
    fps = fps, Gtype = "unique", center = center, head = head, tail = tail)

  }

  listzones <- CartesianBehaviour::list_zones(dados, Zones, zplane)
  listzones <- CartesianBehaviour::distances_by_frame(listzones)
  listzones <- CartesianBehaviour::speeds_by_frame(listzones)
  listzones <- CartesianBehaviour::threshold_speed_by_frame(listzones, threshold)

  if(base::is.na(zplane)){

    listzones <- CartesianBehaviour::angles_by_frame(listzones, deg = deg)
    listzones <- CartesianBehaviour::meandering_by_frame(listzones)

  }

  if("point" %in% Dist.reg){

    listzones <- CartesianBehaviour::reg_dist_by_frame(list_sample = listzones, Dist.reg = "point", reg, faceZ, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena)
  }
  if("line" %in% Dist.reg){

    listzones <- CartesianBehaviour::reg_dist_by_frame(list_sample = listzones, Dist.reg = "line", reg, faceZ, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena)
  }
  if("plane" %in% Dist.reg){

    listzones <- CartesianBehaviour::reg_dist_by_frame(list_sample = listzones, Dist.reg = "plane", reg, faceZ, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena)
  }

  listzones
}
