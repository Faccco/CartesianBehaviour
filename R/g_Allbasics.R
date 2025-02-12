#' All basic measurements by frame for multiple data frames
#'
#' @param list_subjects A list of data frames containing tracking information, including X, Y and, optionally, Z coordinates.
#' @param frames Column name in list_subjects representing the frame number.
#' @param threshold A defined speed to be tested. Measure of speed depends on the input measure of time and distances in XYZ coordinates.
#' @param list_dadoz A second list of data frames that contains the z observations and other identifiers. Must be the same length of list_subjects.
#' @param Xaxi Column name in the dado data frame that includes the X coordinates .
#' @param Yaxi Column name in the dado data frame that includes the Y coordinates.
#' @param Zaxi Column name in list_dadoz, or a vector of the type double  containing the z observations.
#' @param id Identifier of the subject.
#' @param fps The frame rate or another value to convert the time marker into a desired one (the value of time will be divided by this).
#' @param id_col Column name in list_subjects that contain the unique subject identifier.
#' @param Dist.reg Vector, can be the distance to a "point", "line" or "plane". NA for no measure.
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
#' @param reg A data frame with the x,y (and optionally z) axis. NA by default to pin the point manually in the plot tab.
#' @param time The column name in list_subjects representing the time stamp.
#'
#' @return A list of data frames with the splitted trajectories and speed, distance, angles, marker to speed, meandering by each frame.
#' @export g_Allbasics
#'
#' @examples#Creating an trajectory using the function from the package trajr
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
#' #Create the zones of interest
#' #Can be manually drawn but setting Zones = NA,
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
#' TRAJ2D <- g_Allbasics(list_subjects = Traj, Xaxi = "x",
#'                       Yaxi = "y", time = "time", Zones = Zonas,
#'                       threshold = 3, Dist.reg = "point",
#'                       reg = data.frame(x = 0, y = 0))
g_Allbasics <- function(list_subjects, list_dadoz = NULL, Xaxi = "x_cm", Yaxi = "y_cm", Zaxi = NA, time = NA, frames = NA, fps = 30,
                        id_col = NULL, id = 1, Zones = NA, n.zonesd2, n.zonesd3 = 0, faceZ = 0,
                        maxX = NA, minX = NA, maxY = NA, minY = NA, maxZ = NA, minZ = NA, npts = 4, Circ_Arena = F, threshold = 5,
                        Dist.reg = F, reg = NA){
  if(base::is.null(list_dadoz)){
    list_subjects <- base::lapply(list_subjects, FUN = Allbasics, dadoz = list_dadoz, Xaxi = Xaxi, Yaxi = Yaxi, Zaxi = Zaxi, time = time, frames = frames,
                                  fps = fps, id_col = id_col, id = id, Zones = Zones, n.zonesd2 = n.zonesd2, n.zonesd3 = n.zonesd3,
                                  faceZ = faceZ, maxX = maxX, minX = minX, maxY = maxY, minY = minY, maxZ = maxZ, minZ = minZ,
                                  npts = npts, Circ_Arena = Circ_Arena, threshold = threshold, Dist.reg = Dist.reg, reg = reg)
    list_subjects
  }else if(base::typeof(list_dadoz) == "list"){
    if(base::length(list_subjects) == base::length(list_dadoz)){
      for (i in 1:base::length(list_dadoz)) {

        list_subjectsz <- Allbasics(dado = list_subjects[[i]], dadoz = list_dadoz[[i]], Xaxi = Xaxi, Yaxi = Yaxi, Zaxi = Zaxi, time = time, frames = frames,
                                    fps = fps, id_col = id_col, id = id, Zones = Zones, n.zonesd2 = n.zonesd2, n.zonesd3 = n.zonesd3,
                                    faceZ = faceZ, maxX = maxX, minX = minX, maxY = maxY, minY = minY, maxZ = maxZ, minZ = minZ,
                                    npts = npts, Circ_Arena = Circ_Arena, threshold = threshold, Dist.reg = Dist.reg, reg = reg)

        list_subjects[[i]] <- list_subjectsz

      }
      list_subjects
    }
  }else{stop("Incorrect input. The dadoz and list_subjects must have the same length.
              Check if you really have dadoz observations or else set dadoz = NULL",
             call. = FALSE)}
}
