#' The maximum and minimum speeds
#'
#' @param list_zones The output of  \link[CartesianBehaviour]{speeds_by_frame} or \link[CartesianBehaviour]{Allbasics}.
#'
#' @return The maximum and minimum speeds for the zones of interest and total trajectory for one subject.
#' @export MMspeed
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
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
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the maximum and minimum speeds of the subject
#' MaxMinSpeed <- MMspeed(TRAJ2D)
#' #View(MaxMinSpeed)
MMspeed <- function(list_zones){
  list_zone <- list_zones[[1]]
  AA <- base::data.frame()
  for (i in 1:base::length(list_zone)) {

      Ani <- list_zone[[i]]

    if(base::length(Ani) > 0){
      testspeed <- base::c(dplyr::bind_rows(Ani)$Speeds)
      if(base::length(testspeed) > 1 && FALSE %in% base::is.na(testspeed)){
        speedmax <- base::max(testspeed, na.rm = T)
        speedmin <- base::min(testspeed, na.rm = T)
        BB <- base::cbind(speedmax, speedmin)
      }else{
        speedmax <- 0
        speedmin <- 0
        BB <- base::cbind(speedmax, speedmin)
      }
    }else{
      base::print(base::paste("Zone ", i, " not explored"))
      speedmax <- NA
      speedmin <- NA
      BB <- base::cbind(speedmax, speedmin)
    }

    AA <- base::rbind(AA, BB)
    AA

  }

  namezon <- base::vector()
  for (i in 1:base::nrow(list_zones[[2]])) {
    namezon <- base::append(namezon, base::paste("Zone", list_zones[[2]][i,], sep=""))
  }
  namezon <- base::append(namezon, "Total")
  base::row.names(AA) <- namezon
  base::colnames(AA) <- base::c("Max speed in zone", "Minimun speed in zone")
  AA

}
