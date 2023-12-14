#' Analisis over time
#'
#' Returns the means in desired time sections for a defined mesure of interest for one subject tracked.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{Allbasics} or the corresponding desired by frame analysis.
#' @param time.ana The time frames to be analyzed as means. "mean" to analyze the total mean (i.e. the mean measure considering each frame).
#' @param type The desired analysis to be done over time. Can be "point", "line", "plane", "distance", "speed", "angle", "meandering", "underspeed".
#' @param time_threshold The time threshold if the analysis is the under speed episodes.
#' @param t.rounding The number of decimals to be considered in the time analysis if you are using a decimal measure of time.
#'
#' @return A data frame with distance the means for each time section to a defined endpoint of interest.
#' @export over_time_ana
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
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
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", frames = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point", reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of
#' #the distance to a point of interest of the subject
#' Distance.Point <- over_time_ana(TRAJ2D, time.ana = 2, type = "point")
#' #View(Distance.Point)
over_time_ana <- function(list_zones, time.ana = "mean", type = "point", time_threshold = 1, t.rounding = 0){
  mode <- base::as.numeric(base::pmatch(type,
                                         base::c("point", "line", "plane", "distance", "speed", "angle", "meandering", "underspeed")))

  list_zone <- list_zones[[1]]
  pt <- list_zone[[base::length(list_zone)]]
  time <- pt[[1]]$time

  if(mode == 1){
    analiVec <- base::as.numeric(pt[[1]]$distPoint)
  }else if(mode == 2){
    analiVec <- base::as.numeric(pt[[1]]$distLine)
  }else if(mode == 3){
    analiVec <- base::as.numeric(pt[[1]]$distPlane)
  }else if(mode == 4){
    analiVec <- base::as.numeric(pt[[1]]$dist)
  }else if(mode == 5){
    analiVec <- base::as.numeric(pt[[1]]$Speeds)
  }else if(mode == 6){
    analiVec <- base::as.numeric(pt[[1]]$Angles)
    }else if(mode == 7){
      analiVec <- base::as.numeric(pt[[1]]$Meandering)
    }else if(mode == 8){
      final <- data.frame()
      j = 0
      while (j < (base::round(base::max(time), digits = t.rounding))){
        analiDF <- pt[[1]]
        max <- j+time.ana
        min <- j
        analiDF$time <- analiDF$time - min(analiDF$time)
        subdf <- analiDF[analiDF$time <= max,]
        subdf <- subdf[subdf$time > j,]

        ABC <- list(list(subdf))
        XX <- under_sepisods(ABC, time_threshold = time_threshold, MOD = "DF")

        final <- base::rbind(final, XX)
        j <- j+time.ana
      }
    }

if(mode != 8){
  analiDF <- base::data.frame(base::cbind(analiVec,time))

  AA <- base::c(0,0)
  if(time.ana == "mean"){

    final <- base::data.frame(base::mean(analiVec))

    if(mode == 1){
      base::colnames(final) <-  base::c("Total mean distance to point")
    }else if(mode == 2){
      base::colnames(final) <-  base::c("Total mean distance to line")
    }else if(mode == 3){
      base::colnames(final) <-  base::c("Total mean distance to plane")
    }else if(mode == 4){
      base::colnames(final) <-  base::c("Mean distance traveled")
    }else if(mode == 5){
      base::colnames(final) <-  base::c("Mean speed")
    }else if(mode == 6){
      base::colnames(final) <-  base::c("Mean turn angle")
    }else if(mode == 7){
      base::colnames(final) <-  base::c("Mean meandering")
    }
    final
  }else{
    i = 0
    while (i < (base::round(base::max(time), digits = t.rounding))){
      max <- i+time.ana
      min <- i
      subdf <- analiDF[analiDF$time<=max,]
      subdf <- subdf[subdf$time>i,]
      if(mode %in% c(4,6)){
      mean <- base::c(base::sum(subdf$analiVec), base::max(subdf$time))
      }else{
        mean <- base::c(base::mean(subdf$analiVec), base::max(subdf$time))
      }
      AA <- base::rbind(AA, mean)
      i <- i+time.ana
    }
    final <- base::data.frame(AA[2:base::nrow(AA),])
  }

    if(mode == 1){
      base::colnames(final) <- base::c("Mean Point Distance", "Time")
    }else if(mode == 2){
      base::colnames(final) <- base::c("Mean Line Distance", "Time")
    }else if(mode == 3){
      base::colnames(final) <- base::c("Mean Plane Distance", "Time")
    }else if(mode == 4){
      base::colnames(final) <- base::c("Total Traveled Distance", "Time")
    }else if(mode == 5){
      base::colnames(final) <- base::c("Mean Speed", "Time")
    }else if(mode == 6){
      base::colnames(final) <- base::c("Total Turn Angle", "Time")
    }else if(mode == 7){
      base::colnames(final) <- base::c("Mean Meandering", "Time")
    }
  }else if(mode == 8){
    if(time.ana == "mean"){
      stop("Just use under_sepisods function", call. = FALSE)
    }
      base::colnames(final) <- base::c("Total Time Under", "Number of Episodes", "Mean Time under Speed")
    }

    base::rownames(final) <- base::c((1:base::nrow(final))*time.ana)
    final
  }
