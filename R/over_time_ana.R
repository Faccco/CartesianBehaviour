#' Analisis over time
#'
#' Returns the means in desired time sections for a defined mesure of interest for one subject tracked.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{Allbasics} or the corresponding desired by frame analysis.
#' @param time.ana The time frames to be analyzed as means. "mean" to analyze the total mean (i.e. the mean measure considering each frame).
#' @param type The desired analysis to be done over time. Can be "point", "line", "plane", "distance", "speed", "angle", "meandering", "underspeed".
#' @param time_threshold The time threshold if the analysis is the under speed episodes.
#' @param t.rounding The number of decimals to be considered in the time analysis if you are using a decimal measure of time.
#' @param time.length The maximum time to be analysed
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
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point", reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of
#' #the distance to a point of interest of the subject
#' Distance.Point <- over_time_ana(TRAJ2D, time.ana = 2, type = "point")
#' #View(Distance.Point)
over_time_ana <- function(list_zones, time.ana = "mean", time.length = 10, type = "point", time_threshold = 1, t.rounding = 0){
  mode <- base::as.numeric(base::pmatch(type,
                                         base::c("point", "line", "plane", "distance", "speed", "angle", "meandering", "underspeed", "arch")))

  list_zone <- list_zones[[1]]
  pt <- list_zone[[base::length(list_zone)]]

  if(base::is.na(time.length)){
    time <- pt[[1]]$times
  }else{
    time <- pt[[1]]$times[pt[[1]]$times < time.length]
    if(base::max(time, na.rm = T) < time.length){
      time <- base::append(time, time.length)
      }
    if(time.length %in% pt[[1]]$times){
      lastime <- base::c(base::rep(NA, times = 4), time.length, base::rep(NA, times = (base::ncol(pt[[1]]) - 5)))
      pt[[1]] <- base::rbind(pt[[1]][pt[[1]]$times < time.length,], lastime, pt[[1]][pt[[1]]$times > time.length,])
    }
  }

  if(mode == 1){
    analiVec <- base::as.numeric(pt[[1]]$distPoint[pt[[1]]$times < time.length])
  }else if(mode == 2){
    analiVec <- base::as.numeric(pt[[1]]$distLine[pt[[1]]$times < time.length])
  }else if(mode == 3){
    analiVec <- base::as.numeric(pt[[1]]$distPlane[pt[[1]]$times < time.length])
  }else if(mode == 4){
    analiVec <- base::as.numeric(pt[[1]]$dist[pt[[1]]$times < time.length])
  }else if(mode == 5){
    analiVec <- base::as.numeric(pt[[1]]$Speeds[pt[[1]]$times < time.length])
  }else if(mode == 6){
    analiVec <- base::as.numeric(pt[[1]]$Angles[pt[[1]]$times < time.length])
  }else if(mode == 7){
    analiVec <- base::as.numeric(pt[[1]]$Meandering[pt[[1]]$times < time.length])
  }else if(mode == 9){
    analiVec <- base::as.numeric(pt[[1]]$arch1[pt[[1]]$times < time.length])
    analiVec <- base::data.frame(analiVec, (base::as.numeric(pt[[1]]$arch2[pt[[1]]$times < time.length])))
  }else if(mode == 8){
    final <- data.frame()
    j = 0
    while (j < time.length){
      analiDF <- pt[[1]]
      max <- j+time.ana
      min <- j
      analiDF$time <- analiDF$time - base::min(analiDF$time, na.rm = T)
      subdf <- analiDF[analiDF$time <= max,]
      subdf <- subdf[subdf$time > j,]

      ABC <- list(list(subdf))
      XX <- under_sepisods(ABC, time_threshold = time_threshold, MOD = "DF")

      final <- base::rbind(final, XX)
      j <- j+time.ana
    }
  }

if(mode != 8){
  if(mode == 9){
    comp <- base::max(base::nrow(analiVec), base::length(time))
    analiVec <- analiVec[1:comp,]
    base::length(time) <- comp
    analiDF <- base::data.frame(base::cbind(analiVec,time))
  }else{
    comp <- base::max(base::length(analiVec), base::length(time))
    base::length(analiVec) <- comp
    base::length(time) <- comp
    analiDF <- base::data.frame(base::cbind(analiVec,time))
  }

  AA <- base::c(0,0)
  if(time.ana == "mean"){

    if(mode == 9){
      final <- base::data.frame(base::t(base::apply(analiVec, 2, mean, na.rm = T)))
    }else{
      final <- base::data.frame(base::mean(analiVec, na.rm = T))
    }



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
    }else if(mode == 9){
      base::colnames(final) <-  base::c("Mean arch 1", "Mean arch 2")
    }

    final
  }else{
    i = 0
    while (i < (base::round(base::max(time, na.rm = T), digits = t.rounding))){
      max <- i+time.ana
      min <- i
      subdf <- analiDF[analiDF$time<=max,]
      subdf <- subdf[subdf$time>i,]
      zeze <- base::suppressWarnings(base::max(subdf$time, na.rm = T))
      if(base::abs(zeze) != Inf){
        if(mode %in% base::c(4,6)){
          mean <- base::c(base::sum(subdf$analiVec, na.rm = T), base::max(subdf$time, na.rm = T))
        }else{

          if(mode == 9){
            mean <- base::c(base::t(apply(subdf[,1:2], 2, mean, na.rm = T)), base::max(subdf$time, na.rm = T))
          }else{
            mean <- base::c(base::mean(subdf$analiVec, na.rm = T), base::max(subdf$time, na.rm = T))
          }

        }
      }else{
        base::print(base::paste("Compleat lost of track in ", i, " time to ", i+time.ana))
        mean <- base::c(NA,NA)
      }
      base::suppressWarnings(AA <- base::rbind(AA, mean))
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
    }else if(mode == 9){
      base::colnames(final) <- base::c("Mean Arch 1", "Mean Arch 2", "Time")
    }
  }else if(mode == 8){
    if(time.ana == "mean"){
      stop("Just use under_sepisods function", call. = FALSE)
    }
    final$Time <- base::c((1:base::nrow(final))*time.ana)
    base::colnames(final) <- base::c("Total Time Under", "Number of Episodes", "Mean Time under Speed", "Time")
    }

    base::rownames(final) <- base::c((1:base::nrow(final))*time.ana)
    final[is.na(final)] <- NA
    final <- base::data.frame(final)
    final
  }
