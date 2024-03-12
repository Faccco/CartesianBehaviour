#' Episodes & time under a defined speed and above a time period
#'
#' @param list_zones The output of \link[CartesianBehaviour]{threshold_speed_by_frame} or \link[CartesianBehaviour]{Allbasics}
#' @param time_threshold The minimum time for an episode to be counted.
#' @param MOD Don't change, for internal use in \link[CartesianBehaviour]{over_time_ana}.
#'
#' @return The time the episodes lasted and the number of episodes completely in the zones and for total track. Note that even if the zones are tangents the sum of that can be greater or lesser than the number of episodes in the total trajectory.
#' @export under_sepisods
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                       angularErrorSd = .8, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn by setting Zones = NA,
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
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the episodes under a
#' #defined speed of the subject
#' Under.Speed.Episods <- under_sepisods(TRAJ2D, time_threshold = 0.001)
#' #View(Under.Speed.Episods)
under_sepisods <- function(list_zones, time_threshold = 1, MOD = "USER"){
  episodes <- NULL
  if(MOD == "USER"){
    list_zone <- list_zones[[1]]
  }else if(MOD == "DF"){
    list_zone <- list_zones
  }
  CC <- base::data.frame()

  for (j in 1:base::length(list_zone)) {

      www <- list_zone[[j]]

    AA <- base::data.frame()

    if(base::length(www) > 0){

      for (i in 1:base::length(www)) {

        www[[i]]$episodes <- www[[i]]$Abovespeed - dplyr::lag(www[[i]]$Abovespeed)

        d <- dplyr::filter(www[[i]], episodes != 0)

        time_inspeed <- dplyr::lead(d$time) - d$time
        time_inspeed <- base::append((dplyr::first(d$time) - dplyr::first(www[[i]]$time)), time_inspeed)
        time_inspeed <- base::append(time_inspeed, (dplyr::last(www[[i]]$time) - dplyr::last(d$time)))
        time_inspeed <- time_inspeed[!base::is.na(time_inspeed)]

        if(!base::is.na(d$Speeds[1])){

          if(d$episodes[1] == -1){

            uder_time <- time_inspeed[1:base::length(time_inspeed) %% 2 == 0]


          }else{

            uder_time <- time_inspeed[1:base::length(time_inspeed) %% 2 == 1]

          }



        }else if(base::sum(www[[i]]$Abovespeed, na.rm = T) <= 1){

          uder_time <- dplyr::last(www[[i]]$time) - dplyr::first(www[[i]]$time)

        }else{

          BB <- base::cbind(0, 0)
          AA <- base::rbind(AA,BB)
          next

        }

        uder_time <- uder_time[uder_time >= time_threshold]
        BB <- base::cbind(base::sum(uder_time), base::length(uder_time))
        AA <- base::rbind(AA,BB)
        AA
      }
        Zonedt <- base::cbind(base::sum(AA[1]), base::sum(AA[2]), base::sum(AA[1])/base::sum(AA[2]))
        CC <- base::rbind(CC, Zonedt)

    }else{

      Zonedt <- base::cbind(0, 0, 0)
      CC <- base::rbind(CC, Zonedt)

    }
  }

  if(MOD == "USER"){
  namezon <- base::vector()
  for (i in 1:base::nrow(list_zones[[2]])) {
    namezon <- base::append(namezon, base::paste("Under speed completely in Zone", list_zones[[2]][i,], sep=""))
  }
  namezon <- base::append(namezon, "Total")
  base::row.names(CC) <- namezon
  base::colnames(CC) <- c("Time", "Episodes", "Mean Time per Episode")
  CC
  }
  CC
}
