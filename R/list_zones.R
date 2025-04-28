#' Split the trajectory into zones of interest
#'
#' @param dado A data frame crated by \link[CartesianBehaviour]{standardized_trj} containing the positions and time of an subject.
#' @param Zones A data frame crated by \link[CartesianBehaviour]{Zones_int} containing the zones of interest.
#' @param Zplane The complementary axis used in \link[CartesianBehaviour]{Zones_int}. Input can be "xz", "zx", "yz" or "zy".
#' @param remove2D To remove the 2D zones from the final data. FALSE by default.
#'
#' @return A list containing the zones, witch are lists containing the data frames were the subject was inside the zone.
#' @export list_zones
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     id = 1, fps = 30)
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
#' TRAJ2D <- list_zones(TrajEXP, Zones = Zonas)
#'
#' #Run the code below only to see a 3D example, almost the same as the 2D
#' #Export the trajectory considering
#' #the Y axis both as Y and Z axis at the same time.
#' #TrajEXP3D <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", Zaxi = "y",
#' #                       time = "time", id = 2, fps = 30)
#' #
#' #Create your own zones, if you don't know how, go ?Zone_int
#' #zons3D <- Zones_int(nzonesd2 = 2, nzonesd3 = 4,
#' #                   faceZ = 1, maxX = 10, minX = -10,
#' #                   maxY = 5, minY = -5, maxZ = 7,
#' #                   minZ = -7, npts = 4)
#' #
#' #Split the zones
#' #TRAJ3D <- list_zones(TrajEXP, Zones = zons3D, Zplane = "xz", , remove2D = TRUE)
list_zones <- function(dado, Zones, Zplane = NA, remove2D = FALSE){

  zonesXY <- Zones[[1]]
  final <- base::list()
  namezon <- base::data.frame(0)
  nonames <- base::data.frame()

  for (i in 1:base::length(zonesXY)){

    inzone <- TrajInPlanezone(dado, zonesXY[[i]], plane = "xy")
    alt <- inzone
    alt[alt == 1] <- i
    alt[alt != i] <- 0

    final[[base::length(final) + 1]] <- funfunfun(alt, dado)
    nonames <- base::data.frame(base::rbind(nonames, base::as.character(i)))
    final


    if(!base::is.na(Zplane)){
      zonesZ <- Zones[[2]]
      for (j in 1:base::length(zonesZ)){

        inzone <- TrajInPlanezone(dado, zonesZ[[j]], plane = Zplane)
        elt <- inzone
        elt[elt == 1] <- j
        elt[elt != j] <- 0

        ilt <- base::as.numeric(base::paste(alt,elt, sep = "."))
        ilt[ilt != base::as.numeric(base::paste(i,j, sep = "."))] <- 0

        final[[base::length(final) + 1]] <- funfunfun(ilt, dado, axi = Zplane)
        nonames <- base::data.frame(base::rbind(nonames, base::paste(i,j, sep = ".")))
        final
      }
    }else{base::print("2D zone analyses performed")}
  }

  if(!base::is.na(Zplane)){
    zonesZ <- Zones[[2]]
    for (j in 1:base::length(zonesZ)){

      inzone <- TrajInPlanezone(dado, zonesZ[[j]], plane = Zplane)
      elt <- inzone
      elt[elt == 1] <- base::as.numeric(base::paste(0,j, sep = "."))
      elt[elt != base::as.numeric(base::paste(0,j, sep = "."))] <- 0

      final[[base::length(final) + 1]] <- funfunfun(elt, dado, axi = Zplane)
      nonames <- base::data.frame(base::rbind(nonames, base::paste(0,j, sep = ".")))
      final

    }
  }

  if(Zplane %in% c("xz","zx")){

    dado$polarxz <- base::complex(real = dado$x, imaginary = dado$z)
  }else if(Zplane %in% c("yz","zy")){

    dado$polaryz <- base::complex(real = dado$y, imaginary = dado$z)
  }

  for (i in 1:base::length(final)){
    nomeme <- final[[i]]

    if(base::length(nomeme) > 0){
      namezon[base::nrow(namezon)+1,] <- base::rbind(nomeme[[1]]$inzone[[1]])
    }else{
      namezon[base::nrow(namezon)+1,] <- base::rbind(c("not reached"))
    }
  }

  dado$polar <- base::complex(real = dado$x, imaginary = dado$y)
  final[[base::length(final) + 1]] <- base::list(dado)
  namezon[base::nrow(namezon)+1,] <- base::rbind(base::c("Total"))
  namezon <- namezon[2:base::nrow(namezon),]

  base::names(final) <- namezon

  if(remove2D){
    for (i in 1:(base::length(Zones[[1]])-1)) {
      nonames <- base::data.frame(nonames[-(i*(base::length(Zones[[2]])+2)),])
      final <- final[-(i*(base::length(Zones[[2]])+2))]
    }
    nonames <- base::data.frame(nonames[-1,])
    final <- final[-1]
    i<-1
    while(i < (base::length(Zones[[2]])+1)) {
      nonames <- base::data.frame(nonames[-(base::nrow(nonames)),])
      final <- final[-(base::length(final)-(1))]
      i <- i+1
    }
  }

  final <- base::list(final, nonames)
  final
}
