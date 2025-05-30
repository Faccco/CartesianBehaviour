#' Function to standardize a trajectory
#'
#' To standardize data from 1 or multiple data frames. For the 3 point positions of interest to future mesure bodi arch angles you have to chose between
#' "1Lcol3Point" - which is a dataframe with 1 column names and is just straight forward name  the column identifier in the head, center, tail parameters.
#' "2Lcol3Point" - optimized for DeepLabCut outputs, which have 3 lines of columns, 1 with a generated unique name, and 2 lines for defining the body parts and the type of coordinates (X, Y, Z), separately. in this case it will paste the "X/Y" + _ + "Body part column name" to perform the anlisis.
#' "multiDF3Point" - If you have the body parts spread in 3 different data frames. note that the center must be in the dfts input.
#'
#' @param dft The main data frame containing trajectory data (X and Y coordinates), an identifier of the subject and a frame or time information.
#' @param dftz A second data frame that contains the z coordinates and the other identifiers. The first frame of data must be the same as the dft input.
#' @param Xaxi A character input representing the column in dft presenting the X coordinates.
#' @param Yaxi A character input representing the column in dft presenting the Y coordinates.
#' @param Zaxi Can be the name of the column in dftz, or a vector of the type double (numeric) containing the Z coordinates.
#' @param frames The column in dfts representing frame information.
#' @param id Numeric identifier of the subject.
#' @param id_col The column in dfts that contain the subject identifier of the observations. If there is no identifier column all the data in the data frame will be considered.
#' @param fps The frame rate or another value to convert the time marker into a desired one.
#' @param time The column in dfts representing time information.
#' @param Gtype If you have any kind of of 3 point tracking select a type of data input. "unique" by default. See more in description.
#' @param center The name of the center point input.
#' @param head The name of the head columns or a data frame containing it.
#' @param tail The name of the tail columns or a data frame containing it.
#'
#'
#' @return A standardize data frame. Note that if Z is a vector its length must be the same as of the X and Y.
#' @export standardized_trj
#'
#'
#' @examples #Create an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#' #Export the trajectory
#' TrajEXP <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y",
#'                     time = "time", id = 1, fps = 30)
#' #View(TrajEXP)
#'
#' #Export the trajectory considering
#' #the Y axis both as Y and Z axis at the same time
#' TrajEXP2 <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", Zaxi = "y",
#'                      time = "time", id = 2, fps = 30)
#' #View(TrajEXP2)
standardized_trj <- function(dft, dftz = NULL, Xaxi = "x_cm", Yaxi = "y_cm",
                     Zaxi = NA, frames = NA, time = NA, id = 1, id_col = NULL,
                     fps = 30, Gtype = "unique", center = NA, head = NA, tail = NA){

  gtype <- base::as.numeric(base::pmatch(Gtype,
                                        base::c("unique", "1Lcol3Point", "2Lcol3Point", "multiDF3Point")))

  if(gtype == 3){

    columnNames <- base::paste(dft[2,], dft[1,], sep = "_")
    dft <- dft[3:base::nrow(dft),]
    colnames(dft) <- columnNames

    x <- Xaxi
    y <- Yaxi

    Xaxi <- paste(x, center, sep = "_")
    Yaxi <- paste(y, center, sep = "_")
    headx <- paste(x, head, sep = "_")
    heady <- paste(y, head, sep = "_")
    tailx <- paste(x, tail, sep = "_")
    taily <- paste(y, tail, sep = "_")
    if(!(base::is.na(Zaxi)) & base::length(Zaxi) == 1){
      Zaxi <- paste(Zaxi, center, sep = "_")
    }

  }

  if(is.na(frames) && is.na(time)){
    stop("There is no time marker to be based on. Please check.",
         call. = FALSE)
    }
  if(base::is.null(id_col)){
    dfts <- base::data.frame(dft)
  }else{
    dfts <- dplyr::filter(dft, dft[,id_col] == id)
  }



  x  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== Xaxi)])
  y  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== Yaxi)])

  if(base::is.na(frames) && base::is.character(time)){
  times <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== time)])

  frame <- ((times-times[1])*fps)+1

  for (i in 1:base::length(frame)){
    if(i > 1 && frame[i] %in% c(frame[i:(i+fps)])){
      if(frame[i] == frame[i-1]){
        frame[i] <- frame[i]+1
      }else if(frame[i] < frame[i-1]){
        frame[i] <- frame[i-1]+1
      }else{
        frame[i] <- frame[i]
      }
    }
  }
  times <- (frame-frame[1])/fps
  }else if(base::is.na(time) && base::is.character(frames)){
    frame <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== frames)])
    times <- (frame-frame[1])/fps
  }

  DateTime <- base::as.POSIXct(times, origin = "2023-01-01", tz = "UTC")
  ID <- base::c(base::rep(id, times = base::nrow(dfts)))
  row <- 1:base::nrow(dfts)

if(gtype != 1){

      if(gtype %in% c(2,3)){

        headx  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== headx)])
        heady  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== heady)])
        tailx  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== tailx)])
        taily  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== taily)])

    }else if(gtype == 4){

      FramE <- data.frame()
      head$FramE <- base::as.numeric(head[,base::which(base::colnames(head)== frames)])
      tail$FramE <- base::as.numeric(tail[,base::which(base::colnames(tail)== frames)])
      dfts$FramE <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== frames)])

      head$FramE <- (head$FramE - base::min(head$FramE)) + base::min(dfts$FramE)
      tail$FramE <- (tail$FramE - base::min(tail$FramE)) + base::min(dfts$FramE)




      dfts <- dplyr::distinct(dfts, FramE, .keep_all = TRUE)
      head <- dplyr::distinct(head, FramE, .keep_all = TRUE)
      tail <- dplyr::distinct(tail, FramE, .keep_all = TRUE)

      head <- head[head$FramE %in% dfts$FramE,]
      tail <- tail[tail$FramE %in% dfts$FramE,]
      dfts <- dfts[dfts$FramE %in% tail$FramE,]
      dfts <- dfts[dfts$FramE %in% head$FramE,]
      head <- head[head$FramE %in% dfts$FramE,]
      tail <- tail[tail$FramE %in% dfts$FramE,]

      if(TRUE %in% grepl("x", colnames(head))){
      headx <- base::as.numeric(head[,base::which(base::grepl("x", colnames(head)))])
      heady <- base::as.numeric(head[,base::which(base::grepl("y", colnames(head)))])
      tailx <- base::as.numeric(tail[,base::which(base::grepl("x", colnames(tail)))])
      taily <- base::as.numeric(tail[,base::which(base::grepl("y", colnames(tail)))])
      }else{
        headx <- base::as.numeric(head[,base::which(base::grepl("X", colnames(head)))])
        heady <- base::as.numeric(head[,base::which(base::grepl("Y", colnames(head)))])
        tailx <- base::as.numeric(tail[,base::which(base::grepl("X", colnames(tail)))])
        taily <- base::as.numeric(tail[,base::which(base::grepl("Y", colnames(tail)))])
      }

    }
}

  if(base::is.na(Zaxi) & base::is.null(dftz)) {
    base::print("2D suported analyses")


    if(gtype != 1){

      dfts <- base::data.frame(x, y, DateTime, frame, times, ID, row, headx, heady, tailx, taily)

    }else{

      dfts <- base::data.frame(x, y, DateTime, frame, times, ID, row)

    }

  }else if(!base::is.null(dftz) & base::typeof(Zaxi) == "character"){
    base::print("3D suported analyses with 2 data frames")

    if(base::is.null(id_col)){
      dftz <- base::data.frame(dftz)
    }else{
      dftz <- dplyr::filter(dftz, dftz[,id_col] == id)
    }

    dftz$frame <- (dftz$frame - base::min(dftz$frame)) + base::min(dfts$frame)
    dfts <- dplyr::distinct(dfts, frame, .keep_all = TRUE)
    dftz <- dplyr::distinct(dftz, frame, .keep_all = TRUE)
    dftz <- dftz[dftz$frame %in% dfts$frame,]
    dfts <- dfts[dfts$frame %in% dftz$frame,]

    axiZ <- base::as.numeric(dftz[,base::which(base::colnames(dftz) == Zaxi)])
    z <- axiZ

    x  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== Xaxi)])
    y  <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== Yaxi)])
    if(base::is.na(frames) && base::is.character(time)){
      times <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== time)])

      frame <- ((times-times[1])*fps)+1

      for (i in 1:base::length(frame)){
        if(i > 1 && frame[i] %in% c(frame[i:(i+fps)])){
          if(frame[i] == frame[i-1]){
            frame[i] <- frame[i]+1
          }else if(frame[i] < frame[i-1]){
            frame[i] <- frame[i-1]+1
          }else{
            frame[i] <- frame[i]
          }
        }
      }
    }else if(base::is.na(time) && base::is.character(frames)){
      frame <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== frame)])
      times <- (frame-frame[1])/fps
    }

    times <- (frame-frame[1])/fps

    DateTime <- base::as.POSIXct(times, origin = "2023-01-01", tz = "UTC")
    ID <- base::c(base::rep(id, times = base::nrow(dfts)))
    row <- 1:base::nrow(dfts)


    if(gtype != 1){

      dfts <- base::data.frame(x, y, z, DateTime, frame, times, ID, row, headx, heady, tailx, taily)

    }else{

      dfts <- base::data.frame(x, y, z, DateTime, frame, times, ID, row)

    }



  }else if(base::typeof(Zaxi) == "character"){
    base::print("3D suported analyses with 1 data frame")

    z <- base::as.numeric(dfts[,base::which(base::colnames(dfts)== Zaxi)])


    if(gtype != 1){

      dfts <- base::data.frame(x, y, z, DateTime, frame, times, ID, row, headx, heady, tailx, taily)

    }else{

      dfts <- base::data.frame(x, y, z, DateTime, frame, times, ID, row)

    }

    dfts

  }else if(base::typeof(Zaxi) == "double"){
    base::print("3D suported analyses with 1 data frame and a Z vector of positions")

    if(base::length(Zaxi) == base::length(x)){
      z <- base::as.numeric(Zaxi)
    }else{
      stop("Incorrect input of Z data. The length of observations differ from the data frame ",
           call. = FALSE)
    }

    dfts <- base::data.frame(x, y, z, DateTime, frame, times, ID, row)
    dfts
  }else{
    stop("Incorrect input of Z data. Please check.",
         call. = FALSE)
  }

  dfts <- dplyr::distinct(dfts, frame, .keep_all = TRUE)

  dfts

}
