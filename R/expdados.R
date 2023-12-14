#' Function to standardize a trajectory
#'
#' @param dado The main data frame containing trajectory data (X and Y coordinates), an identifier of the subject and a frame or time information.
#' @param dadoz A second data frame that contains the z coordinates and the other identifiers.
#' @param Xaxi A character input representing the column in dado presenting the X coordinates.
#' @param Yaxi A character input representing the column in dado presenting the Y coordinates.
#' @param Zaxi Can be the name of the column in dadoz, or a vector of the type double (numeric) containing the Z coordinates.
#' @param frames The column in dados representing frame or time information. If this is the time column set the fps = 1.
#' @param id Numeric identifier of the subject.
#' @param id_col The column in dados that contain the subject identifier of the observations. If there is no identifier column all the data in the data frame will be considered.
#' @param fps The frame rate or another value to convert the time marker into a desired one.
#'
#' @return A standardize data frame. Note that if Z is a vector its length must be the same as of the X and Y.
#' @export expdados
#'
#' @examples #Create an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#' #Export the trajectory
#' TrajEXP <- expdados(Traj, Xaxi = "x", Yaxi = "y",
#'                     frames = "time", id = 1, fps = 30)
#' #View(TrajEXP)
#'
#' #Export the trajectory considering
#' #the Y axis both as Y and Z axis at the same time
#' TrajEXP2 <- expdados(Traj, Xaxi = "x", Yaxi = "y", Zaxi = "y",
#'                      frames = "time", id = 2, fps = 30)
#' #View(TrajEXP2)
expdados <- function(dado, dadoz = NULL, Xaxi = "x_cm", Yaxi = "y_cm",
                     Zaxi = NA, frames = "frame", id = 1, id_col = NULL, fps = 30){

  if(base::is.null(id_col)){
    dados <- base::data.frame(dado)
  }else{
    dados <- dplyr::filter(dado, dado[,id_col] == id)
  }

  x  <- base::as.numeric(dados[,base::which(base::colnames(dados)== Xaxi)])
  y  <- base::as.numeric(dados[,base::which(base::colnames(dados)== Yaxi)])
  frame <- base::as.numeric(dados[,base::which(base::colnames(dados)== frames)])
  time <- (frame-frame[1])/fps
  DateTime <- base::as.POSIXct(time, origin = "2023-01-01", tz = "UTC")
  ID <- base::c(base::rep(id, times = base::nrow(dados)))
  row <- 1:base::nrow(dados)

  if(base::is.na(Zaxi) & base::is.null(dadoz)) {
    base::print("2D suported analysis")

    dados <- base::data.frame(x, y, DateTime, frame, time, ID, row)

  }else if(!base::is.null(dadoz)){
    base::print("3D suported analysis with 2 data frames")

    if(base::is.null(id_col)){
      dadoz <- base::data.frame(dadoz)
    }else{
      dadoz <- dplyr::filter(dadoz, dadoz[,id_col] == id)
    }

    dadoz$frame <- (dadoz$frame - base::min(dadoz$frame)) + base::min(dados$frame)
    dados <- dplyr::distinct(dados, frame, .keep_all = TRUE)
    dadoz <- dplyr::distinct(dadoz, frame, .keep_all = TRUE)
    dadoz <- dadoz[dadoz$frame %in% dados$frame,]
    dados <- dados[dados$frame %in% dadoz$frame,]

    axiZ <- base::as.numeric(dadoz[,base::which(base::colnames(dadoz) == Zaxi)])
    z <- axiZ

    x  <- base::as.numeric(dados[,base::which(base::colnames(dados)== Xaxi)])
    y  <- base::as.numeric(dados[,base::which(base::colnames(dados)== Yaxi)])
    frame <- base::as.numeric(dados[,base::which(base::colnames(dados)== frames)])
    time <- (frame-frame[1])/fps
    DateTime <- base::as.POSIXct(time, origin = "2023-01-01", tz = "UTC")
    ID <- base::c(base::rep(id, times = base::nrow(dados)))
    row <- 1:base::nrow(dados)

    dados <- base::data.frame(x, y, z, DateTime, frame, time, ID, row)

  }else if(base::typeof(Zaxi) == "character"){
    base::print("3D suported analysis with 1 data frame")

    z <- base::as.numeric(dados[,base::which(base::colnames(dados)== Zaxi)])

    dados <- base::data.frame(x, y, z, DateTime, frame, time, ID, row)
    dados

  }else if(base::typeof(Zaxi) == "double"){
    base::print("3D suported analysis with 1 data frame and a Z vector of positions")

    if(base::length(Zaxi) == base::length(x)){
      z <- base::as.numeric(Zaxi)
    }else{
      stop("Incorrect input of Z data. The length of observations differ from the data frame ",
           call. = FALSE)
    }

    dados <- base::data.frame(x, y, z, DateTime, frame, time, ID, row)
    dados
  }else{
    stop("Incorrect input of Z data. Please check.",
         call. = FALSE)
  }

  dados <- dplyr::distinct(dados, frame, .keep_all = TRUE)

  dados

}
