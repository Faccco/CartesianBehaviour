# Internal function to split a data frame by a vector of interest zones in a defined axis.
#
# @param xlt vector of values to consider inside or outside a zone.
# @param xdft data frame to be spliced.
# @param axi the axis to be considered.
# @param MOD define if the exported zone is marked by 0's or 1's.
#
# @return A list of data frames witch the trajectory is inside the zone.
# @export .funfunfun
#
# @examples #See list.zones( ).
# @keywords internal
funfunfun <- function(xlt, xdft, axi = 0, MOD = 1){
  tempos <- inzone <- NULL
  passag <- list()

  xdft$inzone <- xlt

  xdft$tempos <- (xdft$inzone - dplyr::lead(xdft$inzone))

  d <- dplyr::filter(xdft, tempos != 0)

  xdft <- data.frame(xdft)

  splits <- base::data.frame(d$row + 1)

  if(axi == "yz" | axi == "zy"){

    xdft$polaryz <- base::complex(real = xdft$y, imaginary = xdft$z)

  }else if(axi == "xz" | axi == "zx"){

    xdft$polarxz <- base::complex(real = xdft$x, imaginary = xdft$z)

  }else{

    xdft$polar <- base::complex(real = xdft$x, imaginary = xdft$y)
  }
  passag <- trajr::TrajSplit(xdft, idx = splits)

  if(MOD == 1){
  znx <- base::lapply(passag, function(x) dplyr::filter(x, inzone != 0))
  }else{
    znx <- base::lapply(passag, function(x) dplyr::filter(x, inzone != max(xlt)))
  }
  znx <- base::Filter(function(x) dim(x)[1] > 0, znx)

  znx
}
