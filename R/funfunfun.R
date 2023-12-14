# Internal function to split a data frame by a vector of interest zones in a defined axis.
#
# @param xlt vector of values to consider inside or outside a zone.
# @param xdado data frame to be spliced.
# @param axi the axis to be considered.
# @param MOD define if the exported zone is marked by 0's or 1's.
#
# @return A list of data frames witch the trajectory is inside the zone.
# @export .funfunfun
#
# @examples #See list.zones( ).
# @keywords internal
funfunfun <- function(xlt, xdado, axi = 0, MOD = 1){
  tempos <- inzone <- NULL
  passag <- list()

  xdado$inzone <- xlt

  xdado$tempos <- (xdado$inzone - dplyr::lead(xdado$inzone))

  d <- dplyr::filter(xdado, tempos != 0)

  xdado <- data.frame(xdado)

  splits <- base::data.frame(d$row + 1)

  if(axi == "yz" | axi == "zy"){

    xdado$polaryz <- base::complex(real = xdado$y, imaginary = xdado$z)

  }else if(axi == "xz" | axi == "zx"){

    xdado$polarxz <- base::complex(real = xdado$x, imaginary = xdado$z)

  }else{

    xdado$polar <- base::complex(real = xdado$x, imaginary = xdado$y)
  }
  passag <- trajr::TrajSplit(xdado, idx = splits)

  if(MOD == 1){
  znx <- base::lapply(passag, function(x) dplyr::filter(x, inzone != 0))
  }else{
    znx <- base::lapply(passag, function(x) dplyr::filter(x, inzone != max(xlt)))
  }
  znx <- base::Filter(function(x) dim(x)[1] > 0, znx)

  znx
}
