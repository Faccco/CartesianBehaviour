# Distance of the subject to a point
#
# @param www The output of \link[CartesianBehaviour]{list_zones}.
# @param AA A data frame with the coordinates of the point
#
# @return A list of splitted trajectory with an column of the distances to a point by each frame in the total trajectory data frame.
#
# @examples
point_dist_by_frame <- function(www, AA){
  condition <- base::c(base::colnames(www))
  if("z" %in% condition){
    for(j in 1:base::length(www)){

      ptTrack <- base::as.complex(www$polar)
      ptTrackyz <- base::complex(real = as.numeric(www$y), imaginary = base::as.numeric(www$z))
      polarpt <- base::rep(base::complex(real = as.numeric(AA$x), imaginary = base::as.numeric(AA$y)), times = base::length(ptTrack))
      polarptyz <- base::rep(base::complex(real = base::as.numeric(AA$y), imaginary = base::as.numeric(AA$z)), times = base::length(ptTrack))

      distPt1 <- base::Mod(ptTrack - polarpt)
      distPt2 <- base::Mod(ptTrackyz - polarptyz)
      cpx <- base::complex(real = distPt1, imaginary = distPt2)
      distPt <- base::c(Mod(cpx))
      distPt
    }
  }else{


    ptTrack <- base::as.complex(www$polar)
    polarpt <- base::rep(base::complex(real = base::as.numeric(AA$x), imaginary = base::as.numeric(AA$y)), times = base::length(ptTrack))
    distPt <- base::c(base::Mod(ptTrack - polarpt))
    distPt
  }
  distPt
}
