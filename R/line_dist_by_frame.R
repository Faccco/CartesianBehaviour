# intra function to get distance to a plane by each frame
#
# Note that this function only works if the data frame and points are properly adjusted.
#
# @param Anima A data frame containing X, y and optionally z Coordinates.
# @param b The coordinate of point b in a data frame.
# @param c The coordinate of point c in a data frame.
#
# @examples Please see reg_dist_by_frame
line_dist_by_frame <- function(Anima, b, c){
  condition <- base::c(base::colnames(Anima))
  distLine <- base::data.frame()
  a <- base::data.frame(Anima$x,Anima$y)
  v1 <- b - c
  v2 <- data.frame(a$Anima.x - b$x)
  v2$y <- a$Anima.y - b$y
  if("z" %in% condition){
    a$z <- Anima$z
    v2$z <- a$z - b$z
    names(v2) <- c("x", "y", "z")
    for(i in 1:base::nrow(v2)){
      v3 <- v1[2]*v2[i, 3]-v1[3]*v2[i, 2]
      v3$y <- v1[3]*v2[i, 1]-v1[1]*v2[i, 3]
      v3$z <- v1[1]*v2[i, 2]-v1[2]*v2[i, 1]
      dis <- base::sqrt(base::sum(v3*v3))/base::sqrt(base::sum(v1*v1))
      distLine <- base::rbind(distLine, dis)
      distLine
    }
    }else{
      colnames(v2) <- c("x", "y")
      for(i in 1:base::nrow(v2)){
      mtx <- base::as.matrix(base::rbind(v1,v2[i,]))
      dis <- base::abs(base::det(mtx))/base::sqrt(base::sum(v1*v1))
      distLine <- base::rbind(distLine, dis)
      distLine
      }

      }
  AA <- distLine[,1]
  AA
  }
