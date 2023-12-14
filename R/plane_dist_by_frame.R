# intra function to get distance to a plane by each frame
#
# Note that this function only works if the data frame and points is properly adjusted
#
# @param x1 X coordinate of point 1
# @param y1 Y coordinate of point 1
# @param z1 Z coordinate of point 1
# @param x2 X coordinate of point 2
# @param y2 Y coordinate of point 2
# @param z2 Z coordinate of point 2
# @param x3 X coordinate of point 3
# @param y3 Y coordinate of point 3
# @param z3 Z coordinate of point 3
# @param Anima A data frame containing X, y and z Coordinates.
#
# @examples Please see reg_dist_by_frame
plane_dist_by_frame <- function(x1, y1, z1, x2, y2, z2, x3, y3, z3, Anima){

a1 <- base::as.numeric(x2 - x1)
b1 <- base::as.numeric(y2 - y1)
c1 <- base::as.numeric(z2 - z1)
a2 <- base::as.numeric(x3 - x1)
b2 <- base::as.numeric(y3 - y1)
c2 <- base::as.numeric(z3 - z1)
a <- b1 * c2 - b2 * c1
b <- a2 * c1 - a1 * c2
c <- a1 * b2 - b1 * a2
d <- (-a * x1 - b * y1 - c * z1)
f <- base::sqrt((a^2)+(b^2)+(c^2))
PL <- base::data.frame(a,b,c,d)
distPlane <- base::data.frame(Anima[,1]*a, Anima[,2]*b, Anima[,3]*c)
distPlane <- base::abs(base::rowSums(distPlane))
distPlane <- distPlane+d
distPlane <- distPlane/f

}
