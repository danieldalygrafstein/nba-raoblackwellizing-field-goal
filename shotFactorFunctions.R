## Set of functions to calculate the depth, left-right distance and entry angle of a shot
## Given its trajectory
## Utilized by script getDepthLeftRightAngle.R


CENTRE_OF_HOOP<-c(5.25,25)


#get the location of the front of the rim relative to the shot release
get_front_of_hoop<-function(ball){
  x.diff=CENTRE_OF_HOOP[1]-ball[1]
  y.diff=CENTRE_OF_HOOP[2]-ball[2]
  angle.hoop=atan(x.diff/y.diff)
  front.hoop=c(CENTRE_OF_HOOP[1]-0.75*sin(angle.hoop),CENTRE_OF_HOOP[2]-0.75*cos(angle.hoop))
  return(front.hoop)
}


#get the leftright distance and shot depth 
get_leftright_depth_shot<-function(ballbasket,adjfrontrim){
  
  #slope between adj front of rim and centre of rim
  slope_centre_hoop=(adjfrontrim[2]-CENTRE_OF_HOOP[2])/(adjfrontrim[1]-CENTRE_OF_HOOP[1]) 
  
  #slope of tangent line passing through adj front of rim
  slope_tangent_front=-(1/slope_centre_hoop) 
  
  #y intercept of tangent line passing through adj front of rim
  y_intercept_tangent_front=adjfrontrim[2]-slope_tangent_front*adjfrontrim[1]
  #y intercept of line between centre of rim and adjusted front of rim
  y_intercept_ball_basket=ballbasket[2]-slope_centre_hoop*ballbasket[1]
  
  coeffs=matrix(c(-slope_tangent_front,1,-slope_centre_hoop,1), nrow=2, ncol=2, byrow = T)
  rhs=matrix(c(y_intercept_tangent_front, y_intercept_ball_basket), nrow=2, ncol=1, byrow = T)
  
  #check if slope matrix invertible, if it isn't shot is exactly straight on
  if (!test_invertible(coeffs)){
    left.right.dist<-ballbasket[2]-adjfrontrim[2]
    depth.dist<-ballbasket[1]-adjfrontrim[1]
    return(c(left.right.dist,depth.dist))
  }
  
  #solve for where two lines intersect
  my_intersection=solve(coeffs) %*% rhs 
  
  #distance between adj front of rim and intersection
  left.right.dist=sqrt((adjfrontrim[1]-my_intersection[1])^2+(adjfrontrim[2]-my_intersection[2])^2) 
  
  #mark left distances negative
  if (adjfrontrim[1]>my_intersection[1]){
    left.right.dist=left.right.dist*-1  
  }
  
  #distance between intersection and point ball went in/hit rim
  depth.dist=sqrt((ballbasket[1]-my_intersection[1])^2+(ballbasket[2]-my_intersection[2])^2)
  
  #if ball lands before front rim tangent line, it has neg depth
  if (my_intersection[2]>ballbasket[2]){
    depth.dist=depth.dist*-1       
  }
  
  return(c(left.right.dist,depth.dist))
}

test_invertible <- function(m) class(try(solve(m),silent=T))=="matrix"


#get the entry angle of the shot trajectory
get_entry_angle<-function(ball, ballcoefficients, ballbasket){
  
  #get x in terms of y
  xybestfit=lm(ball$x~ball$y)
  
  # set the distance to measure the angle at
  ydiff=0.01
  
  yfirstdif=ballbasket[2]-ydiff
  xfirstdif=xybestfit$coefficients[1]+xybestfit$coefficients[2]*yfirstdif
  xdiff=ballbasket[1]-xfirstdif
  
  # get the difference in height between the x,y coordinates of the ball at the basket
  # and the x,y coordinates of the ball at the basket minus xdiff, ydiff
  zfirstdif=(ballcoefficients[1]+ballcoefficients[2]*yfirstdif+
               ballcoefficients[3]*xfirstdif + ballcoefficients[4]*yfirstdif^2+
               ballcoefficients[5]*xfirstdif^2)
  
  # ball at the basket is by definition at a height of 10 feet
  zdiff=zfirstdif-10
  
  dist=sqrt(ydiff^2+xdiff^2)
  angle=(abs(atan(zdiff/dist)*180/pi))
  return(angle)
}

