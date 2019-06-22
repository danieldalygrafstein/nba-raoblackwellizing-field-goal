## Set of functions to isolate ball trajectories from tracking data and 
## model shot trajectories using both a linear regression and Bayesian regression
## Utilized by script getDepthLeftRightAngle.R

CENTRE_OF_HOOP<-c(5.25,25)


# get player who took the shot
trim_to_player<-function(shot){
  player <- shot[(shot$event_id %in% c(3,4)),]
  
  # sometimes more than one event ID listed in the three second frame
  # in this case take the first shot
  if (nrow(player > 1)){
    player <- player[order(player$time),]
    player <- player[1,]
  }
  
  #get player in front left corner of court
  if (player$x[1]>50){player$x=94-player$x} 
  if (player$y[1]>25){player$y=50-player$y}
  
  return(player)
}

# isolate just the ball moments
# return NULL if:
# no ball moments
# if the shot trajectory is never getting further from hoop
#### i.e. if the shot trajectory measurements stop before the end of the shot (no bounce off rim or ball
#### travelling through the hoop)
# if ball never has a distance of less than 5 feet from the hoop
# if, after filtering to points where ball is only travelling closer to the hoop, the ball never
# starts travelling down
# if, after filtering, there are less than 15 total points in the shot trajectory
trim_to_ball<-function(shot){
  ball <- shot[shot$entity==-1,]
  
  # return NULL if no ball moments
  if(nrow(ball)==0){
    return(NULL)
  }
  
  #get player in front left corner of court
  if (ball$x[1]>50){ball$x=94-ball$x} 
  if (ball$y[1]>25){ball$y=50-ball$y}
  
  ball$z.firstdif <- append(0,diff(ball$z))
  
  #L1 distance of the ball from the hoop
  ball$distance.to.hoop <- (ball$x-CENTRE_OF_HOOP[1])^2 + (ball$y-CENTRE_OF_HOOP[2])^2 + 
                      (ball$z-10)^2
  #Calculate when ball getting further from hoop
  ball$distance.to.hoop.firstdif <- append(0,diff(ball$distance.to.hoop))
  
  # return NULL if ball is never getting further from the hoop
  if(max(ball$distance.to.hoop.firstdif)<=0){
    return(NULL)
  }
  
  # Cut to points where distance to hoop increasing and distance from ball to hoop is less than 5 
  cut=which(ball$distance.to.hoop.firstdif > 0 & ball$distance.to.hoop < 5)
  
  # return NULL if ball is never increasing in distance from hoop and less than 5 feet away
  if(length(cut)==0){
    return(NULL)
  }
  
  #Filter to points before first time distance to hoop starts increasing
  ball=ball[(1:min(cut)-1),] 
  
  # Keep only points greater than 7 feet
  ball=ball[ball$z>7,]
  
  # Cut points where ball is travelling down
  cut2=which(ball$z.firstdif<0)
  
  # return NULL if, in moments where distance to hoop is decreasing, ball never travels down
  if(length(cut2)==0){
    return(NULL)
  }
  
  ball=ball[(1:max(cut2)),]
  
  # return NULL if, after all this filtering, less than 15 points in shot trajectory
  if(nrow(ball)<15){
    return(NULL)
  }
  return(ball)
}


#Takes ball trajectory and returns list of the 2 x,y locations of the ball when z=10
#as well as the coefficients of the trajectory model
#These two locations are taken to be the start and end points of each shot
#if no real roots to quadratic equation, function returns NULL
ball_trajectory_model <- function(ball){
  
  #get x in terms of y 
  xybestfit <- lm(ball$x~ball$y) 
  #get z in terms of x and y
  zbestfit <- lm(ball$z~ball$y + ball$x + I(ball$y^2) + I(ball$x^2)) 
  
  #solve for quadratic equation coefficients in terms of y
  quadcoefficients <- get_quad_coefficients_y(xybestfit$coefficients,zbestfit$coefficients)
  
  #get y coordinate of ball when z=10
  y_ball=quadsolve(quadcoefficients[1],quadcoefficients[2],quadcoefficients[3])
  
  #if no real roots to quadratic equation, return NULL
  if(!is.numeric(y_ball)){
    return(NULL)
  }
  
  #get x coordinates back from y when z=10
  x_ball<-c(y_ball[1]*xybestfit$coefficients[2]+xybestfit$coefficients[1],
            y_ball[2]*xybestfit$coefficients[2]+xybestfit$coefficients[1])
  
  #take the x,y coordinate closer to the basket as the end of the shot
  y_ball_basket=y_ball[which.min(sqrt((25-y_ball)^2+(5.25-x_ball)^2))]
  x_ball_basket=y_ball_basket*xybestfit$coefficients[2]+xybestfit$coefficients[1]
  y_ball_start=y_ball[which.max(sqrt((25-y_ball)^2+(5.25-x_ball)^2))]
  x_ball_start=y_ball_start*xybestfit$coefficients[2]+xybestfit$coefficients[1]
  
  return(list(c(x_ball_start,y_ball_start),c(x_ball_basket,y_ball_basket), zbestfit$coefficients))
}


#solve for quadritic equation coeffs ay^2+by+c by subbing in y for x and z when z=10
get_quad_coefficients_y<-function(xy,z){
  z[is.na(z)]<-0
  a=z[4]+z[5]*xy[2]^2
  b=(z[2]+z[3]*xy[2]+
       z[5]*2*xy[2]*xy[1])
  c=z[1]-10+z[3]*xy[1]+z[5]*xy[1]^2
  return(c(a,b,c))
}

#solve for roots
quadsolve <- function(a,b,c){
  if(delta(a,b,c) > 0){ #first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
    return(result)
  }
  else if(delta(a,b,c) == 0){ #second case D=0
    x = -b/(2*a)
    return(x)
  }
  else {"No real roots."} #third case D<0
}

delta<-function(a,b,c){
  b^2-4*a*c
}



# takes ball trajectory and player location at shot release and
# returns list of the 2 x,y locations of the ball when z=10, as well as the coefficients of the
# trajectory model
# if no real roots to quadratic equation, function returns NULL
bayesian_ball_trajectory_model<-function(ball,player){
  
  #hyper parameters
  u_0<- c(0,0,0,0,0)
  Lambda_0<-as.matrix(diag(0.05,5,5))
  
  #4 pseudo data points
  #2 at location of player at release, 2 at the centre of the basket
  X_pseudo<-matrix(c(1,1,1,1,player$y[1],player$y[1],25,25,player$x[1],player$x[1],5.25,5.25,
                     player$y[1]^2,player$y[1]^2,25^2,25^2, player$x[1]^2,player$x[1]^2,5.25^2,5.25^2),
                   byrow=F, ncol=5, nrow=4)
  #pseudo response
  #2 at 7 feet in height, 2 at 10 feet
  y<-as.vector(c(7, 7, 10, 10))
  
  #First Update
  Lambda_1<-(t(X_pseudo)%*%X_pseudo + Lambda_0)
  u_1 <- solve((t(X_pseudo)%*%X_pseudo + Lambda_0)) %*% (Lambda_0%*%u_0 + t(X_pseudo)%*%y)
  
  #Real data
  X<-matrix(c(rep(1,length(ball$z)), ball$y,ball$x,ball$y^2,ball$x^2), 
            byrow=F, ncol = 5, nrow=length(ball$z))
  
  #Second Update
  Lambda_2<-(t(X)%*%X + Lambda_1)
  u_2 <- solve((t(X)%*%X + Lambda_1)) %*% (Lambda_1%*%u_1 + t(X)%*%ball$z)
  
  
  #Solve for x,y values when z=10
  
  #x in terms of y
  xybestfit=lm(ball$x~ball$y)  
  quadcoefficients=get_quad_coefficients_y(xybestfit$coefficients,u_2)
  y_ball=quadsolve(quadcoefficients[1],quadcoefficients[2],quadcoefficients[3])
  
  #if no real roots to quadratic equation, return NULL
  if(!is.numeric(y_ball)){
    return(NULL)
  }
  
  #get x coordinates back from y when z=10
  x_ball<-c(y_ball[1]*xybestfit$coefficients[2]+xybestfit$coefficients[1],
            y_ball[2]*xybestfit$coefficients[2]+xybestfit$coefficients[1] )
  
  #take the x,y coordinate closer to the basket as the end of the shot
  y_ball_basket=y_ball[which.min(sqrt((25-y_ball)^2+(5.25-x_ball)^2))]
  x_ball_basket=y_ball_basket*xybestfit$coefficients[2]+xybestfit$coefficients[1]
  y_ball_start=y_ball[which.max(sqrt((25-y_ball)^2+(5.25-x_ball)^2))]
  x_ball_start=y_ball_start*xybestfit$coefficients[2]+xybestfit$coefficients[1]
  
  return(list(c(x_ball_start,y_ball_start),c(x_ball_basket,y_ball_basket), u_2))
}

