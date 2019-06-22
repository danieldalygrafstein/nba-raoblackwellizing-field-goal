##
## Script to calculate the depth, left-right distance and entry angle of all 3-point shots
## Script requires transformed_tracking_data.RData, modelTrajectoryFunctions.R, shotFactorFunctions.R

## Produces a dataframe of all threepoint shots and estimated shot factors
## from given game as well as list of corresponding raw ball trajectories from the tracking data

library(dplyr)


source('modelTrajectoryFunctions.R')
source('shotFactorFunctions.R')


load('transformed_tracking_data.RData')


# function to update shot and balltrajectory lists if balltrajectory incomplete
shot_null <- function(player, ball=NA){
  shots[[index]]<- c(player$entity, rep(NA, 6), player$event_id, index)
  balltraj[[index]]<- ball
  index = index + 1
  
}


# First filter out shots, then select out arc threes and corner threes
time_of_threes <- transformed_tracking_data %>% 
                  filter(event_id %in% c(3,4)) %>% # shot filter
                  filter((sqrt((x-5.25)^2+(y-25)^2)>23.75 & sqrt((x-88.75)^2+(y-25)^2)>23.75) | # arc threes
                           ((x < 14) | (x > 80)) & ((y < 3) | (y > 47))) %>% # corner threes
                  select(time)


shots <- list(nrow(time_of_threes))
balltraj <- list(nrow(time_of_threes))

index = 1
for(tm in time_of_threes$time){
  
  # get all the moments from the time of the shot until 3 seconds after release
  shot <- transformed_tracking_data %>% filter(time >= tm & time < (tm + 3000))
  
  # get player who took shot
  player <- trim_to_player(shot)
  
  # isolate just the ball moments
  ball <- trim_to_ball(shot)
  
  # trim_to_ball returns NULL if shot trajectory not sufficient to create model
  # if NULL, do not calculate depth, left-right distance, or angle, return NAs for shot factors
  
  if (is.null(ball)){
    shot_null(player)
    next 
  }
  
  #linear shot trajectory model calculations
  
  #get the x,y coordinates of the ball at the start and end of the shot trajectory and the trajectory coefficients
  ball.start.end <- ball_trajectory_model(ball)
  
  if(is.null(ball.start.end)){
    shot_null(player, ball=ball)
    next 
  }
  
  ball.release <- ball.start.end[[1]]
  ball.basket <- ball.start.end[[2]]
  linear.coefficients <- ball.start.end[[3]]
  
  
  #adjust the front of the room to the perspective of the player
  adj.front.rim <- get_front_of_hoop(ball.release)
  
  #get the shot factors
  leftright.depth <- get_leftright_depth_shot(ball.basket,adj.front.rim)
  angle <- get_entry_angle(ball, linear.coefficients, ball.basket)
  
  
  #Repeat the shot trajectory model and shot factor calculations using Bayesian regression
  
  # fit the trajectory model using Bayesian regression with 4 pseudo points to specify priors
  ball.start.end.bayes <- bayesian_ball_trajectory_model(ball,player)
  
  if(is.null(ball.start.end.bayes)){
    shot_null(player, ball=ball)
    next 
  }
  
  ball.release.bayes <- ball.start.end.bayes[[1]]
  ball.basket.bayes <- ball.start.end.bayes[[2]]
  bayes.coefficients <- ball.start.end.bayes[[3]]
  
  adj.front.rim.bayes <- get_front_of_hoop(ball.release.bayes)
  leftright.depth.bayes <- get_leftright_depth_shot(ball.basket.bayes,adj.front.rim.bayes)
  angle.bayes <- get_entry_angle(ball, bayes.coefficients, ball.basket.bayes)
  
  shot <- c(player$entity,leftright.depth, angle ,leftright.depth.bayes,
          angle.bayes, player$event_id, index)
  shots[[index]]<-shot
  
  balltraj[[index]]<-ball
  
  index = index + 1
}

df_shots <- as.data.frame(do.call("rbind", shots))
names(df_shots)<-c("Player.ID", "Left.Right.Linear","Depth.Linear","Angle.Linear","Left.Right.Bayes",
                   "Depth.Bayes","Angle.Bayes", "Event.ID","Index")
df_shots<-df_shots[order(df_shots$Player.ID),]
write.csv(df_shots, file = "threes_factors.csv")

save(balltraj, file="threes_trajectories.RData")




