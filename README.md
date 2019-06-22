# nba-raoblackwellizing-field-goal

Selected code for the article "Rao-Blackwellizing Field Goal Percentage" published in the Journal of Quantitative Analysis in Sports. 

This repo includes code to extract shot trajectories from tracking data, model shot trajectories using linear and Bayesian regression models, and calculate the depth, left-right distance, and entry angle of shots. It uses the publicly available tracking data, found [here](https://github.com/dcervone/EPVDemo/blob/master/data/2013_11_01_MIA_BKN.csv), as an example.

- **change_player_tracking_data_format.R** transforms the tracking data to the format used for the project, producing *transformed_tracking_data.RData*. This is not my code, the original script can be found [here](https://github.com/mvanbommel/nba_scorekeeper_bias/blob/master/player_tracking_data/change_player_tracking_data_format.R).

- **getDepthLeftRightAngle.R** is the primary script that calculates shot factors for all three point shots from a given game. This script utlizes functions from **modelTrajectoryFunctions.R** and **shotFactorFunctions.R**. This produces a dataframe of shot factors (*threes_factors.csv*) and a list of raw ball trajectories (*threes_trajectories.RData*). 

- **modelTrajectoryFunctions.R** contains functions that isolate ball trajectories from tracking data and model shot trajectories using both a linear regression and Bayesian regression.

- **shotFactorFunctions.R** contains functions that, given a shot's trajectory, calculates the depth, left-right distance and entry angle of the shot.
