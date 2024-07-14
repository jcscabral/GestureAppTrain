#
# First step, run this code to load 
# the basic functionalities
#

#############
# libraries #
#############

#library(parallel)
pacotes <- c("tidyverse","PerformanceAnalytics", "Hmisc", "psych", "ggplot2",
             "gridExtra", "moments", "goeveg", "SnowballC", "randomForest",
             "caret", "stats", "lsa", "FSelectorRcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

suppressMessages(library(e1071))


#############
# constants #
#############

# restraint random activities
set.seed(42)

SWIPE_FILE <- 'swipeData.csv'  
KEYBOARD_FILE <- 'keyboardData.csv'  
SENSORS_FILE <- 'sensorsData.csv'

PATH_PARENT <- paste(getwd(), '/datasets/', sep = '' )

# All data
FILES_A <- c('20240119', '20240122_20', '20240123', '20240124',
               '20240125', '20240126', '20240228', '20240310')
# Only data with correct keyboard sensors
FILES_B <- c('20240124', '20240125', '20240126', '20240228', '20240310')

KEYBOARD_LOGIN <- 5
KEYBOARD_AUTH <- 6
SWIPE_HOME <- 0
SWIPE_PIX_SEND <- 2 

TYPE_ACCELEROMETER <- 1
TYPE_MAGNETIC_FIELD <- 2
TYPE_GYROSCOPE <- 4

#############
# functions #
#############
#
# stats functions #
# teh2019 
# SOF second-order features
#

# Minimum (mn), 
# Maximum (mx), 
# Arithmetic Mean (am), 
# Quadratic Mean (qm),
# Harmonic Mean (hm),   X
# Geometric Mean (gm),  X  
# Median (md), 
# Range (rg),
# Variance (vr), 
# Standard Deviation (sd),
# Skewness (sk),
# Kurtosis (ku),
# First Quartile (fq), 
# Third Quartile (tq),
# Interquartile Range (ir), 
# Mean Absolute Deviation (ma), X
# Median Absolute Deviation (mi), X
# Coefficient of Variation (cv),
# Standard Error of Mean (se)

# Quadratic Mean
qm <- function(x){
  x.clean <- x[!is.na(x)]
  r <- sqrt(sum(x.clean ^ 2)/length(x.clean))
  return(r)
} 

# Range
rg <- function(x){
  r <-range(x, na.rm = T)
  return(r[2] - r[1])
}

#Percentil
pc <- function(x, p){
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[1])
}

#Interquartile
qt <- function(x, nq){
  p <- c(0.0, 0.25, 0.5, 0.75,1 )
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[nq + 1])
}

# Median Absolute Deviation
mi <- function(x){
  # median(|Yi – median(Yi|)
  x.na <- x[!is.na(x)]
  r <- median(abs(x.na - median(x.na)))
  return(r)
}

# Standard Error of Mean
se <- function(x){
  x.na <- x[!is.na(x)]
  r <- sd(x.na)/sqrt(length(x.na))
  return(r)
} 

# max deviation
mxdv <- function(x){
  x.na <- x[!is.na(x)]
  r <- max(sqrt((x.na - mean(x.na))^2))
  return(r)
}

# percentile of deviation
pcdv <- function(x, p){
  x.na <- x[!is.na(x)]
  r <- sqrt((x.na - mean(x.na))^2)
  return(pc(r, p))
}

# first
fs <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[1])
}

# last
lt <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[length(x)])
}

# magnitude vector
mg <- function(x, y){
  r <- norm(c(x,y), type="2")
  return(r)
}


mg3 <- function(x, y, z){
  r <- norm(c(x,y,z), type="2")
  return(r)
}

# euclidian distance
ec <- function(x1, x2, y1, y2){
  r <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(r)
}

dif <- function(x){
  x0 <- fs(x)    
  x1 <- lt(x)
  return(x1 - x0)
}

ang <- function(x1, x2, y1, y2){
  r <- atan2((y2 - y1), (x2 - x1))
  return(r)
}

avang <- function(x, y){
  y.diff <- diff(y)
  x.diff <- diff(x)
  tangs <- y.diff/x.diff
  r <-  mean(atan(tangs))
  return(r)
}


mh <- function(xyz){
  return(
    mahalanobis(
      xyz,
      colMeans(xyz),
      cov(xyz)
    )
  )
}


###################
# loaders scripts #
###################

# load others files

source('fix_bug_20240119.R')
source('fix_bug_20240122_20.R')

# sensors

loadSensors <- function(){
  
  sensors.total = NULL
  
  for(file in FILES_B){
    
    path_day <- paste(PATH_PARENT, file, '/', sep = '')
    
    # sensors #
    full_path <- paste(path_day, SENSORS_FILE, sep = '')
    sensors <- fread(full_path)
    cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type',
                'x', 'y', 'z', 'timestamp')  
    names(sensors) <- cnames
    
    ids_completed <- unique(sensors[sensors$action_number == 7]$user_id)
    sensors.completed <- sensors %>% filter(user_id %in% ids_completed)
    
    if(is.null(sensors.total)){
      sensors.total <- sensors.completed
    }
    else{
      sensors.total <-rbind(sensors.total, sensors.completed)
    }
  }
  
  sensors <- NULL
  sensors.completed <- NULL
  
  ids_completed <- unique(sensors.total$user_id)
  ids_completed <- ids_completed[ids_completed!=33]
  # except id 33, 2024/1/23
  #length(ids_completed) # 18
  
  sensors <- sensors.total %>% filter(user_id %in% ids_completed)
  sensors.total =  NULL
  sensors$timestamp <- sensors$timestamp / 1000000 
  
  # no longer, unfortunately 
  #sensors <- rbind(sensors, sensor_data_20240119())
  #sensors <- rbind(sensors, sensor_data_20240122_20())
  # sensors.wrong <-sensor_data_20240122_20()
  
  #ids_completed <- sort(unique(sensors$user_id))
  
  # magnitude vector
  sensors$w <- sqrt((sensors$x^2) + (sensors$y^2) + (sensors$z^2))
  
  return(sensors) 
}

loadSensorsLogin <- function(){
  sensors.login <- loadSensors() %>% filter(
    app_action %in% c(KEYBOARD_LOGIN, KEYBOARD_AUTH))
  return(sensors.login)
}

loadSensorsSwipe <- function(){
  sensors.swipe <- loadSensors() %>% filter(
    app_action %in% c(SWIPE_HOME, SWIPE_PIX_SEND)
  )
  return(sensors.swipe)
}

loadKeyboard <- function(loadFixed = F) {
  
  # Load ####
  
  keyboard.total = NULL
  
  for(file in FILES_B){
    
    path_day <- paste(PATH_PARENT, file, '/', sep = '')
    
    full_path <- paste(path_day, KEYBOARD_FILE, sep = '')
    keyboard <- fread(full_path)
    cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
                'character', 'pressure', 'x', 'y', 'uptime')  
    names(keyboard) <- cnames
    
    ids_completed <- unique(keyboard[keyboard$action_number == 7]$user_id)
    keyboard.completed <- keyboard %>% filter(user_id %in% ids_completed)
    
    # it started with user_id == 51
    # if (file == '20240228'){
    #   keyboard[keyboard$user_id>50]
    # }
    if(is.null(keyboard.total)){
      keyboard.total <- keyboard.completed
    }
    else{
      keyboard.total <-rbind(keyboard.total, keyboard.completed)
    }
  }
  
  keyboard <- NULL
  keyboard.completed <- NULL
  
  keyboard <- keyboard.total #%>% filter(user_id %in% ids_completed)
  keyboard.total =  NULL
  
  if (loadFixed == T){
    # fixed data
    keyboard <- rbind(keyboard, keyboard_data_20240119())
    keyboard <- rbind(keyboard, keyboard_data_20240122_20())
  }
  
  ids_completed <- unique(keyboard$user_id)
  
  # end load ####
  
  # new variables ####
  
  keyboard$time_diff <- 0
  keyboard$press_time_diff <- 0
  keyboard$pressure_var <- 0.0 
  keyboard$space_x <- 0.0
  keyboard$space_y <- 0.0
  keyboard$velocity_x <- 0.0
  keyboard$velocity_y <- 0.0
  
  for (id in ids_completed){
    
    id_indices <- which(keyboard$user_id == id)
    
    current_action <- -1
    
    for (indice in id_indices){
      
      former_indice <- indice - 1
      
      # app_action
      if(current_action != keyboard[indice]$app_action){
        keyboard[indice]$time_diff <- 0
        current_action <- keyboard[indice]$app_action
      } else{
        keyboard[indice]$time_diff <- diff(
          keyboard[former_indice:indice]$uptime)
      }
      
      # event_type_pointer
      current_event_type <- keyboard[indice]$pointer_event_type
      if (current_event_type == 1){ 
        keyboard[indice]$press_time_diff <- 0
        keyboard[indice]$pressure_var <- 0.0
        keyboard[indice]$velocity_x <- 0.0
        keyboard[indice]$velocity_y <- 0.0
      } else {
        
        keyboard[indice]$press_time_diff <- diff(
          keyboard[former_indice:indice]$uptime)
        
        keyboard[indice]$pressure_var <- diff(
          keyboard[former_indice:indice]$pressure) /
          keyboard[indice]$press_time_diff
        
        keyboard[indice]$space_x <- diff(keyboard[former_indice:indice]$x)
        
        keyboard[indice]$space_y <- diff(keyboard[former_indice:indice]$y)
        
        keyboard[indice]$velocity_x <- diff(
          keyboard[former_indice:indice]$space_x)/
          keyboard[indice]$press_time_diff
        
        keyboard[indice]$velocity_y <- diff(
          keyboard[former_indice:indice]$space_y)/
          keyboard[indice]$press_time_diff
      }
    }
  }
  
  # end new ####
  
  return(keyboard)
  
}
