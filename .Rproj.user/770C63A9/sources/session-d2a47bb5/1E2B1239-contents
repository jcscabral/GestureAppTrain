# Main reference: (Buriro et al., 2017)

#
# Second step evaluating according sample A, B, C.
# Parameters:
#         |  USE_SENSOR_DATA  |   LOAD_KEYBOARD | LOAD_KEYBOARD_FIXED 
# Group A |       F           |       T         |       T
# Group B |       F           |       T         |       F
# Group C |       T           |       T         |       F
# Group X*|       T           |       F         |       F
#
# * Actually Group "X" doesn't exist. That is the scenario with only sensors

# set group here
GROUP <- 'C'

# Set Parameters:
if(GROUP == 'A'){
  USE_SENSOR_DATA <-F
  LOAD_KEYBOARD_FIXED <- T  
}
if(GROUP == 'B'){
  USE_SENSOR_DATA <-F
  LOAD_KEYBOARD_FIXED <- F  
}
if(GROUP == 'C' || GROUP == 'X'){
  USE_SENSOR_DATA <-T
  LOAD_KEYBOARD_FIXED <- F  
}

############
# KEYBOARD #
############

keyboard <- loadKeyboard(LOAD_KEYBOARD_FIXED)
ids_completed <- sort(unique(keyboard$user_id))

# plot #######

# keyboard.login <- keyboard %>% filter(
#   app_action %in% c(KEYBOARD_LOGIN))
# 
# keyboard.user <- keyboard.login[(keyboard.login$user_id == 74)]
# 
# p0 <- ggplot(keyboard.user, 
#   aes(uptime, pressure)) + geom_point()
# 
# keyboard.user <- NULL
# keyboard.login <- NULL

# end plot####

keyboard_login <- keyboard %>%
  filter(app_action == KEYBOARD_LOGIN | app_action == KEYBOARD_AUTH)

##########
# SENSOR #
##########

if (USE_SENSOR_DATA == T){
  sensors.login <- loadSensorsLogin()
}

###############
# STATS CARDS #
###############

# sensor stats ####
sensorStats <- function(dfdata){
  
  sensor_stats <- dfdata %>%
    mutate(
      acc_x = ifelse(sensor_type == 1, x, NA) ,
      acc_y = ifelse(sensor_type == 1, y, NA) ,
      acc_z = ifelse(sensor_type == 1, z, NA) ,
      acc_w = ifelse(sensor_type == 1, w, NA) ,
      mag_x = ifelse(sensor_type == 2, x, NA) ,
      mag_y = ifelse(sensor_type == 2, y, NA) ,
      mag_z = ifelse(sensor_type == 2, z, NA) ,
      mag_w = ifelse(sensor_type == 2, w, NA) ,
      gyr_x = ifelse(sensor_type == 4, x, NA) ,
      gyr_y = ifelse(sensor_type == 4, y, NA) ,
      gyr_z = ifelse(sensor_type == 4, z, NA) ,
      gyr_w = ifelse(sensor_type == 4, w, NA)
    ) %>%
    summarise(
      # Acc
      # x
      accx_md = median(acc_x, na.rm = T),
      accx_fq = qt(acc_x, 1),
      accx_tq = qt(acc_x, 3),
      accx_ir = IQR(acc_x, na.rm = T),
      accx_am = mean(acc_x, na.rm = T),
      accx_vr = var(acc_x, na.rm = T),
      accx_sd = sd(acc_x, na.rm = T),
      accx_cv = cv(acc_x, na.rm = T),
      accx_se = se(acc_x),
      accx_mn = min(acc_x, na.rm = T),
      accx_mx = max(acc_x, na.rm = T),
      accx_qm = qm(acc_x),
      accx_rg = rg(acc_x),
      accx_sk = skewness(acc_x, na.rm = T),
      accx_ku = kurtosis(acc_x, na.rm = T),
      accx_sm = sum(acc_x, na.rm = T),
      # y
      accy_md = median(acc_y, na.rm = T),
      accy_fq = qt(acc_y, 1),
      accy_tq = qt(acc_y, 3),
      accy_ir = IQR(acc_y, na.rm = T),
      accy_am = mean(acc_y, na.rm = T),
      accy_vr = var(acc_y, na.rm = T),
      accy_sd = sd(acc_y, na.rm = T),
      accy_cv = cv(acc_y, na.rm = T),
      accy_se = se(acc_y),
      accy_mn = min(acc_y, na.rm = T),
      accy_mx = max(acc_y, na.rm = T),
      accy_qm = qm(acc_y),
      accy_rg = rg(acc_y),
      accy_sk = skewness(acc_y, na.rm = T),
      accy_ku = kurtosis(acc_y, na.rm = T),
      accy_sm = sum(acc_y, na.rm = T),
      # z
      accz_md = median(acc_z, na.rm = T),
      accz_fq = qt(acc_z, 1),
      accz_tq = qt(acc_z, 3),
      accz_ir = IQR(acc_z, na.rm = T),
      accz_am = mean(acc_z, na.rm = T),
      accz_vr = var(acc_z, na.rm = T),
      accz_sd = sd(acc_z, na.rm = T),
      accz_cv = cv(acc_z, na.rm = T),
      accz_se = se(acc_z),
      accz_mn = min(acc_z, na.rm = T),
      accz_mx = max(acc_z, na.rm = T),
      accz_qm = qm(acc_z),
      accz_rg = rg(acc_z),
      accz_sk = skewness(acc_z, na.rm = T),
      accz_ku = kurtosis(acc_z, na.rm = T),
      accz_sm = sum(acc_z, na.rm = T),
      # w
      accw_md = median(acc_w, na.rm = T),
      accw_fq = qt(acc_w, 1),
      accw_tq = qt(acc_w, 3),
      accw_ir = IQR(acc_w, na.rm = T),
      accw_am = mean(acc_w, na.rm = T),
      accw_vr = var(acc_w, na.rm = T),
      accw_sd = sd(acc_w, na.rm = T),
      accw_cv = cv(acc_w, na.rm = T),
      accw_se = se(acc_w),
      accw_mn = min(acc_w, na.rm = T),
      accw_mx = max(acc_w, na.rm = T),
      accw_qm = qm(acc_w),
      accw_rg = rg(acc_w),
      accw_sk = skewness(acc_w, na.rm = T),
      accw_ku = kurtosis(acc_w, na.rm = T),
      accw_sm = sum(acc_w, na.rm = T),
      # all vectors 
      accsdp_mg = mg3(acc_x,acc_y,acc_z),
      #Mag
      # x
      magx_md = median(mag_x, na.rm = T),
      magx_fq = qt(mag_x, 1),
      magx_tq = qt(mag_x, 3),
      magx_ir = IQR(mag_x, na.rm = T),
      magx_am = mean(mag_x, na.rm = T),
      magx_vr = var(mag_x, na.rm = T),
      magx_sd = sd(mag_x, na.rm = T),
      magx_cv = cv(mag_x, na.rm = T),
      magx_se = se(mag_x),
      magx_mn = min(mag_x, na.rm = T),
      magx_mx = max(mag_x, na.rm = T),
      magx_qm = qm(mag_x),
      magx_rg = rg(mag_x),
      magx_sk = skewness(mag_x, na.rm = T),
      magx_ku = kurtosis(mag_x, na.rm = T),
      magx_sm = sum(mag_x, na.rm = T),
      # y
      magy_md = median(mag_y, na.rm = T),
      magy_fq = qt(mag_y, 1),
      magy_tq = qt(mag_y, 3),
      magy_ir = IQR(mag_y, na.rm = T),
      magy_am = mean(mag_y, na.rm = T),
      magy_vr = var(mag_y, na.rm = T),
      magy_sd = sd(mag_y, na.rm = T),
      magy_cv = cv(mag_y, na.rm = T),
      magy_se = se(mag_y),
      magy_mn = min(mag_y, na.rm = T),
      magy_mx = max(mag_y, na.rm = T),
      magy_qm = qm(mag_y),
      magy_rg = rg(mag_y),
      magy_sk = skewness(mag_y, na.rm = T),
      magy_ku = kurtosis(mag_y, na.rm = T),
      magy_sm = sum(mag_y, na.rm = T),
      # z
      magz_md = median(mag_z, na.rm = T),
      magz_fq = qt(mag_z, 1),
      magz_tq = qt(mag_z, 3),
      magz_ir = IQR(mag_z, na.rm = T),
      magz_am = mean(mag_z, na.rm = T),
      magz_vr = var(mag_z, na.rm = T),
      magz_sd = sd(mag_z, na.rm = T),
      magz_cv = cv(mag_z, na.rm = T),
      magz_se = se(mag_z),
      magz_mn = min(mag_z, na.rm = T),
      magz_mx = max(mag_z, na.rm = T),
      magz_qm = qm(mag_z),
      magz_rg = rg(mag_z),
      magz_sk = skewness(mag_z, na.rm = T),
      magz_ku = kurtosis(mag_z, na.rm = T),
      magz_sm = sum(mag_z, na.rm = T),
      # w
      magw_md = median(mag_w, na.rm = T),
      magw_fq = qt(mag_w, 1),
      magw_tq = qt(mag_w, 3),
      magw_ir = IQR(mag_w, na.rm = T),
      magw_am = mean(mag_w, na.rm = T),
      magw_vr = var(mag_w, na.rm = T),
      magw_sd = sd(mag_w, na.rm = T),
      magw_cv = cv(mag_w, na.rm = T),
      magw_se = se(mag_w),
      magw_mn = min(mag_w, na.rm = T),
      magw_mx = max(mag_w, na.rm = T),
      magw_qm = qm(mag_w),
      magw_rg = rg(mag_w),
      magw_sk = skewness(mag_w, na.rm = T),
      magw_ku = kurtosis(mag_w, na.rm = T),
      magw_sm = sum(mag_w, na.rm = T),
      # all vectors 
      magsdp_mg = mg3(mag_x,mag_y,mag_z),
      # gyr 
      # x
      gyrx_md = median(gyr_x, na.rm = T),
      gyrx_fq = qt(gyr_x, 1),
      gyrx_tq = qt(gyr_x, 3),
      gyrx_ir = IQR(gyr_x, na.rm = T),
      gyrx_am = mean(gyr_x, na.rm = T),
      gyrx_vr = var(gyr_x, na.rm = T),
      gyrx_sd = sd(gyr_x, na.rm = T),
      gyrx_cv = cv(gyr_x, na.rm = T),
      gyrx_se = se(gyr_x),
      gyrx_mn = min(gyr_x, na.rm = T),
      gyrx_mx = max(gyr_x, na.rm = T),
      gyrx_qm = qm(gyr_x),
      gyrx_rg = rg(gyr_x),
      gyrx_sk = skewness(gyr_x, na.rm = T),
      gyrx_ku = kurtosis(gyr_x, na.rm = T),
      gyrx_sm = sum(gyr_x, na.rm = T),
      # y
      gyry_md = median(gyr_y, na.rm = T),
      gyry_fq = qt(gyr_y, 1),
      gyry_tq = qt(gyr_y, 3),
      gyry_ir = IQR(gyr_y, na.rm = T),
      gyry_am = mean(gyr_y, na.rm = T),
      gyry_vr = var(gyr_y, na.rm = T),
      gyry_sd = sd(gyr_y, na.rm = T),
      gyry_cv = cv(gyr_y, na.rm = T),
      gyry_se = se(gyr_y),
      gyry_mn = min(gyr_y, na.rm = T),
      gyry_mx = max(gyr_y, na.rm = T),
      gyry_qm = qm(gyr_y),
      gyry_rg = rg(gyr_y),
      gyry_sk = skewness(gyr_y, na.rm = T),
      gyry_ku = kurtosis(gyr_y, na.rm = T),
      gyry_sm = sum(gyr_y, na.rm = T),
      # z
      gyrz_md = median(gyr_z, na.rm = T),
      gyrz_fq = qt(gyr_z, 1),
      gyrz_tq = qt(gyr_z, 3),
      gyrz_ir = IQR(gyr_z, na.rm = T),
      gyrz_am = mean(gyr_z, na.rm = T),
      gyrz_vr = var(gyr_z, na.rm = T),
      gyrz_sd = sd(gyr_z, na.rm = T),
      gyrz_cv = cv(gyr_z, na.rm = T),
      gyrz_se = se(gyr_z),
      gyrz_mn = min(gyr_z, na.rm = T),
      gyrz_mx = max(gyr_z, na.rm = T),
      gyrz_qm = qm(gyr_z),
      gyrz_rg = rg(gyr_z),
      gyrz_sk = skewness(gyr_z, na.rm = T),
      gyrz_ku = kurtosis(gyr_z, na.rm = T),
      gyrz_sm = sum(gyr_z, na.rm = T),
      # w
      gyrw_md = median(gyr_w, na.rm = T),
      gyrw_fq = qt(gyr_w, 1),
      gyrw_tq = qt(gyr_w, 3),
      gyrw_ir = IQR(gyr_w, na.rm = T),
      gyrw_am = mean(gyr_w, na.rm = T),
      gyrw_vr = var(gyr_w, na.rm = T),
      gyrw_sd = sd(gyr_w, na.rm = T),
      gyrw_cv = cv(gyr_w, na.rm = T),
      gyrw_se = se(gyr_w),
      gyrw_mn = min(gyr_w, na.rm = T),
      gyrw_mx = max(gyr_w, na.rm = T),
      gyrw_qm = qm(gyr_w),
      gyrw_rg = rg(gyr_w),
      gyrw_sk = skewness(gyr_w, na.rm = T),
      gyrw_ku = kurtosis(gyr_w, na.rm = T),
      gyrw_sm = sum(gyr_w, na.rm = T),
      # all vectors 
      gyrsdp_mg = mg3(gyr_x,gyr_y,gyr_z)
    )
  return(sensor_stats)
}  

# DigitStats stats ####
digitStats <- function(dfdata, digit){
  card_stats <- dfdata %>%
    filter(character == digit) %>%
    mutate(
      fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
      dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
    ) %>%
    summarise(
      # DT Dwelling Time
      dt_md = median(dwell_time, na.rm = T),
      dt_fq = qt(dwell_time, 1),
      dt_tq = qt(dwell_time, 3),
      dt_ir = IQR(dwell_time, na.rm = T),
      dt_ma = mean(dwell_time, na.rm = T),
      dt_vr = var(dwell_time, na.rm = T),
      dt_sd = sd(dwell_time, na.rm = T),
      dt_cv = cv(dwell_time, na.rm = T),
      dt_se = se(dwell_time),
      dt_mn = min(dwell_time, na.rm = T) ,
      dt_mx = max(dwell_time, na.rm = T) ,
      dt_qm = qm(dwell_time) ,
      dt_rg = rg(dwell_time) ,
      dt_sk = skewness(dwell_time) ,
      dt_ku = kurtosis(dwell_time) ,
      # FT Flight Time
      ft_md = median(fly_time, na.rm = T),
      ft_fq = qt(fly_time, 1),
      ft_tq = qt(fly_time, 3),
      ft_ir = IQR(fly_time, na.rm = T),
      ft_am = mean(fly_time, na.rm = T),
      ft_vr = var(fly_time, na.rm = T),
      ft_sd = sd(fly_time, na.rm = T),
      ft_cv = cv(fly_time, na.rm = T),
      ft_se = se(fly_time),
      ft_mn = min(fly_time , na.rm = T) ,
      ft_mx = max(fly_time, na.rm = T) ,
      ft_qm = qm(fly_time) ,
      ft_rg = rg(fly_time) ,
      ft_sk = skewness(fly_time) ,
      ft_ku = kurtosis(fly_time) ,
      # Pressure
      ps_md = median(pressure),
      ps_fq = qt(pressure, 1),
      ps_tq = qt(pressure, 3),
      ps_ir = IQR(pressure, na.rm = T),
      ps_am = mean(pressure, na.rm = T),
      ps_var = var(pressure, na.rm = T),
      ps_sd = sd(pressure, na.rm = T),
      ps_cv = cv(pressure, na.rm = T),
      ps_se = se(pressure),
      ps_mn = min(pressure),
      ps_mx = max(pressure),
      ps_qm = qm(pressure) ,
      ps_rg = rg(pressure) ,
      ps_sk = skewness(pressure) ,
      ps_ku = kurtosis(pressure) ,
      # Spacial x
      x_md = median(x, na.rm = T),
      x_fq = qt(x, 1),
      x_tq = qt(x, 3),
      x_ir = IQR(x, na.rm = T),
      x_am = mean(x, na.rm = T),
      x_vr = var(x, na.rm = T),
      x_sd = sd(x, na.rm = T),
      x_cv = cv(x, na.rm = T),
      x_se = se(x),
      x_mn = min(x, na.rm = T) ,
      x_mx = max(x, na.rm = T) ,
      x_qm = qm(x) ,
      x_rg = rg(x),
      x_sk =  skewness(x) ,
      x_ku = kurtosis(x) ,
      # Spacial y
      y_md = median(y, na.rm = T),
      y_fq = qt(y, 1),
      y_tq = qt(y, 3),
      y_ir = IQR(y, na.rm = T),
      y_am = mean(y, na.rm = T),
      y_vr = var(y),
      y_sd = sd(y, na.rm = T),
      y_cv = cv(y, na.rm = T),
      y_se = se(y),
      y_mn = min(y, na.rm = T) ,
      y_mx = max(y, na.rm = T) ,
      y_qm = qm(y),
      y_rg =  rg(y) ,
      y_sk =  skewness(y) ,    
      y_ku = kurtosis(y)
    )
  return(card_stats)
}

# Renaming chars ####
col_char5 <- function(x){
  return(paste(x, "_5", '', sep= ''))
}
col_char4 <- function(x){
  return(paste(x, "_4", '', sep= ''))
}
col_char8 <- function(x){
  return(paste(x, "_8", '', sep= ''))
}
col_char3 <- function(x){
  return(paste(x, "_3", '', sep= ''))
}
col_char1 <- function(x){
  return(paste(x, "_1", '', sep= ''))
}
col_char7 <- function(x){
  return(paste(x, "_7", '', sep= ''))
}
col_char9 <- function(x){
  return(paste(x, "_9", '', sep= ''))
}
col_char0 <- function(x){
  return(paste(x, "_0", '', sep= ''))
}

# KeyboardStats stats ####
keyboardStats <- function(dbase, secao = T){
  
  groupby <- c("user_id")
  ini.range <- 3
  if (secao == T){
    groupby <- c("user_id","action_number")
    ini.range <- 4
  }
  
  desc_login_char_5 <- digitStats(dbase, 5)
  desc_login_char_4 <- digitStats(dbase, 4)
  desc_login_char_8 <- digitStats(dbase, 8)
  desc_login_char_3 <- digitStats(dbase, 3)
  desc_login_char_1 <- digitStats(dbase, 1)
  desc_login_char_7 <- digitStats(dbase, 7)
  desc_login_char_9 <- digitStats(dbase, 9)
  desc_login_char_0 <- digitStats(dbase, 0)
  
  col.names <- names(desc_login_char_5)
  col.names <- col.names[ini.range:length(col.names)]
  
  desc_login_char_5 <- desc_login_char_5 %>% 
    rename_with(.fn = col_char5, .cols = col.names)
  desc_login_char_4 <- desc_login_char_4 %>% 
    rename_with(.fn = col_char4, .cols = col.names)
  desc_login_char_8 <- desc_login_char_8 %>% 
    rename_with(.fn = col_char8, .cols = col.names)
  desc_login_char_3 <- desc_login_char_3 %>% 
    rename_with(.fn = col_char3, .cols = col.names)
  desc_login_char_1 <- desc_login_char_1 %>% 
    rename_with(.fn = col_char1, .cols = col.names)
  desc_login_char_7 <- desc_login_char_7 %>% 
    rename_with(.fn = col_char7, .cols = col.names)
  desc_login_char_9 <- desc_login_char_9 %>% 
    rename_with(.fn = col_char9, .cols = col.names)
  desc_login_char_0 <- desc_login_char_0 %>% 
    rename_with(.fn = col_char0, .cols = col.names)
  
  desc_login_chars <- inner_join(
    desc_login_char_5, desc_login_char_4, by = groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_8, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_3, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_1, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_7, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_9, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_0, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  
  return(desc_login_chars)
  
}

infoGainCols <- function(dbase){
  
  cols.not.na <- names(which(colSums(is.na(dbase)) == 0))
  dbase <- dbase[cols.not.na]
  
  threshold <- length(ids_completed)/ (ncol(dbase) - 1)
  result <- FSelectorRcpp::information_gain(
    formula = user_id ~ .,
    data = dbase ,
    type = 'infogain'
  )
  cols_ranked  <- result %>% 
    filter(importance >= threshold) %>% select(attributes)
  cols_perc  <- result %>% 
    filter(importance >= threshold) %>% select(importance)
  #return(as_vector(cols_ranked))
  return(
     list(
      "cols_ranked" = as_vector(cols_ranked),
      "cols_perc" = as_vector(cols_perc)
    )
  )
}


# CROSS VALIDATION #####

# results 
df.results <- data.frame(
  cutoff = double(),
  pc_train = double(),
  iter = integer(),
  num_users = integer(),
  num_attackers = integer(),
  num_variables = integer(),
  login_pos = integer(),
  login_neg = integer(),
  login_pos_prob = integer(),
  login_neg_prob = integer(),
  attack_pos = integer(),
  attack_neg = integer(),
  grd_login_pos = integer(),
  grd_login_neg = integer(),
  grd_login_pos_prob = integer(),
  grd_login_neg_prob = integer(),
  grd_attack_pos = integer(),
  grd_attack_neg = integer()
)

# variables
df.variables <- data.frame(
  iter = integer(),
  no_var = character(),
  pc_var = double()
)


# I. set sessions 

NUM_SESSIONS <- 5

# II - split data: test with only real unknown intruder 

ids <- sample(ids_completed)

PC_30 <- 0.3
PC_50 <- 0.5
PC_70 <- 0.7
PC_90 <- 0.9
CUTOFFS <- c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5) 
percents <-c(PC_30, PC_50, PC_70, PC_90) 

cutoff <- 0.5
iters <- c(0,1)
row <- 1
for (pc_train in percents){
  
  # iter
  # 0: train/test:[1-5], val:6 
  # 1: train/test:[1-5], val:7
  
  for (iter in iters){
    
    # split train and attack's validation
    session_start <- 1 + iter
    session_end <- NUM_SESSIONS + iter
    
    ind <- round(length(ids) * pc_train)
    ids.train <-ids[1:ind]
    ids.val <-ids[(ind+1):length(ids)]
    
    num_users <- length(ids.train)
    num_attackers <- length(ids.val)
    
    
    ##########################
    # I- template (training) #
    ##########################
    
    keyboard.train <- keyboard_login %>% 
      filter(user_id %in% ids.train)
    
    
    if (USE_SENSOR_DATA == T){
      sensors.train <- sensors.login  %>% 
        filter(user_id %in% ids.train)
    }
    
    keyboard.grouped <- keyboard.train %>%
      filter(action_number >= session_start,
             action_number <= session_end) %>%
      group_by(user_id, character, action_number)
    
    keyboard.stats <- keyboardStats(keyboard.grouped)
    #info.keyboard.cols <- infoGainCols(keyboard.stats)
    info.gaing <- infoGainCols(keyboard.stats)
    info.keyboard.cols <- info.gaing["cols_ranked"]$cols_ranked
    info.keyboard.perc <- info.gaing["cols_perc"]$cols_perc
    
    keyboard.stats <- keyboard.stats[c("user_id", info.keyboard.cols)]
    
    info.cols <- info.keyboard.cols
    info.perc <- info.keyboard.perc
    
    info.sensor.cols = NULL
    if (USE_SENSOR_DATA == T){
      
      sensors.train.grouped <- sensors.train %>%
        filter(action_number >= session_start,
               action_number <= session_end) %>%
        group_by(user_id, action_number)
      
      sensors.stats <- sensorStats(sensors.train.grouped)
      #info.sensor.cols <- infoGainCols(sensors.stats)
      info.gaing <- infoGainCols(sensors.stats)
      info.sensor.cols <- info.gaing["cols_ranked"]$cols_ranked
      info.sensor.perc <- info.gaing["cols_perc"]$cols_perc
      
      sensors.stats <- sensors.stats[info.sensor.cols]
      if (GROUP != 'X'){
        keyboard.stats <- bind_cols(keyboard.stats, sensors.stats)
      } else {
        keyboard.stats <- bind_cols(keyboard.stats["user_id"], sensors.stats)
      }
      
      
      info.cols <- c(info.cols, info.sensor.cols)
      info.perc <-c(info.perc, info.sensor.perc)
      
    }
    
    df.variables <- rbind(
      df.variables,
      data.frame(
        iter = row, 
        no_var = info.cols,
        pc_var = info.perc)
    )
    
    card.train <- keyboard.stats
    num_variables <- length(info.cols) -1
    
    
    ##############
    # II - login #
    ##############
    
    keyboard.login <- keyboard.train %>%
      filter(action_number == session_end + 1) %>%
        group_by(user_id, character, action_number)
    keyboard.login.stats <- keyboardStats(keyboard.login)
    keyboard.login.stats <- keyboard.login.stats[
      c("user_id", info.keyboard.cols)]
    
    if (USE_SENSOR_DATA == T){
      sensor.login.grouped <- sensors.train %>%
        filter(action_number == session_end + 1) %>%
        group_by(user_id, action_number)
      
      sensors.login.stats <- sensorStats(sensor.login.grouped)
      sensors.login.stats <- sensors.login.stats[info.sensor.cols]
      
      keyboard.login.stats <- bind_cols(keyboard.login.stats, 
          sensors.login.stats)
    }
    card.login <- keyboard.login.stats
    
    
    ####################
    # III - validation #
    ####################
    
    keyboard.val <- keyboard_login %>% 
      filter(user_id %in% ids.val)
    
    if (USE_SENSOR_DATA == T){
      sensors.val <- sensors.login %>% 
        filter(user_id %in% ids.val)
    }
    
    keyboard.val.grouped <- keyboard.val %>%
       group_by(user_id, character, action_number)
    keyboard.val.stats <- keyboardStats(keyboard.val.grouped, secao = T)
    keyboard.val.stats <- keyboard.val.stats[c("user_id", info.keyboard.cols)]
    
    if (USE_SENSOR_DATA == T){
      sensor.val.grouped <- sensors.val %>% group_by(user_id, action_number)
      sensors.val.stats <- sensorStats(sensor.val.grouped)
      sensors.val.stats <- sensors.val.stats[info.sensor.cols]
      keyboard.val.stats <- bind_cols(keyboard.val.stats, sensors.val.stats)
    }
    
    card.val <- keyboard.val.stats
    
    ###############
    ### TRAIN #####
    ###############
    
    x_train <- card.train[,-1]
    y_train <- as.factor(card.train$user_id)
    
    # write.csv(x_train,'train_x')
    # write.csv(y_train,'train_y', pc_train, ite)
    x_login <- card.login[,-1]
    y_login <- as.factor(card.login$user_id)
    
    x_val <- card.val[,-1]
    y_val <- as.factor(card.val$user_id)
    
    # randomForest class w/ 5 sections
    rf <- randomForest(
        x_train, 
        y_train, 
        importance = T)
    
    # with k-fold
    df.train <- card.train
    df.train$user_id <- paste("u", df.train$user_id, sep = "")
    control <- trainControl(method = "repeatedcv", 
                            number = 10 ,
                            repeats = 2,
                            search = "grid",
                            classProbs = T)
    
    gridsearch <- train(
      user_id ~ .,
      data = df.train ,
      method = 'rf',
      ntree = 100,
      trControl = control
    )

    #for (cutoff in CUTOFFS){
      
      # I. login try
      
      pred.class.login <- predict(object = rf, x_login, type ="class")
      login_pos <- sum(pred.class.login == y_login)
      login_neg <- sum(pred.class.login != y_login)
      
      pred.prob.login <- predict(object = rf, x_login, type ="prob")
      max.probs.login <- apply(pred.prob.login, 1, max)
      matched <- pred.class.login == y_login
      login_pos_prob <- sum(matched[max.probs.login> cutoff])
      login_neg_prob <- length(pred.class.login) - login_pos_prob
      
      
      # II. attack try
      pred.prob.val <- predict(object = rf, x_val, type ="prob")
      max.probs <- apply(pred.prob.val, 1, max, na.rm=TRUE)
      
      attack_pos <- sum(max.probs > cutoff)
      attack_neg <- length(max.probs) - attack_pos
      
        
      # II. gridsearch
      
      # login
      
      df.login.y <- paste("u", y_login, sep = "")
      
      pred.class.kfold <- predict(gridsearch, x_login)
      grd_login_pos <- sum(pred.class.kfold == df.login.y)
      grd_login_neg <- sum(pred.class.kfold != df.login.y)
      grd_login_acc <- grd_login_pos / length(df.login.y)
      
      pred.prob.kfold <- predict(gridsearch, x_login, type = 'prob')
      max.probs.login.kfold <- apply(pred.prob.kfold, 1, max, na.rm=TRUE)
      matched.kfold <- pred.class.kfold == df.login.y
      
      grd_login_pos_prob <- sum(matched.kfold[max.probs.login.kfold> cutoff])
      grd_login_neg_prob <- length(df.login.y) - grd_login_pos_prob
      
      # attack 
      
      pred.prob.attack.kfold <- predict(gridsearch, x_val, type = 'prob')
      max.probs.kfold <- apply(pred.prob.attack.kfold, 1, max, na.rm=TRUE) # max by row
      
      grd_attack_pos <- sum(max.probs.kfold > cutoff)
      grd_attack_neg <- length(max.probs.kfold) - grd_attack_pos
      
      
      df.results[row,] <- c(cutoff,
                            pc_train, 
                            iter, 
                            num_users,
                            num_attackers,
                            num_variables,
                            login_pos,
                            login_neg,
                            login_pos_prob,
                            login_neg_prob,
                            attack_pos,
                            attack_neg,
                            grd_login_pos,
                            grd_login_neg,
                            grd_login_pos_prob,
                            grd_login_neg_prob,
                            grd_attack_pos,
                            grd_attack_neg
      )
      row <- row + 1
    #}
  }
}

# save results

fileresults <- paste(GROUP,'-results', sep ='')
filevariables <- paste(GROUP,'-variables', sep = '')

write.csv(df.results, fileresults)
write.csv(df.variables, filevariables)