getKinMats = function(files, upBool_allsubs = NULL, fcTrials_bool_allsubs = NULL) {
  
  library(rmatio)
  
  VFB = 1:30 #This will need to change depending on the experiment!!!
  KFB = 31:60
  VFB2 = 61:62
  AD1 = 63:92
  AD2 = 93:132
  AD3 = 133:172
  AD4 = 173:202
  AD5 = 203:242
  AD6 = 243:282
  AD7 = 283:312
  DAD = 313:362
  
  # LEFT HAND LOOP
  for (i in 1:length(files)) {
    
    
    
    tic(paste("finished processing subject", i, "of", length(files)))
    
    # for debugging
    # load("curFile.rData")
    # files = "C:/Users/philc/OneDrive/Desktop/Post Step 2 files/VDP_VDP_04_postStep2_lh.mat"
    # files = "C:/Users/philc/OneDrive/Desktop/Post Step 2 files/VDP_CTL_01_postStep2_rh.mat"
    
    curFile = read.mat(files[i])
    
    hand = substr(files[i], nchar(files[i])-5, nchar(files[i])-4)
    sub = substr(files[i], nchar(files[i])-26, nchar(files[i])-17)
    group = substr(files[i], nchar(files[i])-22, nchar(files[i])-20)
    
    sortData = curFile$sortData
    onset = curFile$onset
    offset = curFile$offset
    upTrials = curFile$upTrials
    downTrials = curFile$downTrials
    wrongTrial = curFile$wrong_trial
    ide = curFile$ide
    
    #RH_step_2 didn't save upBool or fcTrials or fcTrials_bool, so I create fcTrials_bool if left handed file, then if RH file, function will import from what the left hand did.
    if (hand == "lh") {
      upBool = curFile$upBool
      fcTrials = curFile$fcTrials
      fcTrials_bool = integer(length(ide))
      fcTrials_bool[fcTrials] = 1
    } else {
      upBool = upBool_allsubs$upBool[which(upBool_allsubs$subID == sub)]
      fcTrials_bool = fcTrials_bool_allsubs$fcTrials[which(fcTrials_bool_allsubs$subID == sub)]
    }
    
    #try to save a little memory...
    rm(curFile)
    
    # Hand/Cursor Paths Trial Loop
    for (j in 1:length(sortData$Right_HandX)) {
      if(hand == "rh") { # different onset/offset for different hands
        xPath = unlist(sortData$Right_HandX[j]) # get x and y coordinates
        yPath = unlist(sortData$Right_HandY[j])
      } else {
        xPath = unlist(sortData$Left_HandX[j])
        yPath = unlist(sortData$Left_HandY[j])
      }
      
      if(wrongTrial[j] == 0) {
        xPath_move = xPath[onset[j]:offset[j]] # only movement
        yPath_move = yPath[onset[j]:offset[j]]
      }
      
      Path_int_x = approx(xPath_move, n = 100) # linear interpolation to 100 datapoints
      Path_int_y = approx(yPath_move, n = 100) 
      
      xPath = Path_int_x$y
      yPath = Path_int_y$y
      
      #Create cursor path
      if(hand == "rh" & j %in% c(AD1, AD2, AD3, AD4, AD5, AD6, AD7)) {
        theta = unlist(sortData$TP_TABLE$Rotation_Angle)[5]  # INDEX AND TP COLUMN MAY HAVE TO CHANGE DEPENDING ON EXPERIMENT
      } else {
        theta = 0
      }
      # theta = -45  # just for debugging
      
      global_x = unlist(sortData$TARGET_TABLE$X_GLOBAL)[2]/100 #conv. to m
      global_y = unlist(sortData$TARGET_TABLE$Y_GLOBAL)[2]/100
      
      #this is weird, but right - need to check if rh sortData follows
      if (hand == "lh") {
        global_x = global_x * -1
      }
      
      # Translate to Origin
      xPath_to_origin = xPath - global_x
      yPath_to_origin = yPath - global_y
      
      # Rotate via rotation matrix
      xCursor_at_origin = xPath_to_origin*cos(theta*pi/180) - yPath_to_origin*sin(theta*pi/180)
      yCursor_at_origin = xPath_to_origin*sin(theta*pi/180) + yPath_to_origin*cos(theta*pi/180)
      
      # Translate back to origin
      xCursor = xCursor_at_origin + global_x
      yCursor = yCursor_at_origin + global_y
      
      trial_data = data.frame(sub = sub, group = group, trial = j, xPath = xPath, yPath = yPath, xCursor = xCursor, yCursor= yCursor, wrongTrial = wrongTrial[j], hand = hand, upBool = upBool[j], ide = ide[j], fcTrials = fcTrials_bool[j], sample = 1:100)
      
      if (!exists("session_data")) {
        session_data = trial_data
      } else {
        session_data = rbind(session_data, trial_data)
      }
      
    } 
    
    session_data$block = "the force is strong with this one"        # THIS WILL HAVE TO CHANGE DEPENDING ON EXPERIMENT
    session_data$block[which(session_data$trial %in% VFB)] = "VFB"
    session_data$block[which(session_data$trial %in% KFB)] = "KFB"
    session_data$block[which(session_data$trial %in% VFB2)] = "VFB2"
    session_data$block[which(session_data$trial %in% AD1)] = "AD1"
    session_data$block[which(session_data$trial %in% AD2)] = "AD2"
    session_data$block[which(session_data$trial %in% AD3)] = "AD3"
    session_data$block[which(session_data$trial %in% AD4)] = "AD4"
    session_data$block[which(session_data$trial %in% AD5)] = "AD5"
    session_data$block[which(session_data$trial %in% AD6)] = "AD6"
    session_data$block[which(session_data$trial %in% AD7)] = "AD7"
    session_data$block[which(session_data$trial %in% DAD)] = "DAD"
    
    session_data$wrongTrial[which(session_data$block == "VFB2")] = 1
    
    #get mean IDE for KFB block
    ide_mean_KFB = as.numeric(session_data %>%
                                filter(block == "KFB" & wrongTrial == 0 & fcTrials == 0) %>%
                                #group_by(sample) %>%
                                summarize_at("ide", mean))*-1 #flip sign so that ide is reversed by rotation matrix
    
    #Baseline correct lh handpaths and cursor paths via rotation matrix
    session_data = session_data %>%
      mutate(xPath_BC = ((xPath - global_x)*cos(ide_mean_KFB*pi/180) - (yPath - global_y)*sin(ide_mean_KFB*pi/180)) + global_x) %>%
      mutate(yPath_BC = ((xPath - global_x)*sin(ide_mean_KFB*pi/180) + (yPath - global_y)*cos(ide_mean_KFB*pi/180)) + global_y) %>%
      mutate(xCursor_BC = ((xCursor - global_x)*cos(ide_mean_KFB*pi/180) - (yCursor - global_y)*sin(ide_mean_KFB*pi/180)) + global_x) %>%
      mutate(yCursor_BC = ((xCursor - global_x)*sin(ide_mean_KFB*pi/180) + (yCursor - global_y)*cos(ide_mean_KFB*pi/180)) + global_y)
    
    
    # Need to subtract global y from all y points so that participants who have had the workspace shifted all end up in the same position
    session_data = session_data %>%
      mutate(yPath_atGlobal = yPath - global_y) %>%
      mutate(yCursor_atGlobal = yCursor - global_y) %>%
      mutate(yPath_BC_atGlobal = yPath_BC - global_y) %>%
      mutate(yCursor_BC_atGlobal = yCursor_BC - global_y)
    
    session_data_blockMean = session_data %>%
      filter(wrongTrial == 0 & fcTrials == 0) %>%
      group_by(sub, group, block, sample, upBool, hand) %>%
      #summarize_at(c("xPath", "yPath", "xCursor", "yCursor"), mean)
      summarize_all(list(mean = mean, sd = sd), na.rm = TRUE)
    
    if (!exists("fcTrials_bool_allsubs")) {
      fcTrials_bool_allsubs = data.frame(fcTrials = fcTrials_bool, subID = sub)
    } else {
      fcTrials_bool_allsubs_temp = data.frame(fcTrials = fcTrials_bool, subID = sub)
      fcTrials_bool_allsubs = rbind(fcTrials_bool_allsubs, fcTrials_bool_allsubs_temp)
    }
    
    if (!exists("upBool_allsubs")) {
      upBool_allsubs = data.frame(upBool = upBool, subID = sub)
    } else {
      upBool_allsubs_temp = data.frame(upBool = upBool, subID = sub)
      upBool_allsubs = rbind(upBool_allsubs, upBool_allsubs_temp)
    }
    
    if (!exists("full_data")) {
      full_data = session_data_blockMean
    } else {
      full_data = rbind(full_data, session_data_blockMean)
    }
    
    rm(session_data) # Delete session_data so that new participant can be run...I know this is not the best way but whatever.
    
    toc()
    
  }
  
  # ggplot(data = filter(session_data, trial == 1))+
  #   geom_path(aes(x = xPath, y = yPath))+
  #   geom_path(aes(x = xCursor, y = yCursor), color = 'red') +
  #   geom_path(aes(x = xPath_BC, y = yPath_BC), color = "blue") +
  #   geom_path(aes(x = xCursor_BC, y = yCursor_BC), color = "green") +
  #   coord_fixed()
  
  up_full_data = filter(full_data, upBool == 1)
  down_full_data = filter(full_data, upBool == 0)
  
  return(list(up_full_data, down_full_data, upBool_allsubs, fcTrials_bool_allsubs))
}



