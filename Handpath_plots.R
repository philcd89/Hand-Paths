# This code reads in post-step 2 hand path data for cleaned KINARM trials and generates hand path plots using ggplot2.  This code was previously written in MATLAB.

# Written by Phil Desrochers 11-19-2020.  Stored in github repository at 


# Setup -------------------------------------------------------------------

library(tidyverse)
library(tcltk)
library(tictoc)
library(ggforce)
library(patchwork)

setwd("C:/Users/philc/OneDrive/Desktop/Post Step 2 files")


# Choose Single Participant or ALL participants ---------------------------

# If user chooses to run all participants, this code will aggregate all of the post-step-2.mat files in the working directory.

doAllSubs = tk_messageBox(title = "Process all participants?", icon = "question", message = "Aggregate handpaths for all subjects in current directory?", type = "yesno")

if (doAllSubs == "no") {
  lh_files = tk_choose.files(default = "C:/Users/philc/OneDrive/Desktop/Post Step 2 files/.mat", caption = "Choose subjects to process (both hands will be run)", filter = matrix(c("lh_matfiles", "lh.mat"), 1, 2, byrow = TRUE))
} else {
  lh_files = list.files(pattern = "lh.mat", full.names = TRUE)
}

# get RH filenames
rh_files = gsub("lh", "rh", lh_files)

# Read in Data ------------------------------------------------------------

source("C:/Users/philc/Google Drive/MNL/PROJECTS/Combined_Vis_Dyn/Kinematic Data/Post_Step_3/Hand-Paths/func_getKinMats.R")

#devtools::install_github("collectivemedia/tictoc")

#NOTE: rh Step 2 did not save fcTrials or upBool, so I'm getting them from the LH file and passing them into the RH function
tic("finished getting data")

# Get LH
lh_data = getKinMats(lh_files) # This will take a while, especially for all subjects.

lh_up_full_data = lh_data[[1]]
lh_down_full_data = lh_data[[2]]
upBool = lh_data[[3]]
fcTrials_bool = lh_data[[4]]

rh_data = getKinMats(rh_files, upBool, fcTrials_bool)
rh_up_full_data = rh_data[[1]]
rh_down_full_data = rh_data[[2]]

toc()


# Plot --------------------------------------------------------------------

CTL_subCount = nlevels(as.factor(as.character(lh_up_full_data$sub[which(lh_up_full_data$group == "CTL")])))
DYN_subCount = nlevels(as.factor(as.character(lh_up_full_data$sub[which(lh_up_full_data$group == "DYN")])))
VIS_subCount = nlevels(as.factor(as.character(lh_up_full_data$sub[which(lh_up_full_data$group == "VIS")])))
VDP_subCount = nlevels(as.factor(as.character(lh_up_full_data$sub[which(lh_up_full_data$group == "VDP")])))

if (CTL_subCount > 1 & DYN_subCount > 1 & VIS_subCount > 1 & VDP_subCount > 1) {
  
  #combine into one dataframe
  lh_up_groupMeans = lh_up_full_data %>%
    ungroup() %>%
    select(block, sample, group, xPath_mean, yPath_mean, xCursor_mean, yCursor_mean, xPath_BC_mean, yPath_BC_mean, xCursor_BC_mean, yCursor_BC_mean, yPath_atGlobal_mean, yCursor_atGlobal_mean, yPath_BC_atGlobal_mean, yCursor_BC_atGlobal_mean) %>%
    group_by(block, sample, group) %>%
    summarize_all(list(mean = mean, sd = sd))
  lh_up_groupMeans$hand = "lh"
  lh_up_groupMeans$movDir = "up"
  
  rh_up_groupMeans = rh_up_full_data %>%
    ungroup() %>%
    select(block, sample, group, xPath_mean, yPath_mean, xCursor_mean, yCursor_mean, xPath_BC_mean, yPath_BC_mean, xCursor_BC_mean, yCursor_BC_mean, yPath_atGlobal_mean, yCursor_atGlobal_mean, yPath_BC_atGlobal_mean, yCursor_BC_atGlobal_mean) %>%
    group_by(block, sample, group) %>%
    summarize_all(list(mean = mean, sd = sd))
  rh_up_groupMeans$hand = "rh"
  rh_up_groupMeans$movDir = "up"
  
  lh_down_groupMeans = lh_down_full_data %>%
    ungroup() %>%
    select(block, sample, group, xPath_mean, yPath_mean, xCursor_mean, yCursor_mean, xPath_BC_mean, yPath_BC_mean, xCursor_BC_mean, yCursor_BC_mean, yPath_atGlobal_mean, yCursor_atGlobal_mean, yPath_BC_atGlobal_mean, yCursor_BC_atGlobal_mean) %>%
    group_by(block, sample, group) %>%
    summarize_all(list(mean = mean, sd = sd))
  lh_down_groupMeans$hand = "lh"
  lh_down_groupMeans$movDir = "down"
  
  rh_down_groupMeans = rh_down_full_data %>%
    ungroup() %>%
    select(block, sample, group, xPath_mean, yPath_mean, xCursor_mean, yCursor_mean, xPath_BC_mean, yPath_BC_mean, xCursor_BC_mean, yCursor_BC_mean, yPath_atGlobal_mean, yCursor_atGlobal_mean, yPath_BC_atGlobal_mean, yCursor_BC_atGlobal_mean) %>%
    group_by(block, sample, group) %>%
    summarize_all(list(mean = mean, sd = sd))
  rh_down_groupMeans$hand = "rh"
  rh_down_groupMeans$movDir = "down"
  
  fulldata_groupMeans = rbind(lh_up_groupMeans, lh_down_groupMeans, rh_up_groupMeans, rh_down_groupMeans)
  
  fulldata_groupMeans$group = factor(fulldata_groupMeans$group, levels = c("CTL", "DYN", "VIS", "VDP"))
  levels(fulldata_groupMeans$group) <- c("Control", "Dyn Pert.", "VM Pert.", "Combo Pert")
  fulldata_groupMeans$block = factor(fulldata_groupMeans$block, levels = c("VFB", "KFB", "AD1", "AD2", "AD3", "AD4", "AD5", "AD6", "AD7", "DAD"))
  fulldata_groupMeans$hand[which(fulldata_groupMeans$hand == "lh")] = "Left Hand"
  fulldata_groupMeans$hand[which(fulldata_groupMeans$hand == "rh")] = "Right Hand"
  
  
  # AND PLOT!!
  lh_early_plot = ggplot(data = filter(fulldata_groupMeans, hand == "Left Hand"))+
    facet_grid(group~hand)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD1" & hand == "Left Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD1" & hand == "Left Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_path(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD1" & hand == "Left Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_path(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD1" & hand == "Left Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_point(aes(x = -0.085, y = -0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = -0.085, y = 0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = -0.085, y = 0), color = "orange", shape = 1, size = 2)+
    scale_x_continuous(limits = c(-0.235, .065))+
    scale_y_continuous(limits = c(-0.15, 0.15))+
    coord_fixed()+
    ylab("y cursor position (m)")+
    xlab("x cursor position (m)")+
    theme_bw()+ 
    theme(strip.background.y = element_blank(),
      strip.text.y = element_blank())
  lh_early_plot
  
  rh_early_plot = ggplot(data = filter(fulldata_groupMeans, hand == "Right Hand"))+
    facet_grid(group~hand)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD1" & hand == "Right Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD1" & hand == "Right Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_path(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD1" & hand == "Right Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_path(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD1" & hand == "Right Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_point(aes(x = 0.085, y = -0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = 0.085, y = 0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = 0.085, y = 0), color = "orange", shape = 1, size = 2)+
    scale_x_continuous(limits = c(-0.065, 0.235))+
    scale_y_continuous(limits = c(-0.15, 0.15))+
    coord_fixed()+
    xlab("x cursor position (m)")+
    theme_bw()+ 
    theme(strip.background.y = element_blank(),
          strip.text.y = element_blank(),
          axis.title.y = element_blank())
  rh_early_plot
  
  lh_late_plot = ggplot(data = filter(fulldata_groupMeans, hand == "Left Hand"))+
    facet_grid(group~hand)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD7" & hand == "Left Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD7" & hand == "Left Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_path(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD7" & hand == "Left Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_path(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD7" & hand == "Left Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_point(aes(x = -0.085, y = -0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = -0.085, y = 0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = -0.085, y = 0), color = "orange", shape = 1, size = 2)+
    scale_x_continuous(limits = c(-0.235, .065))+
    scale_y_continuous(limits = c(-0.15, 0.15))+
    coord_fixed()+
    xlab("x cursor position (m)")+
    theme_bw()+ 
    theme(strip.background.y = element_blank(),
          strip.text.y = element_blank(),
          axis.title.y = element_blank())
  lh_late_plot

  rh_late_plot = ggplot(data = filter(fulldata_groupMeans, hand == "Right Hand"))+
    facet_grid(group~hand)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD7" & hand == "Right Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_ellipse(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD7" & hand == "Right Hand"), aes(x0 = xCursor_BC_mean_mean, y0 = yCursor_BC_atGlobal_mean_mean, a = xCursor_BC_mean_sd, b = yCursor_BC_atGlobal_mean_sd, angle = 0), fill = "gray", color = NA)+
    geom_path(data = filter(fulldata_groupMeans, movDir == "up" & block == "AD7" & hand == "Right Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_path(data = filter(fulldata_groupMeans, movDir == "down" & block == "AD7" & hand == "Right Hand"), aes(x = xCursor_BC_mean_mean, y = yCursor_BC_atGlobal_mean_mean))+
    geom_point(aes(x = 0.085, y = -0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = 0.085, y = 0.10), color = "red", shape = 1, size = 2)+
    geom_point(aes(x = 0.085, y = 0), color = "orange", shape = 1, size = 2)+
    scale_x_continuous(limits = c(-0.065, 0.235))+
    scale_y_continuous(limits = c(-0.15, 0.15))+
    coord_fixed()+
    xlab("x cursor position (m)")+
    theme_bw()+
    theme(axis.title.y = element_blank())
  rh_late_plot
  
  handpaths_plot = (lh_early_plot | rh_early_plot | lh_late_plot | rh_late_plot)
  handpaths_plot
  
} else {
  
  
  
}


