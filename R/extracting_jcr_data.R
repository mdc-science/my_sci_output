#### Set working directory #####
# Alternatively click on Session -> Set working directory -> Choose working directory...
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This sets WD to source file location
getwd()

##### Loading packages #####
pacman::p_load(rcompanion, FSA, car, tibble, rstatix, pacman, plyr, 
               stringr, ggforce, ggpubr, scales, ggthemes, ggpmisc, 
               ggh4x, tidyverse, multcompView, RVAideMemoire, 
               ggrepel, here, ggsci, readxl)

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)
