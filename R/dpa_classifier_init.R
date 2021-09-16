dpa_classifier_init = function(){
  
  

# dependencies ------------------------------------------------------------
  
  library(dplyr)
  library(gtasql)
  library(pool)
  library(word2vec)
  library(stringr)
  library(stats)
  library(randomForest)
  
  

# check in correct wd -----------------------------------------------------

  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  bastiat.path = paste0(wd.pref, "/Bastiat")
  setwd(bastiat.path)
  
  
}