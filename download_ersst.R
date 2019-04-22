#Script to download the ERSST from IRI data library for a specific season 
#and specific area in CPT format

#Created by: Diego Agudelo
#Date: February 2019

options(timeout=180)

if(require(stringr)==FALSE){install.packages("stringr",dependencies = TRUE)}
library("stringr")
if(require(R.utils)==FALSE){install.packages("R.utils",dependencies = TRUE)}
library(R.utils)
suppressMessages(if(require(parallel)==FALSE){install.packages("parallel")});library("parallel")

####### function ######

download_ERSST_CPT=function(firs_year,last_year,i_month,l_season,dir_save,m_for,l_for,area1){
  
  trimestrel <- i_month:(i_month+l_season-1)
  fores <- m_for:(m_for+l_for-1)
  if(sum(fores>12)>0)fores[which(fores>12)]=fores[which(fores>12)]-12
  if(sum(trimestrel>12)>0)trimestrel[which(trimestrel>12)]=trimestrel[which(trimestrel>12)]-12
  route <- paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.ERSST/.version4/.sst/T/%28", month.abb[trimestrel[1]] ,"%20", firs_year ,"%29%28",  month.abb[trimestrel[l_season]] ,"%20", last_year ,"%29RANGEEDGES/T/", l_season ,"/boxAverage/T/12/STEP/Y/%28",area1[4],"%29%28",area1[3],"%29RANGEEDGES/X/%28",area1[1],"%29%28",area1[2],"%29RANGEEDGES/-999/setmissing_value/Y/high/low/RANGE/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz")
  path_save <- paste0(dir_save,"/",paste(month.abb[fores],collapse = "-"),"_",paste(month.abb[trimestrel],collapse = "-"),".tsv.gz")
  download.file(route,path_save)
  gunzip(path_save)
  
  return("Successful download")
  
}

####### run ##########

main_dir <- "D:/OneDrive - CGIAR/Desktop/descarga/" # Modifique esta línea de acuerdo a su directorio de trabajo

lapply(paste0(main_dir,  2005:2015,"/input/sst_ersst"),function(x)dir.create(x,recursive = T))
lapply(paste0(main_dir,  2005:2015,"/input/stations"),function(x)dir.create(x,recursive = T))

area1 <- c(130,340,-15,35) #xmin, xmax, ymin, ymax
i_month <-  c(5,6,7,8,11,  4,5,6,7,10,  1,2,3,4,7) #First month to download
l_season <- c(3,3,2,3,3 ,  1,1,1,1,1 ,  1,1,1,1,1) #Length season (1, 2, 3. meses)
m_for <-    c(5,6,7,8,11,  5,6,7,8,11,  5,6,7,8,11) #Month to forecast
l_for <-    c(3,3,2,3,3 ,  3,3,2,3,3 ,  3,3,2,3,3) #Length forescast 
firs_year <- 1981 #Initial year


numCores <- detectCores()
numCores
cl <- makeCluster(numCores-1)
clusterExport(cl,list("area1","i_month","l_season","m_for","l_for","firs_year","download_ERSST_CPT"),envir=environment()) 
clusterEvalQ(cl, library("R.utils"))

for(i in 2005:2015){
  
  last_year <- i #Last year
  dir_save <- paste0(main_dir,i,"/input/sst_ersst")
  clusterExport(cl,list("last_year","dir_save"),envir=environment())
  
  clusterMap(cl,download_ERSST_CPT,firs_year,last_year,i_month,l_season,dir_save,m_for,l_for,list(area1))
  
}

stopCluster(cl)


