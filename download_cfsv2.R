#Script to download the cfsv2 from IRI data library for a specific season 
#and specific area in CPT format

#Created by:  Diego Agudelo
#Date: February 2019

options(timeout=180)

if(require(stringr)==FALSE){install.packages("stringr",dependencies = TRUE)}
library("stringr")
if(require(R.utils)==FALSE){install.packages("R.utils",dependencies = TRUE)}
library(R.utils)
suppressMessages(if(require(parallel)==FALSE){install.packages("parallel")});library("parallel")

####### function ######

download_CFSV2_CPT_1=function(firs_year,last_year,i_month,ic,dir_save,area1, lg){
  lg_s <- lg -1
  lead <- i_month-ic
  if(lead<0){lead <- lead + 12 ; last_year=last_year-1}
  
  route <- paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[ic],"%20",firs_year,"-",last_year,"%29VALUES/L/",lead,".5/",lead+lg_s,".5/RANGE%5BL%5D//keepgrids/average/M/1/24/RANGE%5BM%5Daverage/Y/%28",area1[4],"%29%28",area1[3],"%29RANGEEDGES/X/%28",area1[1],"%29%28",area1[2],"%29RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BS/L/add%5Dcptv10.tsv.gz")
  
  trimestrel <- (ic+lead):(ic+lead+lg_s)
  if(sum(trimestrel>12)>0)trimestrel[which(trimestrel>12)]=trimestrel[which(trimestrel>12)]-12
  path_save <- paste0(dir_save,"/",month.abb[ic],"_",paste(month.abb[trimestrel],collapse = "-"),".tsv.gz")
  download.file(route,path_save)
  gunzip(path_save)
  
  return(paste("Successful download",path_save))
}

####### run ##########

main_dir <- "D:/OneDrive - CGIAR/Desktop/descarga_cfvs2/" # Modifique esta línea de acuerdo a su directorio de trabajo

lapply(paste0(main_dir,  2005:2015,"/input/sst_cfsv2"),function(x)dir.create(x,recursive = T))
lapply(paste0(main_dir,  2005:2015,"/input/stations"),function(x)dir.create(x,recursive = T))

area1 <- list(c(130,340,-15,35)) #xmin, xmax, ymin, ymax
i_month <- c(5,6,7,8,11,  5,6,7,8,11,  5, 6,7,8,11  )# Modifique esta línea con el Mes de inicio del trimestre de interés
lg <-      c(3,3,2,3,3 ,  3,3,2,3,3 ,  3, 3,2,3,3   )  # Modifique esta línea con el Tamaño de la temporada a descargar. Por ej. AMJ lg=3/ AM lg=2
ic <-      c(4,5,6,7,10 , 1,2,3,4,7 , 11,12,1,2,5   ) # Modifique esta línea con la Condición inicial de la corrida del pronóstico o también conocido como lead time
firs_year <- 1981 # Modifique esta línea con el Año de inicio de la descarga

numCores <- detectCores()
numCores
cl <- makeCluster(numCores-1)
clusterExport(cl,list("area1","i_month","lg","ic","firs_year","download_CFSV2_CPT_1"),envir=environment()) 
clusterEvalQ(cl, library("R.utils"))

for(i in 2005:2015){
  
  last_year <- i-1 #Last year
  dir_save <- paste0(main_dir,i,"/input/sst_cfsv2")
  clusterExport(cl,list("last_year","dir_save"),envir=environment())
  
  clusterMap(cl,download_CFSV2_CPT_1,firs_year,last_year,i_month,ic,dir_save,area1,lg)
  
}

stopCluster(cl)




