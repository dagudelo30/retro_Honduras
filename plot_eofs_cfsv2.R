
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#### functions ######


lead <- function(x){ 
  
  month=which(month.abb==substr(as.character(x),5,7))
  l=which(month.abb==substr(x,1,3))
  if((month-4)<=0){pos =(month-4)+12}else pos =(month-4)
  if(pos==l){out <- "LT-3"}else if(month-1==l){
    out <- "LT-0"}else out <- "LT-5"
  return(out)
  
}

table_cor <- function(path){
  

        path_m <- paste0(path,"/output/opt_domain/metrics.csv")
        data <- fread(path_m,sep = ",",dec=".",select=c(2)) %>% unique(.)
        data_e <-  lapply(data[,1],function(x)str_split(x,"_")) %>% `[[`(1) %>% do.call("rbind.data.frame",.)
        path_opt <-paste0(path,"/output/raw_output/",data$file,"_cca_scores_x.txt")  
        path_all <- paste0(path,"/output/raw_output/",paste(data_e[,1],data_e[,2],sep="_"),"_0","_cca_scores_x.txt") 
        
        data_opt=lapply(path_opt, function(x)fread(x,header=T,select = c(2)))   
        data_all=lapply(path_all, function(x)fread(x,header=T,select = c(2)))
        cor=Map(function(x,y)round(cor(x,y),2),data_opt,data_all ) %>% unlist(.)
        
        l=lapply(basename(path_all),lead)%>%unlist(.)
        
        data_out=cbind.data.frame(ic=as.character(data_e[,1]),lead=l,trim=as.character(data_e[,2]),year=basename(path),cor=cor)

        return(data_out)
        
        }

#### run ##########


main_dir <- "D:/Dagudelo/Desktop/descarga/cpt_r"

all_path <- paste0(main_dir,"/",2005:2015)


all_cor <- lapply(all_path,table_cor) %>% do.call("rbind.data.frame",.)

all_cor$cor[all_cor$cor==1]=NA

myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd"))


cor.heatmap <- ggplot(data = all_cor, mapping = aes(x = lead,y = year,fill = abs(cor))) +
  geom_tile() +
  xlab(label = "Season") +
  ylab(label = "Year") +
  facet_grid(~ trim, switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradientn(name = "Pearson correlation",colours = myPalette(100), limits=c(0, 1)) +
  theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5), strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  theme_bw() # Use the black and white theme

cor.heatmap