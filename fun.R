#####################
##### Libraries #####
#####################
library(readr)
library(lme4)
library(nlme)
library(foreign)
library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)
library(lattice)
library(splines2)
library(splines)
library(nnls)
library(foreach)
library(doParallel)
library(factoextra)
library(fpc)
library(cluster)
#######################
####Config Variables###
#######################
set.seed(127)
showPlots = FALSE
tenMin = TRUE
df = 6
cvNumbers<- 5 
outlierClean = TRUE
clean_bins_number = 100
number_of_turbines = 63
turbine_list = seq(1,number_of_turbines,1)
######################################
############ FUNCTIONS ###############
######################################
scale_func <- function(data_list){
  (data_list-min(data_list))/(max(data_list)-min(data_list))
}
RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}
cleanData <- function(data , nbins){
  mydata_temp <- data
  speedRange <- range(mydata_temp$Speed)
  bins <- seq(from = speedRange[1], to = speedRange[2], by = ((speedRange[2] - speedRange[1])/(nbins- 1)) )
  binmeans <- seq(from = (bins[1]+bins[2])/2, to = (bins[nbins]+bins[nbins-1])/2, by = ((speedRange[2] - speedRange[1])/(nbins- 1)) ) 
  meanBin <- c()
  sdBin <- c()
  
  for ( i in 1:(nbins-1)){
    rangedata <- mydata_temp[which(mydata_temp$Speed <= bins[i+1]),]
    rangedata <- rangedata[rangedata$Speed >= bins[i],]
    meanVal <- mean(rangedata$Power)
    meanBin <- c(meanBin , meanVal)
    sdVal <- sd(rangedata$Power)
    sdBin <- c(sdBin , sdVal)
  }
  se.bands=cbind(meanBin +3* sdBin ,meanBin -3* sdBin)
  
  removedData <- c()
  newdata <- c()
  for ( i in 1:(nbins-1)){
    
    rangedata <- mydata_temp[mydata_temp$Speed <= bins[i+1],]
    rangedata <- rangedata[rangedata$Speed >= bins[i],]
    n<- 1#dim(rangedata)[1]
    
    tempdata <- rangedata[(rangedata$Power - ( meanBin[i]- (3*sdBin[i]/(n^0.5)) ) ) >=0,]
    tempdata2 <- tempdata[(tempdata$Power - ( meanBin[i]+ (3*sdBin[i]/(n^0.5)) ) ) <=0,]
    
    newdata <- rbind(newdata , tempdata2)
    
    
    tempdata3 <- rangedata[(rangedata$Power - ( meanBin[i]- (3*sdBin[i]/(n^0.5)) ) ) < 0,]
    tempdata4 <- rangedata[(rangedata$Power - ( meanBin[i]+ (3*sdBin[i]/(n^0.5)) ) ) >0,]
    
    removedData <- rbind(removedData , tempdata4)
    removedData <- rbind(removedData , tempdata3)
    
  }
  cleanData <- newdata
}
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Function that returns Mean Absolute Error
nmpae <- function(error , maxFitted)
{
  mean(abs(error)/maxFitted)*100
}

naToMean <- function (y){
  for(i in 1:length(y)){
    if(is.na(y[i])){
      if(i == 1){
        y[1] <- 0
      }else{
        y[i]<-y[i-1]
      }
    }
  }
  return(y)
}

match.numeric <- function(x, table) {
  are.equal <- function(x, y) isTRUE(all.equal(x, y))
  match.one <- function(x, table)
    match(TRUE, vapply(table, are.equal, logical(1L), x = x))
  vapply(x, match.one, integer(1L), table)
}
####################################
####################################

