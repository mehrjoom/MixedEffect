rm(list=ls())
#setwd("C:/Users/Mehrdad/Documents/Personal/thesis/20200608/mixed effect git/mixed-effect-power-curve-modeling")
source("C:/Users/Mehrj/OneDrive - FarmLink Marketing Solutions/Personal/Thesis/My Papers/Paper 3/fun.R")
library(lme4)
library(arm)
library(optimx)
all_data = c()
set.seed(127)
tur = 1
number_of_turbines = 63
df = 5
for (tur in seq(1,number_of_turbines)){
  mydata <- read_csv(paste("C:/Users/Mehrj/OneDrive - FarmLink Marketing Solutions/Personal/Thesis/My Papers/Dataset/turbine data/Turbine/T",tur,".csv" ))
  mydata <- mydata[mydata$Power >= 0, ]
  mydata <- mydata[mydata$Power < 2000, ]
  mydata <- mydata[mydata$Speed > 5, ]
  mydata <- mydata[mydata$Speed <= 13, ]
  
  mydata <-mydata[complete.cases(mydata$Power), ]
  mydata <-mydata[complete.cases(mydata$Speed), ]
  mydata <-mydata[complete.cases(mydata$Direction), ]
  mydata<-cleanData(mydata,10)
  mydata <-mydata[complete.cases(mydata$Power), ]
  mydata <-mydata[complete.cases(mydata$Speed), ]
  mydata <-mydata[complete.cases(mydata$Direction), ]
  mydata = mydata[order(mydata$Time, decreasing = FALSE),]
  current_time = mydata$Time
  #current_hour = strftime(current_time, format="%H")
  curren_month = as.integer(strftime(current_time, format="%m"))
  mydata[,6] = curren_month
  mydata$X1 = tur
  all_data = rbind(all_data, mydata)
}  
colnames(all_data)[1] = "Turbine"
colnames(all_data)[6] = "Month"
plot(all_data[which(all_data$Turbine==1),]$Speed,all_data[which(all_data$Turbine==1),]$Power)  
save(all_data ,file = "C:/Users/Mehrj/OneDrive - FarmLink Marketing Solutions/Personal/Thesis/My papers/Paper 3/saved data/63TurbineCleaned40.RData")

first_month_ind = which(all_data$Month == 6)
Cluster_data = all_data[first_month_ind,]
feature_power_means = c()
tur = 1
for (tur in 1:number_of_turbines){
  temp_list33 = c()
  tur1_data = Cluster_data[Cluster_data$Turbine == tur,]
  knot_range = c(5,6,7,8,9,10,11,12,13)
  knt = 1
  temp_list33 = c()
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  knt = knt+1
  temp_list33 = c(temp_list33, mean(tur1_data[knot_range[knt]<tur1_data$Speed & tur1_data$Speed<knot_range[(knt+1)],]$Power))
  
  feature_power_means = rbind(feature_power_means, temp_list33)
  rownames(feature_power_means)[tur]=tur
}
feature_set = feature_power_means
gap_stat <- clusGap(feature_set,  kmeans, K.max = 22, B = 100 )#firstSEmax , Tibs2001SEmax
#fviz_gap_stat(gap_stat, maxSE=list(method="globalmax"))
#fviz_nbclust(feature_power_means, kmeans, method = "silhouette")
fviz_nbclust(feature_power_means, kmeans, method = "wss")+ 
  theme(axis.text.x = element_text(size = 19),axis.text.y = element_text(size = 19),axis.title.x = element_text(size = 19),axis.title.y = element_text(size = 19) )+ 
  geom_line(aes(group = 1),size = 2) + 
  geom_point( size = 5)

all_data = cbind(all_data,scale_func(all_data$Power))

save(all_data ,feature_set,Cluster_data,feature_power_means,file = "C:/Users/Mehrj/OneDrive - FarmLink Marketing Solutions/Personal/Thesis/My papers/Paper 3/saved data/63Turbine10_scaled.RData")
load(file = "C:/Users/Mehrj/OneDrive - FarmLink Marketing Solutions/Personal/Thesis/My papers/Paper 3/saved data/63Turbine10_scaled.RData")

clust_number = 4
K_cluster = kmeans(feature_set,clust_number, nstart=5)
table(K_cluster$cluster)

###########################################
###### clustring feature figure ###########
###########################################
color_list_names = c("red","blue","green")
op <- par(family = "Times", cex.axis=0.9, cex.lab=1.2 , mai=c(1,1,0.5,0.5))
plot(Cluster_data[Cluster_data$Turbine == 1,]$Speed, Cluster_data[Cluster_data$Turbine == 1,]$Power
     , xlab="Wind Speed (m/s)", ylab=" Power  (kw)", pch = 20,col = "red" , ylim = c(0,1650),
     family="Times",
     cex.lab=1.75, 
     cex.axis=1.5)
#points(Cluster_data[Cluster_data$Turbine == 7,]$Speed, Cluster_data[Cluster_data$Turbine == 7,]$Power, pch = 20,col = "blue")
points(Cluster_data[Cluster_data$Turbine == 22,]$Speed, Cluster_data[Cluster_data$Turbine == 22,]$Power, pch = 20,col = "blue")
points(Cluster_data[Cluster_data$Turbine == 8,]$Speed, Cluster_data[Cluster_data$Turbine == 8,]$Power, pch = 20,col = "green")

abline(v=c(6,7,8,9,10,11,12), lwd=0.6, lty=2)

legend(x=5.5,y=1600,c("T1","T2", "T3"),cex=1.5, merge = FALSE, col=c("red","green","blue"), pch=c(20,20,20))#,pch=c(5,22,17)

x_mean = seq(5.5 , 12.5)
plot(x_mean,feature_power_means[ 1,] , xlab="Wind Speed (m/s)", ylab="Mean Power  (kw)",  ylim = c(0,1650),
     family="Times",
     cex.lab=1.75, 
     cex.axis=1.5,pch = 20,col = "red")
#points(Cluster_data[Cluster_data$Turbine == 7,]$Speed, Cluster_data[Cluster_data$Turbine == 7,]$Power, pch = 20,col = "blue")
points(x_mean,feature_power_means[ 22,] , pch = 20,col = "blue")
points(x_mean,feature_power_means[ 8,] , pch = 20,col = "green")
abline(v=c(6,7,8,9,10,11,12), lwd=0.8, lty=2)
legend(x=5.5,y=1600,c("T1","T2", "T3"),cex=1.3, merge = FALSE, col=c("red","green","blue"), pch=c(20,20,20))#,pch=c(5,22,17)

###########################################
###########################################
###########################################
#define cluster of each data

all_data = cbind(all_data, 0)
colnames(all_data)[8] = "Cluster"
#all_data = as.data.frame(all_data)
for (tu in seq(1,number_of_turbines)) {
  all_data[all_data$Turbine == tu,]$Cluster = K_cluster$cluster[tu]
}

###########################################
###########################################
###########################################
train_single_aggregated_mae = c()
train_single_aggregated_rmse = c()

test_single_aggregated_mae = c()
test_single_aggregated_rmse = c()

train_random_intercept_turbine_aggregated_mae = c()
train_random_intercept_turbine_aggregated_rmse = c()
test_random_intercept_turbine_aggregated_mae = c()
test_random_intercept_turbine_aggregated_rmse = c()

train_random_intercept_aggregated_mae = c()
train_random_intercept_aggregated_rmse = c()
test_random_intercept_aggregated_mae = c()
test_random_intercept_aggregated_rmse = c()

train_random_slope_aggregated_mae = c()
train_random_slope_aggregated_rmse = c()
test_random_slope_aggregated_mae = c()
test_random_slope_aggregated_rmse = c()


test_single_aggregated_mae_bins = c()
test_single_aggregated_rmse_bins = c()


test_random_intercept_aggregated_mae_bins = c()
test_random_intercept_aggregated_rmse_bins = c()

test_random_slope_aggregated_mae_bins = c()
test_random_slope_aggregated_rmse_bins = c()
###########################################
###########################################
iterat1=1
fig_show = FALSE
for (iterat1 in seq(1,10)) {#seq(1,9)
  print(paste("Iteration",iterat1,"started"))
  Train_set =c ()
  Test_set = c()
  mont = 1
  rest_data = all_data
  for (mont in c(7,8,9,10,11,12,1) ){
    ind_set = sample(which(rest_data$Month==mont), size = (0.75*length(which(rest_data$Month==mont))), replace = FALSE)
    Train_set = rbind(Train_set, rest_data[ind_set,])
    rest_data = rest_data[-ind_set,]
    ind_set = which(rest_data$Month==mont)
    Test_set = rbind(Test_set, rest_data[ind_set,])
    rest_data = rest_data[-ind_set,]
  }
  
  x.grid = seq(5.1,13,0.1)
  
  
  ###################################
  ######### evak time computation  ##
  ###################################
  t_11 = Sys.time()
  for (tu in seq(1,number_of_turbines)) {
    tur_data_temp = Train_set[Train_set$Turbine == tu,]
    X_111 = ns(tur_data_temp$speed, df=df)
    fit_ns_11 = lm(tur_data_temp$power ~ X_111 , data = tur_data_temp)
  }
  t_12 = Sys.time()
  time_diff1 = t_12 - t_11
  print(time_diff1)
  ########################################
  ###### Eval P_ik for each turbine ######
  ########################################
  tu = 1
  all_pk_list = c()
  for (tu in seq(1,number_of_turbines)) {
    tur_data_temp = Train_set[Train_set$Turbine == tu,]
    #if(fig_show){
     # plot(tur_data_temp$Speed, tur_data_temp$Power)
    #}
    pk_list = c()
    
    for (ind_grid in x.grid){
      
      if(length(which((abs(tur_data_temp$Speed-ind_grid))<0.001))>0){
        power_ind_grid = mean(tur_data_temp[which((abs(tur_data_temp$Speed-ind_grid))<0.001),]$Power)
        pk_list = rbind(pk_list,c(ind_grid, power_ind_grid))
      }
    }
    if(fig_show){
      plot(pk_list[,1],pk_list[,2])  
    }
    pk_list = cbind(pk_list,tu)
    pk_list = cbind(pk_list,K_cluster$cluster[tu])
    pk_list =as.data.frame(pk_list)
    colnames(pk_list)[1] = "speed"
    colnames(pk_list)[2] = "power"
    colnames(pk_list)[3] = "turbine"
    colnames(pk_list)[4] = "cluster"
    all_pk_list = rbind(all_pk_list,pk_list)
  }
  
  ########################################
  ###### Plot P_ik for each turbine ######
  ########################################
  if(fig_show){
    plot(all_pk_list[all_pk_list$turbine==1,]$speed,all_pk_list[all_pk_list$turbine==1,]$power, type = "l", col = all_pk_list[all_pk_list$turbine==1,]$cluster,
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5)
    for (tu in seq(2,number_of_turbines)) {
      lines(all_pk_list[all_pk_list$turbine==tu,]$speed,all_pk_list[all_pk_list$turbine==tu,]$power,  col = all_pk_list[all_pk_list$turbine==tu,]$cluster)
    }
  }
  ########################################
  ####### Single aggregated Model ########
  ########################################
  
  X = ns(all_pk_list$speed, df=df)
  
  x.grid.ns =  ns( x.grid,  knots= attr(X, "knots"))
  
  fit_ns = lm(all_pk_list$power ~ X , data = all_pk_list)
  train_single_aggregated_mae = c(train_single_aggregated_mae,mean(abs(residuals(fit_ns))))
  train_single_aggregated_rmse  = c(train_single_aggregated_rmse,sqrt(mean(residuals(fit_ns)^2)))
  sd(residuals(fit_ns))
  
  XTest = ns( Test_set$Speed,  knots= attr(X, "knots"))
  test_number = nrow(Test_set)
  beta_fit_ns <- coef(fit_ns)
  pred_ns= (cbind(rep(1,test_number),XTest) %*% beta_fit_ns)
  pred_ns[which(pred_ns<0)] = 0
  
  err = pred_ns- Test_set$Power
  test_single_aggregated_mae = c(test_single_aggregated_mae ,mean(abs(err)))
  test_single_aggregated_rmse = c(test_single_aggregated_rmse ,sqrt(mean(err^2)))#sqrt(mean(err^2)))
  
  mae_each_bin = c()
  rmse_each_bin = c()
  for (each_bin in seq(6,13)){
    bins_ind = which(Test_set$Speed<each_bin & (each_bin-1)<Test_set$Speed)
    err = pred_ns[bins_ind,]- Test_set[bins_ind,]$Power
    mae_each_bin = c(mae_each_bin,mean(abs(err)))
    rmse_each_bin = c(rmse_each_bin,sqrt(mean(err^2)))
  }
  test_single_aggregated_mae_bins = rbind(test_single_aggregated_mae_bins, mae_each_bin)
  test_single_aggregated_rmse_bins = rbind(test_single_aggregated_rmse_bins, rmse_each_bin)
  
  ######################
  #par(mfrow=c(1,1))
  beta_fit_ns <- coef(fit_ns)
  pred_ns_xgrid= (cbind(rep(1,dim(x.grid.ns)[1]),x.grid.ns) %*% beta_fit_ns)
  pred_ns_xgrid[which(pred_ns_xgrid<0)] = 0
  
  ######################
  ###############################################
  ######## Plot Single Aggregated Power Curve ###
  ###############################################
  if(fig_show){
    plot(all_pk_list$speed,all_pk_list$power, pch=20, cex=0.5, col = "grey", xlab="Wind Speed (m/s)", ylab="Power  (kw)",  ylim = c(0,1650),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5)
    #plot(Train_set$Speed,Train_set$Power, pch=20, cex=0.5, col = "grey")
    
    lines(x.grid,pred_ns_xgrid, type = "l",cex=0.9, xlab="Wind Speed (m/s)", ylab=" Power  (kw)", lwd=2,
          family="Times",
          cex.lab=1.65, 
          cex.axis=1.0)
    
    plot(all_pk_list[which(all_pk_list$speed<13 & 10<all_pk_list$speed),]$speed,all_pk_list[which(all_pk_list$speed<13 & 10<all_pk_list$speed),]$power, pch=20,  col = "grey", xlab="Wind Speed (m/s)", ylab="Power  (kw)",    ylim = c(1100,1700),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5)#cex=0.5,
    #plot(Train_set$Speed,Train_set$Power, pch=20, cex=0.5, col = "grey")
    
    lines(x.grid,pred_ns_xgrid, type = "l",cex=0.9, xlab="Wind Speed (m/s)", ylab=" Power  (kw)", lwd=2,
          family="Times",
          cex.lab=1.65, 
          cex.axis=1.0)
  }
  
  
  
  #############################################
  ###########  Random Intercept ###############
  #############################################
  X = ns(all_pk_list$speed, df=df)
  fit_intercept <- lmer(power ~  X + (1|cluster), data= all_pk_list, REML = FALSE)
  mean(abs(residuals(fit_intercept)))
  
  train_random_intercept_aggregated_mae= c(train_random_intercept_aggregated_mae,mean(abs(residuals(fit_intercept))))
  train_random_intercept_aggregated_rmse = c(train_random_intercept_aggregated_rmse,sqrt(mean(residuals(fit_intercept)^2)))
  sd(residuals(fit_intercept))
  
  beta_fit_cluster1 <- coef(fit_intercept)
  
  ######################
  #par(mfrow=c(1,1))
  
  beta_fit_intercept <- coef(fit_intercept)
  pred_all_tur_x_grid = c()
  tempall_x_grid = cbind(1,x.grid.ns)
  temp2 = t(beta_fit_intercept$cluster[1,])
  predict_k_grid_1 = tempall_x_grid%*%temp2
  temp2 = t(beta_fit_intercept$cluster[2,])
  predict_k_grid_2 = tempall_x_grid%*%temp2
  temp2 = t(beta_fit_intercept$cluster[3,])
  predict_k_grid_3 = tempall_x_grid%*%temp2
  
  if (clust_number==4){
    temp2 = t(beta_fit_intercept$cluster[4,])
    predict_k_grid_4 = tempall_x_grid%*%temp2
    
  }
  #####################################################################
  ######## Plot Multiple Aggregated (Random Effect) Power Curve ###
  #####################################################################
  if(fig_show){
    plot(all_pk_list[all_pk_list$turbine==1,]$speed,all_pk_list[all_pk_list$turbine==1,]$power, pch=20, cex=0.5, col = color_list_names[ all_pk_list[all_pk_list$turbine==1,]$cluster], xlab="Wind Speed (m/s)", ylab=" Power  (kw)",  ylim = c(0,1650),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5)
    for (tu in seq(2,number_of_turbines)) {
      points(all_pk_list[all_pk_list$turbine==tu,]$speed,all_pk_list[all_pk_list$turbine==tu,]$power,  pch=20, cex=0.5, col = color_list_names[ all_pk_list[all_pk_list$turbine==tu,]$cluster])
    }
    lines(x.grid,predict_k_grid_1, type = "l",lwd=2,  col = color_list_names[1])
    lines(x.grid,predict_k_grid_2, type = "l",lwd=2,  col = color_list_names[2])
    lines(x.grid,predict_k_grid_3, type = "l",lwd=2,  col = color_list_names[3])
    legend(x=5.2,y=1600,c("Cluster-1","Cluster-2", "Cluster-3"),lwd=c(2,2,2),
           cex=1.25, merge = FALSE,col=color_list_names )#,pch=c(5,22,17)
    
    
    plot(all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==1,]$speed,all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==1,]$power, pch=20,  col = color_list_names[ all_pk_list[all_pk_list$turbine==1,]$cluster], xlab="Wind Speed (m/s)", ylab=" Power  (kw)",  ylim = c(1100,1700),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5) #cex=0.5,
    for (tu in seq(2,number_of_turbines)) {
      points(all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==tu,]$speed,all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed &  all_pk_list$turbine==tu,]$power,  pch=20,  col = color_list_names[ all_pk_list[all_pk_list$turbine==tu,]$cluster])
    }
    lines(x.grid,predict_k_grid_1, type = "l",lwd=2,  col = color_list_names[1])
    lines(x.grid,predict_k_grid_2, type = "l",lwd=2,  col = color_list_names[2])
    lines(x.grid,predict_k_grid_3, type = "l",lwd=2,  col = color_list_names[3])
    legend(x=12,y=1300,c("Cluster-1","Cluster-2", "Cluster-3"),lwd=c(2,2,2),
           cex=1.25, merge = FALSE,col=color_list_names )#,pch=c(5,22,17)
  }
  ######################
  ######################
  error_groups_intercept = c()
  
  mae_each_bin_cluster = c()
  rmse_each_bin_cluster = c()
  
  for (cl_ind  in seq(1,clust_number)) {
    list_turbines_in_cluster = which(K_cluster$cluster==cl_ind)
    test_data_in_cluster = Test_set[which(Test_set$Turbine%in%list_turbines_in_cluster),]
    test_number_cluster = nrow(test_data_in_cluster)
    XTest_cluster = ns(test_data_in_cluster$Speed,  knots= attr(X, "knots"))
    tempall = cbind(rep(1,test_number_cluster),XTest_cluster)
    temp2 = t(beta_fit_intercept$cluster[cl_ind,])
    predict_k = tempall%*%temp2
    error_groups_intercept = c(error_groups_intercept,abs(predict_k-test_data_in_cluster$Power))
    
    mae_each_bin = c()
    rmse_each_bin = c()
    
    for (each_bin in seq(6,13)){
      bins_ind = which(test_data_in_cluster$Speed<each_bin & (each_bin-1)<test_data_in_cluster$Speed)
      err = predict_k[bins_ind,]- test_data_in_cluster[bins_ind,]$Power
      mae_each_bin = c(mae_each_bin,mean(abs(err)))
      rmse_each_bin = c(rmse_each_bin,sqrt(mean(err^2)))
    }
    mae_each_bin_cluster= rbind(mae_each_bin_cluster, mae_each_bin)
    rmse_each_bin_cluster = rbind(rmse_each_bin_cluster,rmse_each_bin)
  }
  
  
  
  test_random_intercept_aggregated_mae_bins = rbind(test_random_intercept_aggregated_mae_bins, colMeans(mae_each_bin_cluster))
  test_random_intercept_aggregated_rmse_bins = rbind(test_random_intercept_aggregated_rmse_bins, colMeans(rmse_each_bin_cluster))
  
  test_random_intercept_aggregated_mae = c(test_random_intercept_aggregated_mae ,mean(abs(error_groups_intercept)))
  test_random_intercept_aggregated_rmse = c(test_random_intercept_aggregated_rmse ,sqrt(mean(error_groups_intercept^2)))#sqrt(mean(err_all_tur^2)))
  
  
  #############################################
  ###########  Random Slope ###############
  #############################################
  
  X = ns(all_pk_list$speed, df=df)
  fit_slope <- lmer(power ~  X + (X|cluster), data= all_pk_list, REML = TRUE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))#
  fit_slope
  train_random_slope_aggregated_mae= c(train_random_slope_aggregated_mae,mean(abs(residuals(fit_slope))))
  train_random_slope_aggregated_rmse = c(train_random_slope_aggregated_rmse,sqrt(mean(residuals(fit_slope)^2)))
  sd(residuals(fit_slope))
  
  beta_fit_slop <- coef(fit_slope)
  pred_all_tur_x_grid_slope = c()
  tempall_x_grid = cbind(1,x.grid.ns)
  temp2 = t(beta_fit_slop$cluster[1,])
  predict_k_grid_1_slope = tempall_x_grid%*%temp2
  temp2 = t(beta_fit_slop$cluster[2,])
  predict_k_grid_2_slope = tempall_x_grid%*%temp2
  temp2 = t(beta_fit_slop$cluster[3,])
  predict_k_grid_3_slope = tempall_x_grid%*%temp2
  
  
  #####################################################################
  ######## Plot Multiple Aggregated (Random Effect) Power Curve ###
  #####################################################################
  if(fig_show){
    plot(all_pk_list[all_pk_list$turbine==1,]$speed,all_pk_list[all_pk_list$turbine==1,]$power, pch=20, cex=0.5, col = color_list_names[ all_pk_list[all_pk_list$turbine==1,]$cluster], xlab="Wind Speed (m/s)", ylab=" Power  (kw)",  ylim = c(0,1650),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5)
    for (tu in seq(2,number_of_turbines)) {
      points(all_pk_list[all_pk_list$turbine==tu,]$speed,all_pk_list[all_pk_list$turbine==tu,]$power,  pch=20, cex=0.5, col = color_list_names[all_pk_list[all_pk_list$turbine==tu,]$cluster])
    }
    lines(x.grid,predict_k_grid_1_slope, type = "l",lwd=2,  col = color_list_names[1])
    lines(x.grid,predict_k_grid_2_slope, type = "l",lwd=2,  col = color_list_names[2])
    lines(x.grid,predict_k_grid_3_slope, type = "l",lwd=2,  col = color_list_names[3])
    legend(x=5.2,y=1600,c("Cluster-1","Cluster-2", "Cluster-3"),lwd=c(2,2,2),
           cex=1.25, merge = FALSE,col=color_list_names )#,pch=c(5,22,17)
    
    
    plot(all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed &all_pk_list$turbine==1,]$speed,all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==1,]$power, pch=20,  col = color_list_names[ all_pk_list[all_pk_list$turbine==1,]$cluster], xlab="Wind Speed (m/s)", ylab=" Power  (kw)",  ylim = c(1100,1700),
         family="Times",
         cex.lab=1.75, 
         cex.axis=1.5) #cex=0.5,
    for (tu in seq(2,number_of_turbines)) {
      points(all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==tu,]$speed,all_pk_list[all_pk_list$speed<13 & 10<all_pk_list$speed & all_pk_list$turbine==tu,]$power,  pch=20,  col = color_list_names[all_pk_list[all_pk_list$turbine==tu,]$cluster])
    }
    lines(x.grid,predict_k_grid_1_slope, type = "l",lwd=2,  col = color_list_names[1])
    lines(x.grid,predict_k_grid_2_slope, type = "l",lwd=2,  col = color_list_names[2])
    lines(x.grid,predict_k_grid_3_slope, type = "l",lwd=2,  col = color_list_names[3])
    legend(x=12,y=1300,c("Cluster-1","Cluster-2", "Cluster-3"),lwd=c(2,2,2),
           cex=1.25, merge = FALSE,col=color_list_names )#,pch=c(5,22,17)
  }
  ######################
  ######################
  error_groups = c()
  
  mae_each_bin_cluster = c()
  rmse_each_bin_cluster = c()
  
  for (cl_ind  in seq(1,clust_number)) {
    list_turbines_in_cluster = which(K_cluster$cluster==cl_ind)
    test_data_in_cluster = Test_set[which(Test_set$Turbine%in%list_turbines_in_cluster),]
    test_number_cluster = nrow(test_data_in_cluster)
    XTest_cluster = ns(test_data_in_cluster$Speed,  knots= attr(X, "knots"))
    tempall = cbind(rep(1,test_number_cluster),XTest_cluster)
    temp2 = t(beta_fit_slop$cluster[cl_ind,])
    predict_k = tempall%*%temp2
    error_groups = c(error_groups,abs(predict_k-test_data_in_cluster$Power))
    
    mae_each_bin = c()
    rmse_each_bin = c()
    
    for (each_bin in seq(6,13)){
      bins_ind = which(test_data_in_cluster$Speed<each_bin & (each_bin-1)<test_data_in_cluster$Speed)
      err = predict_k[bins_ind,]- test_data_in_cluster[bins_ind,]$Power
      mae_each_bin = c(mae_each_bin,mean(abs(err)))
      rmse_each_bin = c(rmse_each_bin,sqrt(mean(err^2)))
    }
    mae_each_bin_cluster= rbind(mae_each_bin_cluster, mae_each_bin)
    rmse_each_bin_cluster = rbind(rmse_each_bin_cluster,rmse_each_bin)
    
  }
  test_random_slope_aggregated_mae = c(test_random_slope_aggregated_mae ,mean(abs(error_groups)))
  test_random_slope_aggregated_rmse = c(test_random_slope_aggregated_rmse ,sqrt(mean(error_groups^2)))#sqrt(mean(err_all_tur^2)))
  
  test_random_slope_aggregated_mae_bins = rbind(test_random_slope_aggregated_mae_bins, colMeans(mae_each_bin_cluster))
  test_random_slope_aggregated_rmse_bins = rbind(test_random_slope_aggregated_rmse_bins, colMeans(rmse_each_bin_cluster))
  
}
print("MAE:")
print("Single Aggregated:")
print(mean(test_single_aggregated_mae))
print(sd(test_single_aggregated_mae))
print("Random Intercept:")
print(mean(test_random_intercept_aggregated_mae))
print(sd(test_random_intercept_aggregated_mae))
print("Random Slope:")
print(mean(test_random_slope_aggregated_mae))
print(sd(test_random_slope_aggregated_mae))

print("RMSE:")
print("Single Aggregated:")
print(mean(test_single_aggregated_rmse ))
print(sd(test_single_aggregated_rmse ))
print("Random Intercept:")
print(mean(test_random_intercept_aggregated_rmse))
print(sd(test_random_intercept_aggregated_rmse))
print("Random Slope:")
print(mean(test_random_slope_aggregated_rmse))
print(sd(test_random_slope_aggregated_rmse))
#anova(fit_slope,fit_intercept,fit_ns)




print("MAE:")
print("Single Aggregated:")
print(colMeans(test_single_aggregated_mae_bins))
print("Random Intercept (BINS):")
print(colMeans(test_random_intercept_aggregated_mae_bins))
print("Random Slope:")
print(colMeans(test_random_slope_aggregated_mae_bins))

print("RMSE:")
print("Single Aggregated:")
print(colMeans(test_single_aggregated_rmse_bins ))
print("Random Intercept:")
print(colMeans(test_random_intercept_aggregated_rmse_bins))
print("Random Slope:")
print(colMeans(test_random_slope_aggregated_rmse_bins))
#anova(fit_slope,fit_intercept,fit_ns)



print("MAE:")
print("Single Aggregated:")
print(mean(colMeans(test_single_aggregated_mae_bins)))
print(sd(rowMeans(test_single_aggregated_mae_bins)))
print("Random Intercept (BINS):")
print(mean(colMeans(test_random_intercept_aggregated_mae_bins)))
print(sd(rowMeans(test_random_intercept_aggregated_mae_bins)))
print("Random Slope:")
print(mean(colMeans(test_random_slope_aggregated_mae_bins)))
print(sd(rowMeans(test_random_slope_aggregated_mae_bins)))

print("RMSE:")
print("Single Aggregated:")
print(mean(colMeans(test_single_aggregated_rmse_bins )))
print(sd(rowMeans(test_single_aggregated_rmse_bins )))
print("Random Intercept:")
print(mean(colMeans(test_random_intercept_aggregated_rmse_bins)))
print(sd(rowMeans(test_random_intercept_aggregated_rmse_bins)))
print("Random Slope:")
print(mean(colMeans(test_random_slope_aggregated_rmse_bins)))
print(sd(rowMeans(test_random_slope_aggregated_rmse_bins)))












clusterinMethod = K_cluster$cluster
t1=which(clusterinMethod==1)
power_list = cbind((all_pk_list[which(all_pk_list$turbine%in%t1),]$power),1)
plot(K_cluster$centers[1,],type="l")
for (ind in 2:clust_number) {
  t1=which(clusterinMethod==ind)
  lines(K_cluster$centers[ind,])
  power_list = rbind(power_list,cbind(all_pk_list[which(all_pk_list$turbine%in%t1),]$power,ind))
}
boxplot(power_list[,1] ~ power_list[,2], xlab="Cluster", ylab=" Power  (kw)", main = "All bins together", family="Times",cex.lab=1.75, cex.axis=1.5,col = color_list_names)



clusterinMethod = K_cluster$cluster
ranges_speed = seq(5,12)
for (ii in ranges_speed){
  t1=which(clusterinMethod==1)
  power_list = cbind((all_pk_list[which(all_pk_list$turbine%in%t1&all_pk_list$speed>ii&all_pk_list$speed<(ii+1)),]$power),1)
  for (ind in 2:clust_number) {
    t1=which(clusterinMethod==ind)
    power_list = rbind(power_list,cbind((all_pk_list[which(all_pk_list$turbine%in%t1&all_pk_list$speed>ii&all_pk_list$speed<(ii+1)),]$power),ind))
  }
  boxplot(power_list[,1] ~ power_list[,2], xlab="Cluster", ylab=" Power  (kw)", main = paste("bin ", ii , " - "  ,(ii+1)), family="Times",cex.lab=1.75, cex.axis=1.5,col = color_list_names)
  
}
