#test.R#
#source("train.R")
library(rhdf5)
library(topicmodels)
library(glmnet)

#model from train.R#
fit.glm<-readRDS("fit_glm.RData")
beta_lda<-readRDS("beta_lda.RData")
load("C:/Users/Owner/Desktop/ADS/Project4/Project4_data/lyr.RData")

#data processing#
nameh5_test<-dir("C:/Users/Owner/Desktop/ADS/Project4/TestSongFile100/TestSongFile100",recursive=T,full.names=T,pattern="\\.h5$")
datah5_test<-vector("list",length=length(nameh5_test))
for(i in 1:length(nameh5_test))
{datah5_test[[i]]<-h5read(file=nameh5_test[i],"/analysis")}
names_factor_test<-names(datah5_test[[1]])
names_factor_test<-names_factor_test[c(8,9,10,11,12,13)]
for(i in 1:length(nameh5_test))
{datah5_test[[i]]<-datah5_test[[i]][names_factor_test]} 
data28_test<-vector("list",length=length(nameh5_test))
for(i in 1:length(nameh5_test))
{ data28_test[[i]]<-rbind(datah5_test[[i]]$segments_loudness_max,datah5_test[[i]]$segments_loudness_max_time,
                     datah5_test[[i]]$segments_loudness_start,datah5_test[[i]]$segments_pitches,
                     datah5_test[[i]]$segments_start,datah5_test[[i]]$segments_timbre)
}
data28_obs_test<-vector("list",length=length(nameh5_test))
for(i in 1:length(nameh5_test))
{ for(j in 1:28)
{data28_obs_test[[i]]<-rbind(data28_obs_test[[i]],rep(data28_test[[i]][j,],length.out=1375))}}

#PCA#
data28_pca_test<-matrix(0,nrow=length(nameh5_test),ncol=1375)
for(i in 1:length(nameh5_test))
{ data28_pca_test[i,]<-prcomp(data28_obs_test[[i]],scale=F)$rotation[,1]}

#prediction matrix#
yhat_glm<-predict(fit.glm,as.matrix(data28_pca_test),type="response")
yhat_glm_mat<-yhat_glm[1:length(nameh5_test),1:20,1]

#probability rank#
result_glm<-yhat_glm_mat%*%beta_lda
rank_glm<-matrix(0,nrow=length(nameh5_test),ncol=5000)
for(i in 1:length(nameh5_test))
{ rank_glm[i,]<-5000+1-rank(c(result_glm[i,1],0,0,result_glm[i,2:3],rep(0,25),result_glm[i,4:4973]))}
colnames(rank_glm)<-colnames(lyr[,-1])
write.csv(rank_glm,"test_rank.csv")
