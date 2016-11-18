#train.R#
library(rhdf5)
library(topicmodels)
library(glmnet)
H5close()

#data precessing#
load("C:/Users/Owner/Desktop/ADS/Project4/Project4_data/lyr.RData")
lyr<-lyr[-c(2,3,6:30)]
nameh5<-dir("C:/Users/Owner/Desktop/ADS/Project4/Project4_data/data",recursive=T,full.names=T,pattern="\\.h5$")
datah5<-vector("list",length=length(nameh5))
for(i in 1:length(nameh5))
{datah5[[i]]<-h5read(file=nameh5[i],"/analysis")}
names_factor<-names(datah5[[1]])
names_factor<-names_factor[c(8,9,10,11,12,13)]
for(i in 1:length(nameh5))
{datah5[[i]]<-datah5[[i]][names_factor]} 

#feature selection#
data28<-vector("list",length=2350)
for(i in 1:2350)
{ data28[[i]]<-rbind(datah5[[i]]$segments_loudness_max,datah5[[i]]$segments_loudness_max_time,
                     datah5[[i]]$segments_loudness_start,datah5[[i]]$segments_pitches,
                     datah5[[i]]$segments_start,datah5[[i]]$segments_timbre)
}
data28_obs<-vector("list",length=2350)
for(i in 1:2350)
{ for(j in 1:28)
{data28_obs[[i]]<-rbind(data28_obs[[i]],rep(data28[[i]][j,],length.out=1375))}}


#PCA#
data28_pca<-matrix(0,nrow=2350,ncol=1375)
for(i in 1:2350)
{ data28_pca[i,]<-prcomp(data28_obs[[i]],scale=F)$rotation[,1]}

#Topic Model#
topic_lda<-LDA(lyr[-1],20,method="Gibbs")
gamma_lda<-topic_lda@gamma
beta_lda<-topic_lda@beta
beta_lda<-exp(beta_lda)

#Ridge Regression#
#lambda tuning#
cv_glm<-cv.glmnet(as.matrix(data28_pca),gamma_lda,family="mgaussian",alpha=0)
#ridge regression#
fit.glm<-glmnet(as.matrix(data28_pca),gamma_lda,family="mgaussian",alpha=0,lambda=cv_glm$lambda.min)

#save data#
saveRDS(fit.glm,"fit_glm.RData")
saveRDS(beta_lda,"beta_lda.RData")
