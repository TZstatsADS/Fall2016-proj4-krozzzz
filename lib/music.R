source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
library(MASS)
library(topicmodels)
library(randomForest)
library(nnet)
library(glmnet)
library(gbm)
library(h2o)
H5close()

#data get#
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

#Topic model for all songs#

#topic_lda<-LDA(lyr[-1],20,method="Gibbs")
#gamma_lda<-topic_lda@gamma
#beta_lda<-topic_lda@beta
#beta_lda<-exp(beta_lda)
#cluster<-vector(length=2350)
#for(i in 1:2350)
#{ cluster[i]<-which.max(gamma_lda[i,])}


#song_labels<-vector("list",length=2350)
#for(i in 1:2350)
#{ song_labels[[i]]<-matrix(0,nrow=20,ncol=10)}
#for(i in 1:2350)
#{ templyr<-as.matrix(lyr[-1])[i,]
#  temptopic<-terms(topic_lda,20)
#  song_labels[[i]]<-cbind(templyr[temptopic[,1]],templyr[temptopic[,2]],
#                          templyr[temptopic[,3]],templyr[temptopic[,4]],   
#                          templyr[temptopic[,5]],templyr[temptopic[,6]],
#                          templyr[temptopic[,7]],templyr[temptopic[,8]],
#                          templyr[temptopic[,9]],templyr[temptopic[,10]])
                         
#}
#song_value<-vector("list",length=2350)
#for(i in 1:2350)
#{ song_value[[i]]<-vector(length=10)}
#logit<-function(x)
#{ return(exp(x)/(1+exp(x)))}
#for(i in 1:2350)
#{for(j in 1:10)
#{ song_value[[i]][j]<-logit(song_labels[[i]][1,j])[[1]]+logit(song_labels[[i]][2,j])[[1]]
#                      +logit(song_labels[[i]][3,j])[[1]]+logit(song_labels[[i]][4,j])[[1]]
#                      +logit(song_labels[[i]][5,j])[[1]]+logit(song_labels[[i]][6,j])[[1]] 
#                      +logit(song_labels[[i]][7,j])[[1]]+logit(song_labels[[i]][8,j])[[1]]
#                      +logit(song_labels[[i]][9,j])[[1]]+logit(song_labels[[i]][10,j])[[1]]
#                      +logit(song_labels[[i]][11,j])[[1]]+logit(song_labels[[i]][12,j])[[1]]
#                      +logit(song_labels[[i]][13,j])[[1]]+logit(song_labels[[i]][14,j])[[1]]
#                      +logit(song_labels[[i]][15,j])[[1]]+logit(song_labels[[i]][16,j])[[1]] 
#                      +logit(song_labels[[i]][17,j])[[1]]+logit(song_labels[[i]][18,j])[[1]]
#                      +logit(song_labels[[i]][19,j])[[1]]+logit(song_labels[[i]][20,j])[[1]]
#}}
#cluster<-vector(length=2350)
#for(i in 1:2350)
#{ cluster[i]<-which.max(song_value[[i]])}

#l<-vector(length=2350)
#for(i in 1:2350)
#{ l[i]<-length(datah5[[i]][[1]])}
#quantile(l,prob=c(0.05,0.95))
#95% quantile is 1375


#28 rows per obs#
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

#Model Construction#
####
#PCA#
data28_pca<-matrix(0,nrow=2350,ncol=1375)
for(i in 1:2350)
{ data28_pca[i,]<-prcomp(data28_obs[[i]],scale=F)$rotation[,1]}


#train&test: 80% : 20%#


#train topic#
ind.train<-sample(2350,2350*0.8)
ind.test<-c(1:2350)[-ind.train]
topic_train<-LDA(lyr[ind.train,-1],20,method="Gibbs")
gamma_train<-topic_train@gamma
beta_train<-exp(topic_train@beta)
cluster_train<-vector(length=2350*0.8)
for(i in 1:2350*0.8)
{ cluster_train[i]<-which.max(gamma_train[i,])}
fit_train<-data.frame(data28_pca[ind.train,],as.factor(cluster_train))
fit_test<-data.frame(data28_pca[ind.test,])

#Test Rank#

#rank_test<-matrix(0,nrow=length(ind.test),ncol=4973)
#for(i in 1:length(ind.test))
#{ rank_test[i,]<-4973+1-rank(lyr[ind.test,-1][i,])}

#randomForest#
fit<-randomForest(as.factor.cluster_train.~.,data=fit_train,mtry=43,ntree=200,importance=TRUE)
yhat<-predict(fit,newdata=fit_test,type="prob")
result_rf<-yhat%*%beta_train
rank_rf<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_rf[i,]<-4973+1-rank(result_rf[i,])}
r_avg2<-vector(length=470)
for(i in 1:470)
{r_avg2[i]<-mean(rank_rf[i,])}
err_rf<-vector(length=470)
for(i in 1:470)
{ err_rf[i]<-mean(rank_rf[i,][lyr[ind.test,-1][i,]!=0])/r_avg2[i]}




#Multi logistic: Ridge Regression#
cv_glm<-cv.glmnet(as.matrix(fit_train[,-1376]),gamma_train,family="mgaussian",alpha=0)
fit.glm<-glmnet(as.matrix(fit_train[,-1376]),gamma_train,family="mgaussian",alpha=0,lambda=cv_glm$lambda.min)
yhat_glm<-predict(fit.glm,as.matrix(fit_test),type="response")
yhat_glm_mat<-yhat_glm[1:470,1:20,1]
result_glm<-yhat_glm_mat%*%beta_train
rank_glm<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_glm[i,]<-4973+1-rank(result_glm[i,])}
r_avg1<-vector(length=470)
for(i in 1:470)
{r_avg1[i]<-mean(rank_glm[i,])}
err_glm<-vector(length=470)
for(i in 1:470)
{ err_glm[i]<-mean(rank_glm[i,][lyr[ind.test,-1][i,]!=0])/r_avg1[i]}




#yhat_ml<-matrix(0,nrow=470,ncol=20)
#for(i in 1:470)
#{ for(j in 1:20)
#{yhat_ml[i,j]<-exp(yhat_glm[i,j,3])/(1+exp(yhat_glm[i,j,3]))}}
#for(i in 1:470)
#{yhat_ml[i,]<-yhat_ml[i,]/sum(yhat_ml[i,])}
#result_glm<-yhat_ml%*%beta_lda
#rank_glm<-matrix(0,nrow=470,ncol=4973)
#for(i in 1:470)
#{ rank_glm[i,]<-4973+1-rank(result_glm[i,])}
#r_avg1<-vector(length=470)
#for(i in 1:470)
#{r_avg1[i]<-mean(rank_glm[i,])}
#err_glm<-vector(length=470)
#for(i in 1:470)
#{ err_glm[i]<-mean(rank_glm[i,][lyr[ind.test,-1][i,]!=0])/r_avg1[i]}

#GBM#
fit_gbm<-gbm(as.factor.cluster_train.~.,data=fit_train,distribution="multinomial",n.trees=100,shrinkage=0.1,interaction.depth =2)
yhat_gbm<-predict(fit_gbm,fit_test,n.trees=100,type="response")
yhat_gbm_factor<-yhat_gbm[1:470,1:20,1]
result_gbm<-yhat_gbm_factor%*%beta_train
rank_gbm<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_gbm[i,]<-4973+1-rank(result_gbm[i,])}
r_avg<-vector(length=470)
for(i in 1:470)
{r_avg[i]<-mean(rank_gbm[i,])}
err_gbm<-vector(length=470)
for(i in 1:470)
{ err_gbm[i]<-mean(rank_gbm[i,][lyr[ind.test,-1][i,]!=0])/r_avg[i]}



  
#deep learning#
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
train.h2o<-as.h2o(data.frame(fit_train))
model <- 
  h2o.deeplearning(x = 1:1375,  # column numbers for predictors
                   y = 1376,   # column number for label
                   train.h2o, # data in H2O format
                   activation = "MaxoutWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = FALSE,
                   distribution = "multinomial",
                   hidden = c(50,50,50,50,50), # five layers of 50 nodes
                   epochs = 50,
                   nfolds=5,  #cross-valid part
                   fold_assignment =c("Random"),
                   keep_cross_validation_predictions = TRUE,
                   keep_cross_validation_fold_assignment = TRUE)
test.h2o<-as.h2o(data.frame(fit_test)) 
yhat_test_h2o <- h2o.predict(model, test.h2o) 
yhat_h2o<-as.data.frame(yhat_test_h2o)
yhat_h2o_mat<-cbind(yhat_h2o$p1,yhat_h2o$p2,yhat_h2o$p3,yhat_h2o$p4,yhat_h2o$p5,
                    yhat_h2o$p6,yhat_h2o$p7,yhat_h2o$p8,yhat_h2o$p9,yhat_h2o$p10,
                    yhat_h2o$p11,yhat_h2o$p12,yhat_h2o$p13,yhat_h2o$p14,yhat_h2o$p15,
                    yhat_h2o$p16,yhat_h2o$p17,yhat_h2o$p18,yhat_h2o$p19,yhat_h2o$p20)
result_dp<-yhat_h2o_mat%*%beta_train
rank_dp<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_dp[i,]<-4973+1-rank(result_dp[i,])}
r_avg_dp<-vector(length=470)
for(i in 1:470)
{r_avg_dp[i]<-mean(rank_dp[i,])}
err_dp<-vector(length=470)
for(i in 1:470)
{ err_dp[i]<-mean(rank_dp[i,][lyr[ind.test,-1][i,]!=0])/r_avg_dp[i]}



#mean of probs#
mean_prob<-(result_glm+result_gbm+result_rf)/3
rank_mean<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_mean[i,]<-4973+1-rank(mean_prob[i,])}
r_avg_mean<-vector(length=470)
for(i in 1:470)
{r_avg_mean[i]<-mean(rank_mean[i,])}
err_mean<-vector(length=470)
for(i in 1:470)
{ err_mean[i]<-mean(rank_mean[i,][lyr[ind.test,-1][i,]!=0])/r_avg_mean[i]}

####
#A vector#
#size > 5.5GB#
#data28_vector<-vector("list",length=2350)
#for(i in 1:2350)
#{ for(j in 1:28)
# {  data28_vector[[i]]<-c(data28_vector[[i]],data28_obs[[i]][j,])}}
#data28_com<-matrix(0,nrow=2350,ncol=28*1375)
#for(i in 1:2350)
#{data28_com[i,]<-data28_vector[[i]]}

#train&test#
#ind.train1<-sample(2350,2350*0.8)
#ind.test<-c(1:2350)[-ind.train]
#fit_data1<-data.frame(data28_com,as.factor(cluster))
#fit_train1<-fit_data1[ind.train,]
#fit_test1<-fit_data1[ind.test,]
#rank_test<-matrix(0,nrow=length(ind.test),ncol=4973)
#for(i in 1:length(ind.test))
#{ rank_test[i,]<-4973+1-rank(lyr[ind.test,-1][i,])}


####
#mean#
#data28_mean<-matrix(0,ncol=1375,nrow=2350)
#for(i in 1:2350)
#{  data28_mean[i,]<-colMeans(data28_obs[[i]])}


#train&test#
#ind.train2<-sample(2350,2350*0.8)
#ind.test2<-c(1:2350)[-ind.train]
#fit_data2<-data.frame(data28_mean,as.factor(cluster))
#fit_train2<-fit_data2[ind.train,]
#fit_test2<-fit_data2[ind.test,]
#rank_test<-matrix(0,nrow=length(ind.test),ncol=4973)
#for(i in 1:length(ind.test))
#{ rank_test[i,]<-4973+1-rank(lyr[ind.test,-1][i,])}

#GBM:mean#
#fit_gbm2<-gbm(as.factor.cluster.~.,data=fit_train2,distribution="multinomial",n.trees=100,shrinkage=0.1,interaction.depth =2)
#yhat_gbm2<-predict(fit_gbm2,fit_test2[,-1376],n.trees=100)
#yhat_gbm_factor2<-matrix(0,nrow=470,ncol=20)
#for(i in 1:470)
#{for(j in 1:20)
#{yhat_gbm_factor2[i,j]<-exp(yhat_gbm2[i,j,1])}}
#for(i in 1:470)
#{yhat_gbm_factor2[i,]<-yhat_gbm_factor2[i,]/sum(yhat_gbm_factor2[i,])}
#result_gbm2<-yhat_gbm_factor2%*%beta_lda
#rank_gbm2<-matrix(0,nrow=470,ncol=4973)
#for(i in 1:470)
#{ rank_gbm2[i,]<-rank(result_gbm2[i,])}
#abs_err2<-rowMeans(abs(rank_gbm2-rank_test))
#avg_abs_err2<-mean(abs_err2)

#gbm2_value<-vector(length=470)
#for(i in 1:470)
#{gbm2_value[i]<-which.max(yhat_gbm2[i,1:20,1])}
#sum(gbm2_value==fit_test2[,1376])/length(gbm2_value)

#PCA:2 components#
data28_pca2<-matrix(0,nrow=2350,ncol=1375*2)
for(i in 1:2350)
{ data28_pca2[i,]<-c(prcomp(data28_obs[[i]],scale=F)$rotation[,1],prcomp(data28_obs[[i]],scale=F)$rotation[,2])}
fit_train2<-data.frame(data28_pca2[ind.train,],as.factor(cluster_train))
fit_test2<-data.frame(data28_pca2[ind.test,])

#RF#
fit2<-randomForest(as.factor.cluster_train.~.,data=fit_train2,mtry=43,ntree=200,importance=TRUE)
yhat2<-predict(fit2,newdata=fit_test2,type="prob")
result_rf2<-yhat2%*%beta_train
rank_rf2<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_rf2[i,]<-4973+1-rank(result_rf2[i,])}
r_avg2<-vector(length=470)
for(i in 1:470)
{r_avg2[i]<-mean(rank_rf2[i,])}
err_rf2<-vector(length=470)
for(i in 1:470)
{ err_rf2[i]<-mean(rank_rf2[i,][lyr[ind.test,-1][i,]!=0])/r_avg2[i]}

#GBM#
fit_gbm2<-gbm(as.factor.cluster_train.~.,data=fit_train2,distribution="multinomial",n.trees=100,shrinkage=0.1,interaction.depth =2)
yhat_gbm2<-predict(fit_gbm2,fit_test2,n.trees=100,type="response")
yhat_gbm_factor2<-yhat_gbm2[1:470,1:20,1]
result_gbm2<-yhat_gbm_factor2%*%beta_train
rank_gbm2<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_gbm2[i,]<-4973+1-rank(result_gbm2[i,])}
r_avg<-vector(length=470)
for(i in 1:470)
{r_avg[i]<-mean(rank_gbm2[i,])}
err_gbm2<-vector(length=470)
for(i in 1:470)
{ err_gbm2[i]<-mean(rank_gbm2[i,][lyr[ind.test,-1][i,]!=0])/r_avg[i]}

#Ridge#
cv_glm2<-cv.glmnet(as.matrix(fit_train2[,-2751]),gamma_train,family="mgaussian",alpha=0)
fit.glm2<-glmnet(as.matrix(fit_train2[,-2751]),gamma_train,family="mgaussian",alpha=0,lambda=cv_glm2$lambda.min)
yhat_glm2<-predict(fit.glm2,as.matrix(fit_test2),type="response")
yhat_glm_mat2<-yhat_glm2[1:470,1:20,1]
result_glm2<-yhat_glm_mat2%*%beta_train
rank_glm2<-matrix(0,nrow=470,ncol=4973)
for(i in 1:470)
{ rank_glm2[i,]<-4973+1-rank(result_glm2[i,])}
r_avg1<-vector(length=470)
for(i in 1:470)
{r_avg1[i]<-mean(rank_glm2[i,])}
err_glm2<-vector(length=470)
for(i in 1:470)
{ err_glm2[i]<-mean(rank_glm2[i,][lyr[ind.test,-1][i,]!=0])/r_avg1[i]}

#barplot of PCA#
#data28_fv<-matrix(0,nrow=2350,ncol=28)
#for(i in 1:2350)
#{ data28_fv[i,]<-prcomp(data28_obs[[i]],scale=F)$sdev}
#Variance<-matrix(0,nrow=2350,ncol=28)
#for(i in 1:2350)
#{for(j in 1:28)
#{ Variance[i,j]<-(data28_fv[i,j])^2}}
#colnames(Variance)<-sprintf("PC%s",c(1:28))
#barplot(colMeans(Variance),ylab=c("Variance"),main=c("Mean Variance of Principle Components "))
