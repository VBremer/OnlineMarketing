##############
### load packages
##############
library(bayesm)
library(ROCR)
source("helpers.r")

################
#### import data
################
load("data.RData")

#################
#### convert time
#################
all_data$time <- strptime(all_data$datetime, format="%Y-%m-%d %H:%M:%S")                 

##############
#### order
##############
all_data <- all_data[order(all_data$userID,all_data$datetime),]

#######################
#### specify Y variable
#######################
all_data$Y <- all_data$type=='CONVERSION' 

#################
#### prepare data 
#################                                                                
time <- all_data$time                                                            
userID <- matrix(all_data$userID)                                                
channel <- matrix(all_data$channel)                                            
Y <- matrix(all_data$Y)                                                          

data <- NULL                                                                     
count <- 0                                                                       
currentUserID <- userID[1]                                                       
erg_time <- time[1]                                                              
erg_channel <- channel[1]                                                       
erg_y <- Y[1]                                                                    

for (j in 2:nrow(all_data)){                                                     
  if (currentUserID==userID[j]){                                                 
    erg_time <- c(erg_time,time[j])                                              
    erg_channel <- c(erg_channel, channel[j])                                    
    erg_y <- c(erg_y,Y[j])                                                         
  }else{                            
    count <- count+1                                                            
    data[[count]] <- list(time=erg_time, channel=erg_channel,y=erg_y)           
    currentUserID <- userID[j]                                                   
    erg_time <- time[j]                                                          
    erg_channel <- channel[j]                                                    
    erg_y <- Y[j]                                                                
  }
}

#####################
#### specify channels
#####################
Xchannel <- list(
 seo = c("SEO"),
 sea = c("SEA"),
 other= c("RETARGETING", "PREISSUCHMASCHINE", "TYPE_IN", "AFFILIATE", "SOCIAL", "NEWSLETTER")
)

##################
#### build dataset 
##################
storedata <- preparedata(data,Xchannel)

##############
### set first parameters
##############
p=2 # number of choice alternatives                           

##############
### prepare data for analysis
##############
count <- 0
lgtdata <- preplgtdata(storedata)      

##############
### set parameters
##############
burn=10 ### number of burn in iterations        
R=50    ### number of MCMC iterations           
keep=2    ### every xth iteration is stored
ncomp=2    ### number of mixture components

##############
### set parms for priors
##############
Prior=list(ncomp=ncomp)
Mcmc=list(R=R,keep=keep)
Data=list(p=p,lgtdata=lgtdata)

##############
### start MCMC analysis
##############
out=rhierMnlRwMixture(Data=Data,Prior=Prior,Mcmc=Mcmc)

##############
### output analysis
##############
coef_names <- c("Intercept",paste("X",names(Xchannel)),"X-SEA|SEO","X-SEO|SEA","XConv",paste("Y",names(Xchannel)),"Y-SEA|SEO","Y-SEO|SEA","YConv","priorconv","IST","Session")

mixture_draws <- NULL
for (numberofelement in 1:ncol(lgtdata[[1]]$X)){
  erg <- NULL
  for (i in 1:ncomp){
    temp <- NULL
    for (j in (burn/keep):(R/keep)){
      temp <- c(temp,out$nmix[[3]][[j]][[i]]$mu[numberofelement])
    }
    erg <- rbind(erg,quantile(temp,c(0.05,.5,.95)))
  }
  mixture_draws[[numberofelement]] <-  list(coef_names[numberofelement],erg)
}
mixture_draws

##############
### predictive power
##############
lgtdata_ROC <- lgtdata
for (i in 1:length(lgtdata_ROC)){                      ### delete every other row from lgtdata
  rows <- c()
  for (k in 1: nrow(lgtdata_ROC[[i]]$X)){
    if (sum(lgtdata_ROC[[i]]$X[k,]) == 0){rows <- c(rows, k)}   
  }
  lgtdata_ROC[[i]]$X <- lgtdata_ROC[[i]]$X[-rows,]
}

mixture_draws_av <- NULL
for (i in 1:length(mixture_draws)){
  mixture_draws_av <- unname(c(mixture_draws_av, mixture_draws[[i]][[2]][1,2]))
}

result_bayesm <- mixture_draws_av

e <- exp(1)
conv <- c()
mult_bayes <- c()
for (i in 1:length(lgtdata_ROC)){
  conv <- c(conv, lgtdata_ROC[[i]]$y)
  for (k in 1:nrow(lgtdata_ROC[[i]]$X)){
    mult_bayes <- c(mult_bayes,sum(lgtdata_ROC[[i]]$X[k,]*result_bayesm))        
  }
}

conv <- conv-1                                                                   

probs <- matrix(0, ncol=1, nrow=length(conv))
for (i in 1:length(probs)){
  probs[i,1] <- 1/(1+e^-mult_bayes[i])                             
}

ROC_bayes <- matrix(NA, ncol=2, nrow=length(probs))        
colnames(ROC_bayes) <- list("p_i", "c_i")
ROC_bayes[,1] <- probs
ROC_bayes[,2] <- conv
ROC_bayes <- ROC_bayes[order(ROC_bayes[,1], decreasing=FALSE),]

##############
### ROC curve
##############
pred <- prediction(ROC_bayes[,1], ROC_bayes[,2])
perf <- performance(pred, measure = "tpr", x.measure = "fpr", fpr.stop=0.5)
plot(perf)
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
maxauc<-max(round(auc, digits = 5))
legend(0.2,0.5,maxauc,border="white",cex=1.7,box.col = "white")



