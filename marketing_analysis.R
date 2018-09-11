################
#### import data
################
load("data.RData")
#################
#### convert time
#################
all_data$time <- strptime(all_data$datetime, format="%Y-%m-%d %H:%M:%S")                 

##############
#### set ID, Ad-Channels
##############
all_data$userID <- all_data$userkey                                              # set Id and channels/just for future projects,not necessary, but easier
all_data$channel <- all_data$publisher
all_data<-all_data[order(all_data$userkey,all_data$datetime),]

#######################
#### specify Y variable
#######################
all_data$Y <- all_data$type=='conversion' #& all_data$publisher=='Google Adwords' # Y = only user who made a sea/sea/only - conversion

#################
#### prepare data 
#################                                                                # set new objects
time <- all_data$time                                                            # new object "time" from all_data_time
userID <- matrix(all_data$userID)                                                # new object "userID" from all_data_userID
channel <- matrix(all_data$publisher)                                            # new object "channel" from all_data_publisher
Y <- matrix(all_data$Y)                                                          # new object "Y" from all_data_Y

data <- NULL                                                                     # set data "NULL"
count <- 0                                                                       # set count 0
currentUserID <- userID[1]                                                       # userID now from the first user/first data [1]  
erg_time <- time[1]                                                              # time from the first user/first data [1]
erg_channel <- channel[1]                                                        # channel from the first user/first data [1]
erg_y <- Y[1]                                                                    # Y from the first user/first data (true or false) [1]

for (j in 2:nrow(all_data)){                                                     # for "j" from row 2 till the end of all_data:::
  if (currentUserID==userID[j]){                                                 # if current user equals the userID from "j" 
    erg_time <- c(erg_time,time[j])                                              # concatenate erg_time and the time from user "j" to a vector ,
    erg_channel <- c(erg_channel, channel[j])                                    # concatenate erg_channel and the channel from user "j" to a vector,
    erg_y <- c(erg_y,Y[j])                                                       # and concatenate erg_y and the Y-variable from user "j" to a vector   
  }else{                            
    count <- count+1                                                             # otherwise count plus 1,
    data[[count]] <- list(time=erg_time, channel=erg_channel,y=erg_y)            # add new object -> list of time, channel and y
    currentUserID <- userID[j]                                                   # then current user is set as user "j",
    erg_time <- time[j]                                                          # erg_time is set as time from user "j",   
    erg_channel <- channel[j]                                                    # erg_channel is set as channel from user "j",
    erg_y <- Y[j]                                                                # and erg_y is set das Y from user "j"
  }
}

#####################
#### specify channels
#####################
  Xchannel <- list(
   seo = c("Google DE"),
   sea = c("Google Adwords"),
   other= c("d3media","Roboter Netz","idealo.de","Elektor","Affilinet","Google Display Network","geizhals.de","SypplyFrame","Reaktivierungsmail","ciao.de","shopping.com"
            ,"Newsletter","chip-preisvergleich","billiger.de","preistrend.de","g?nstiger.de","hardwareschotte.de","hukd.mydealz.de","Facebook")
   )
##################
#### build dataset 
##################
preparedata <- function(data,Xchannel){                         # new function which consists the objects Xchannel and data
  erg <- NULL                                                                     
  storedata = NULL                                                               
                      
  for (i in 1:length(data)){                                    # for "i" from 1 till the end of data:::    
    time <- data[[i]]$time                                      # set time from data_time
    erg <- NULL                                                                   
    store <- NULL                                                                 
    y = NULL                                                                    
    count <- 1                                                                           
      
    ##############
    #### determine y
    ##############
    if(data[[i]]$y[1]=="FALSE"){                               # if y-1 from data = FALSE (means no conversion) --> y=1
      y <- 1
    
      ##############
      #### determine x
      ##############
      priorSEO <- 0                                                          
      priorSEA <- 0
      
      for(k in 1:length(Xchannel)){                             # for "k" from 1 till the end of Xchannel:::
        if(is.element(data[[i]]$channel[1],Xchannel[[k]])){     
          erg <- c(erg,1)                                       # concatenate erg from before and 1
        }else{    
          erg <- c(erg,0)                                       # otherwise concatenate erg from before and 0
        }
       }

      erg <- c(erg,priorSEO,priorSEA)                           # concatenate erg, priorSEO and prior SEA (they are still 0) to a vector
      
      store <- rbind(store,erg)                                 # "bind" store and erg into a matrix
      
      if(erg[1]==1){                                            # if seo=1 -> prior seo=1 
        priorSEO=1
      }
      if(erg[2]==1){                                            # if sea=1 -> prior sea=1
        priorSEA=1
      }
      
     }else{
      time <- time[-1]                                          
      count <- 0         
    }
    
   if(length(data[[i]]$y)>1){                                   
      for (j in 2:length(data[[i]]$y)){                         # for "j" from 2 till the end of y:::
        erg <- NULL
        ##############
        #### determine y
        ##############
        count <- count + 1
        if(data[[i]]$y[j]=="FALSE"){                            # if y-"j" from data = false/no conversion
                                              
          y <- c(y,1)                                           # concatenate y and 1/no conversion
        }else{
          y <- c(y,2)                                           # concatenate y and 2/conversion
        }
          ##############
          #### determine x
          ##############
          for(k in 1:length(Xchannel)){                          
            if(is.element(data[[i]]$channel[j],Xchannel[[k]])){ 
              erg <- c(erg,1)
            }else{
              erg <- c(erg,0)
            }
              }
          
          if(priorSEO==1&&erg[2]==1){                           # if SEO was 1 before and SEA=1
            erg <-c(erg,1) ### SEA|SEO                          # SEA|SEO = 1
            erg[2] <- 0                                         # SEA = 0
          }else{
            erg <-c(erg,0)                                      # otherwise SEA|SEO = 0
          }
          
          if(priorSEA==1&&erg[1]==1){                           # if SEA was 1 before and SEO=1
            erg <-c(erg,1)### SEO|SEA                           # SEO|SEA = 1
            erg[1] <- 0                                         # SEO = 0
          }else{
            erg <-c(erg,0)                                      # otherwise SEO|SEA = 0 
          }
         
          if(erg[1]==1){                                      
            priorSEO=1
          }
          if(erg[2]==1){
            priorSEA=1
          }
          
          store <- rbind(store,erg)
      }
    }
    storedata[[i]] <- list(y=y,X=store,time=time)

  }
  storedata
}

storedata <- preparedata(data,Xchannel)

##############
### load packages
##############
library(bayesm)

##############
### set first parameters
##############
p=2 # number of choice alternatives                           

##############
### prepare data for analysis
##############
count<-0
preplgtdata <- function(lgtdata, obs){
  dataoutput <- NULL
  for(i in 1:length(lgtdata)){                 # for "i" till the end of lgtdata:::
    journeylength <- length(lgtdata[[i]]$y)
    if(journeylength>4){                       # only having user with at least x queries
      count <- count + 1                       # start with next data/user
      X=NULL
      y <- lgtdata[[i]]$y[1]                   
      
      ### model intrasession (X) ad-covariates # 
      Xtemp <- cbind(lgtdata[[i]]$X)             
      Xd <- matrix(c(Xtemp[1,]),nrow=1)
      Xconv <- 0 
      
      ### model intersession (Y) ad-covariates # length of time between a user?s 2 visits
      Yd <- matrix(rep(0,length(Xd)),nrow=1)
      Yconv <- 0
      
      ### model additional variables           # set new variables
      IST <- 0 ## intersession time
      session <- 1 ## first session
      priorconv <- 0 ## prior conversion
      advar = cbind(priorconv,IST,session)     # concatenate priorconv, IST and session
      
      ###createX
      X=rbind(X,createX(p,na=NULL,nd=length(cbind(Xd,Xconv,Yd,Yconv,advar)),
                        Xa=NULL,Xd=cbind(Xd,Xconv,Yd,Yconv,advar),base=1,INT=T))
      
      for(j in 2:journeylength){               # for "j" from 2 till the end of journeylength:::
        y <- c(y,lgtdata[[i]]$y[j])            
        priorconv <- ifelse(y[j-1]==2,1,0)     # prior conv
        
        ### check if new session               
        lastsession <- session[j-1]            # set last session (session-1)
        session <- c(session,session[j-1]+     # set session: if the a session is under 60min it?s the same session 
                       ifelse(difftime(lgtdata[[i]]$time[j],lgtdata[[i]]$time[j-1],units="mins")<60,0,1))
        
        ### model session specific covariates  
        if(lastsession==session[j]){              # if last session is current session
          Xd <- Xd + matrix(c(Xtemp[j,]),nrow=1)  
          Xconv <- Xconv + priorconv               
          
          ### intersession time = 0
          IST <- c(IST, IST[j-1])
        }else{
          ###model intersession (Y) ad-covariates
          Yd <- Yd + Xd
          Yconv <- Yconv + Xconv
          
          Xconv <- priorconv
          Xd <- matrix(c(Xtemp[j,]),nrow=1)
          
          ### calculate intersession time
          IST <- c(IST, log(as.numeric(difftime(lgtdata[[i]]$time[j],lgtdata[[i]]$time[j-1],units="hours"))+1))
        }
        
        ### model additional variables
        advar = cbind(priorconv,IST[j],session[j])
        
        ### createX
        X=rbind(X,createX(p,na=NULL,nd=length(cbind(Xd,Xconv,Yd,Yconv,advar)),
                          Xa=NULL,Xd=cbind(Xd,Xconv,Yd,Yconv,advar),base=1,INT=T)) 
      }
      dataoutput[[count]] <- list(X=X, y=y)
    }
  }
  dataoutput
}

##############
### prepare data for analysis
##############
lgtdata <- preplgtdata(storedata)      # use function with storedata data and save it under lgtdata

##############
### set parameters
##############
burn=10000 ### number of burn in iterations        
R=20000    ### number of MCMC iterations           
keep=20     ### every xth iteration is stored
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

###################
## calculating mean/output_2
###################

probs <- apply(out$nmix$probdraw,2,mean)
mixture_draws2 <- NULL
for (numberofelement in 1:ncol(lgtdata[[1]]$X)){
  erg <- NULL
  for (i in 1:ncomp){
    temp <- NULL
    for (j in (burn/keep):(R/keep)){
      temp <- c(temp,out$nmix[[3]][[j]][[i]]$mu[numberofelement])
    }
    erg <- rbind(erg,quantile(temp,c(0.05,.5,.95))*probs[i])
  }
  erg <- apply(erg, 2, sum)
  mixture_draws2[[numberofelement]] <-  list(coef_names[numberofelement],erg)
}
