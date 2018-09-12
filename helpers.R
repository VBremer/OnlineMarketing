##################
#### build dataset 
##################
preparedata <- function(data,Xchannel){                         
  erg <- NULL                                                                     
  storedata = NULL                                                               
  
  for (i in 1:length(data)){                                      
    time <- data[[i]]$time                                     
    erg <- NULL                                                                   
    store <- NULL                                                                 
    y = NULL                                                                    
    count <- 1                                                                           
    
    ##############
    #### determine y
    ##############
    if(data[[i]]$y[1]=="FALSE"){                              
      y <- 1
      
      ##############
      #### determine x
      ##############
      priorSEO <- 0                                                          
      priorSEA <- 0
      
      for(k in 1:length(Xchannel)){                            
        if(is.element(data[[i]]$channel[1],Xchannel[[k]])){     
          erg <- c(erg,1)                                       
        }else{    
          erg <- c(erg,0)                                       
        }
      }
      
      erg <- c(erg,priorSEO,priorSEA)                          
      
      store <- rbind(store,erg)                                
      
      if(erg[1]==1){                                           
        priorSEO=1
      }
      if(erg[2]==1){                                            
        priorSEA=1
      }
      
    }else{
      time <- time[-1]                                          
      count <- 0         
    }
    
    if(length(data[[i]]$y)>1){                                   
      for (j in 2:length(data[[i]]$y)){                         
        erg <- NULL
        ##############
        #### determine y
        ##############
        count <- count + 1
        if(data[[i]]$y[j]=="FALSE"){                            
          
          y <- c(y,1)                                          
        }else{
          y <- c(y,2)                                          
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
        
        if(priorSEO==1&&erg[2]==1){                           
          erg <-c(erg,1) ### SEA|SEO                          
          erg[2] <- 0                                       
        }else{
          erg <-c(erg,0)                                      
        }
        
        if(priorSEA==1&&erg[1]==1){                           
          erg <-c(erg,1)### SEO|SEA                          
          erg[1] <- 0                                         
        }else{
          erg <-c(erg,0)                                      
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



##############
### prepare data for analysis
##############
preplgtdata <- function(lgtdata, obs){
  dataoutput <- NULL
  for(i in 1:length(lgtdata)){                
    journeylength <- length(lgtdata[[i]]$y)
    if(journeylength>4){                       
      count <- count + 1                       
      X=NULL
      y <- lgtdata[[i]]$y[1]                   
      
      ### model intrasession (X) ad-covariates # 
      Xtemp <- cbind(lgtdata[[i]]$X)             
      Xd <- matrix(c(Xtemp[1,]),nrow=1)
      Xconv <- 0 
      
      ### model intersession (Y) ad-covariates # length of time between a user?s 2 visits
      Yd <- matrix(rep(0,length(Xd)),nrow=1)
      Yconv <- 0
      
      ### model additional variables           
      IST <- 0 ## intersession time
      session <- 1 ## first session
      priorconv <- 0 ## prior conversion
      advar = cbind(priorconv,IST,session)    
      
      ###createX
      X=rbind(X,createX(p,na=NULL,nd=length(cbind(Xd,Xconv,Yd,Yconv,advar)),
                        Xa=NULL,Xd=cbind(Xd,Xconv,Yd,Yconv,advar),base=1,INT=T))
      
      for(j in 2:journeylength){               
        y <- c(y,lgtdata[[i]]$y[j])            
        priorconv <- ifelse(y[j-1]==2,1,0)    
        
        ### check if new session               
        lastsession <- session[j-1]            
        session <- c(session,session[j-1]+     # set session: if the a session is under 60min its the same session 
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