 
# Simulation code for stochastic individual-based social learning model with 
# either temporal or spatial variability in the environment as well as temporal model with adult learning

# Produces output for Figures 4,5,6, S2 and S3

# Running all parameter combination takes quite long if you don't have access
# to a computer cluster


######
#####
####
###
##
# TEMPORAL MODEL
##
###
####
#####
######


seq_temporal<- expand.grid(Nsim=10, tmax=7000, Nmax=1000, max_age=80, sigma= c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), u = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                           z=c(0.01,0.5,1),c= c(0.05, 0.1, 0.2), mu_pi = 0.005,mu_phi= 0.005, SL_Error=c(0,0.1,0.3), Nmodels = 2)




#Define simulation function

sim_temporal <- function(Nsim, tmax,Nmax ,max_age,sigma,u,z,c,mu_pi,mu_phi, SL_Error, Nmodels) {
  
  #Create empty list for output matrices of all N=Nsim simulation runs
  Combined_list <- list()
  
  #Loop over all N=Nsim separate simulations
  for(xsim in 1:Nsim){
    
    #Create population array with age-strategy classes
    
    n <- array( 0 , dim=c( max_age , 2 , 2, 2) ) # age, adapted = 2, Learning Strategy , IL = 1, Age Bias, copy old = 1
    
    #Half IL and half SL
    n[,1,1,1] <- floor(0.25*Nmax/max_age)
    n[,1,1,2] <- floor(0.25*Nmax/max_age)
    n[,1,2,1] <- floor(0.25*Nmax/max_age)
    n[,1,2,2] <- floor(0.25*Nmax/max_age)
    N <- sum(n)
    
    #Output objects
    
    PropIL  <- c()
    PropOld <- c()
    p_hat <- c()
    
    
    #Define Fertilities
    b <- c(1,1)
    
    #Define Survival Probs
    s=c(0.9 * sigma,0.9)
    
    
    for ( t in 1:tmax ) {
      
      #Number of individuals
      N <- sum(n)
      
      #Does the environement change at time t
      ut <- ifelse( runif(1) < u , 1 , 0 )
      
      # life cycle:
      # age (survival)
      # reproduction
      # environment change
      # learning
      
      # aging
      # loop over age classes, in reverse
      for ( i in (max_age-1):1 ) {
        for ( j in 1:2 ) { #j indexes non-adapted/adapted
          for (k in 1:2) { #k indexes learning strat
            for ( l in 1:2 ){ #l indexes age bias
              if (i==1 & k==1){ #Individual learning juveniles pay recruitment cost
                n[ i+1 , j , k, l ] <- rbinom( 1 , size=n[ i , j , k,  l ] , prob=s[j]*(1-c) )
              } else {           
                n[ i+1 , j , k, l ] <- rbinom( 1 , size=n[ i , j , k,  l ] , prob=s[j] )
              }
            }#l
          }#k
        }#j
      }#i
      
      # reproduction --- fill age class 1
      # produce offspring and then truncate N to Nmax by sampling from all offspring produced
      
      for ( j in 1:2 ){
        for ( k in 1:2 ){
          for (l in 1:2) {
            #Empty juvenile slots
            n[ 1 , j , k , l ] <- 0 
          }
        }
      }
      
      # babies get stored in a long table, and later sample from it and inserted into n array
      babies <- matrix( NA , nrow=Nmax*2 , ncol=2 )
      
      # columns are [1] learning strategy [2] Age Bias
      n_babies <- 0
      n_parents <- 0
      for ( i in 2:max_age ) {
        for ( j in 1:2 ){
          for ( k in 1:2 ) {
            for (l in 1:2){
              n_parents <- n[ i , j , k, l ]
              if (n_parents > 0){
                nn <- rbinom( 1 , size=n_parents , prob=b[j] )
                if ( nn > 0 ) {
                  #Assign to baby matrix with mutation
                  babies[ (n_babies+1):(n_babies+nn) , 1 ] <- ifelse(k==1, sample(c(1,2), 1, prob = c(1-mu_pi, mu_pi)),sample(c(2,1), 1, prob = c(1-mu_pi, mu_pi)))
                  babies[ (n_babies+1):(n_babies+nn) , 2 ] <- ifelse(l==1, sample(c(1,2), 1, prob = c(1-mu_phi, mu_phi)),sample(c(2,1), 1, prob = c(1-mu_phi, mu_phi)))
                  n_babies <- n_babies + nn
                }
              }
            }#l
          }#k
        }#j
      }#i
      
      
      # put babies in first age class, according to learning strat inherited
      pop_size_without_babies <- sum(n)
      baby_slots <- Nmax - pop_size_without_babies
      baby_slots <- min( baby_slots , n_babies )
      
      
      # sample babies and put them into n[1,1,,] - all are born non-adapted
      if ( baby_slots > 0 ) {
        # shorten and shuffle babies list
        babies <- babies[ 1:n_babies , ]
        babies <- babies[ sample(1:n_babies) , ]
        for ( i in 1:baby_slots ) {
          # go in order and slot in babies
          n[ 1 , 1 , babies[i,1],babies[i,2]] <- n[ 1 , 1 , babies[i,1],babies[i,2]] + 1
        }
      }
      
      #environmental change
      #all adults become non-adapted
      if (ut == 1){
        for ( i in 1:max_age) {
          for ( k in 1:2 ) { #k indexes learning strat
            for (l in 1:2) { #l indexes age bias
              
              n[ i , 1 , k, l ] <- sum(n[ i ,  , k, l ])
              n[ i , 2 , k, l ] <- 0
              
            }#k
          }#j
        }#i
      }
      
      
      # learning
      # each juvenile gets a chance to learn, either individually or socially
      for ( k in 1:2 ) {
        for (l in 1:2) {
          if ( n[1,1,k,l]>0 ){ 
            if ( k==1 ){
              
              #Individual Learners
              
              n_successful_ind <- rbinom( 1 , size=n[1,1,k,l] , prob=z)
              n[1,1,k,l] <- n[1,1,k,l] - n_successful_ind
              n[1,2,k,l] <- n_successful_ind
              
            } else {
              
              #Social Learners
              #Proportion of each age class
              Age_Prop <- c()
              Age_Prop[1] <- 0
              for (j in 2:max_age) {
                Age_Prop[j]  <- sum(n[j,,,])/sum(n[2:max_age,,,])
              }
              
              n_successful_soc <- 0
              for (x in 1:n[1,1,k,l]) {
                Age_Models <- sample(1:max_age, Nmodels, prob=Age_Prop, replace = TRUE)
                
                #Select age class depending on age bias
                Selected <- ifelse(l==1, max(Age_Models), min(Age_Models))
                
                #Sample from selected age class proportional to proportion of adapted individuals
                kk <- rbinom(1,size=1, prob = sum(n[Selected,2,,])/sum(n[Selected,,,]))
                n_successful_soc <- n_successful_soc + kk
              }
              
              n_successful_soc <- rbinom(1, n_successful_soc, (1-SL_Error))
              
              n[1,1,k,l] <- n[1,1,k,l] - n_successful_soc
              n[1,2,k,l] <- n_successful_soc
              
            }
          }
        }#l
      }#k
      
      # Store info in output objects
      
      PropIL[t] <- sum(n[,,1,]/sum(n))
      PropOld[t] <- sum(n[,,,1]/sum(n))
      p_hat[t] <- sum(n[1,2,,])/sum(n[1,,,])
      
      
    }#t
    
    Combined_list[[xsim]]<- list(PropIL= PropIL,
                                 PropOld=PropOld, 
                                 p_hat = p_hat) 
    
    
  }#xsim
  
  return(Combined_list)  
  
  
}


# pass to mclapply

library(parallel)

result_temporal <- mclapply(
  1:nrow(seq_temporal) ,
  function(i) sim_temporal(seq_temporal$Nsim[i], seq_temporal$tmax[i], seq_temporal$Nmax[i] , seq_temporal$max_age[i],
                           seq_temporal$sigma[i], seq_temporal$u[i], seq_temporal$z[i], seq_temporal$c[i], seq_temporal$mu_pi[i],
                           seq_temporal$mu_phi[i],seq_temporal$SL_Error[i], seq_temporal$Nmodels[i]),
  mc.cores=1)




######
#####
####
###
##
# SPATIAL MODEL
##
###
####
#####
######



seq_spatial<-expand.grid(Nsim=10, tmax=7000, NmaxPerGroup=500, max_age=80, sigma= c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), m = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                         z=c(0.01,0.5,1),c= c(0.05, 0.1, 0.2), mu_pi = 0.005,mu_phi= 0.005, SL_Error=c(0,0.1,0.3),Nmodels = 2, NGroups=4)



#Define simulation function

sim_spatial <- function(Nsim, tmax,NmaxPerGroup,max_age, sigma, m, z, c, mu_pi,mu_phi,SL_Error,Nmodels,NGroups) {
  
  #Create empty list for output matrices of all N=Nsim simulation runs
  Combined_list <- list()
  
  #Loop over all N=Nsim separate simulations
  for(xsim in 1:Nsim){
    
    Nmax <- NmaxPerGroup*NGroups
    
    #Create population array with age-strategy classes
    
    n <- array( 0 , dim=c(NGroups, max_age , 2 , 2, 2) ) # Group, age, adapted = 2, Learning Strategy , IL = 1, Age Bias, copy old = 1
    
    #Half IL and half SL
    n[,,1,1,1] <- floor(0.25*NmaxPerGroup/max_age)
    n[,,1,1,2] <- floor(0.25*NmaxPerGroup/max_age)
    n[,,1,2,1] <- floor(0.25*NmaxPerGroup/max_age)
    n[,,1,2,2] <- floor(0.25*NmaxPerGroup/max_age)
    
    N <- sum(n)
    
    #Output objects
    
    PropIL <- c()
    PropOld <- c()
    p_hat <- c()
    
    s <- c(0.9 * sigma,0.9)
    b <- c(1,1)
    
    for ( t in 1:tmax ) {

      #Number of individuals
      N <- sum(n)
      
      # life cycle:
      # age (survival)
      # reproduction
      # migration
      # learning
      
      # aging
      # loop over age classes, in reverse
      for (h in 1:NGroups) {
        for ( i in (max_age-1):1 ) {
          for ( j in 1:2 ) { #j indexes non-adapted/adapted
            for (k in 1:2) { #k indexes learning strat
              for ( l in 1:2 ){ #l indexes age bias
                if (i==1 & k==1){ #Individual learning juveniles pay recruitment cost
                  n[h, i+1 , j , k, l ] <- rbinom( 1 , size=n[h, i , j , k,  l ] , prob=s[j]*(1-c) )
                } else {           
                  n[h, i+1 , j , k, l ] <- rbinom( 1 , size=n[h, i , j , k,  l ] , prob=s[j] )
                }
              }#l
            }#k
          }#j
        }#i
      }#h
      
      # reproduction --- fill age class 1
      # produce offspring and then truncate N to Nmax by sampling from all offspring produced
      for (h in 1:NGroups) {
        for ( j in 1:2 ){
          for ( k in 1:2 ){
            for (l in 1:2) {
              #Empty juvenile slots
              n[h, 1 , j , k , l ] <- 0 
            }
          }
        }
      }
      # babies get stored in a long table, and later sample from it and inserted into n array
      babies <- array(NA, dim = c(NGroups, Nmax/NGroups, 2)) 
      
      # columns are [1] learning strategy [2] Age Bias
      
      n_babies <- c()
      
      for (h in 1:NGroups) {
        n_babies[h] <- 0
        n_parents <- 0
        for ( i in 2:max_age ) {
          for ( j in 1:2 ){
            for ( k in 1:2 ) {
              for (l in 1:2){
                n_parents <- n[h, i , j , k, l ]
                if (n_parents > 0){
                  nn <- rbinom( 1 , size=n_parents , prob=b[j] )
                  if ( nn > 0 ) {
                    #Assign to baby matrix with mutation
                    babies[h, (n_babies[h]+1):(n_babies[h]+nn) , 1 ] <- ifelse(k==1, sample(c(1,2), 1, prob = c(1-mu_pi, mu_pi)),sample(c(2,1), 1, prob = c(1-mu_pi, mu_pi)))
                    babies[h, (n_babies[h]+1):(n_babies[h]+nn) , 2 ] <- ifelse(l==1, sample(c(1,2), 1, prob = c(1-mu_phi, mu_phi)),sample(c(2,1), 1, prob = c(1-mu_phi, mu_phi)))
                    n_babies[h] <- n_babies[h] + nn
                  }
                }
              }#l
            }#k
          }#j
        }#i
      }
      
      
      # put babies in first age class, according to learning strat inherited
      pop_size_without_babies <- c()
      baby_slots <- c()
      
      for (h in 1:NGroups) {
        pop_size_without_babies[h] <- sum(n[h,,,,])
        baby_slots[h] <- NmaxPerGroup - pop_size_without_babies[h]
        baby_slots[h] <- min( baby_slots[h] , n_babies[h] )
      }
      
      
      
      # sample babies and put them into n[h,1,1,,] - all are born non-adapted
      for (h in 1:NGroups) {
        if ( baby_slots[h] > 0 ) {
          # shorten and shuffle babies list
          Newborns <- babies[h, 1:n_babies[h] , ]
          Newborns <- Newborns[sample(1:n_babies[h]) , ]
          for ( i in 1:baby_slots[h] ) {
            # go in order and slot in babies
            n[h, 1 , 1 , Newborns[i,1],Newborns[i,2]] <- n[h, 1 , 1 , Newborns[i,1],Newborns[i,2]] + 1
          }
        }
      }
      
      #Migration
      
      # columns are [1] learning strategy [2] Age Bias and [3] Age
      MigrantPool <- array(NA, dim = c(NGroups, Nmax/NGroups, 3)) 
      
      n_Migrants <- c()
      
      for (h in 1:NGroups) {
        n_Migrants[h] <- 0
        for ( i in 2:max_age ) {
          for ( j in 1:2 ){
            for ( k in 1:2 ) {
              for (l in 1:2){
                nn <- rbinom( 1 , size=n[h,i,j,k,l] , prob=m )
                if ( nn > 0 ) {
                  #Assign to migrant matrix
                  MigrantPool[h, (n_Migrants[h]+1):(n_Migrants[h]+nn) , 1 ] <- k
                  MigrantPool[h, (n_Migrants[h]+1):(n_Migrants[h]+nn) , 2 ] <- l
                  MigrantPool[h, (n_Migrants[h]+1):(n_Migrants[h]+nn) , 3 ] <- i
                  n_Migrants[h] <- n_Migrants[h] + nn
                  
                  #Remove individuals from Groups
                  n[h,i,j,k,l] <- n[h,i,j,k,l] - nn
                }
              }#l
            }#k
          }#j
        }#i
      }
      
      
      Migrants <- c()
      
      for (h in 1:NGroups) {
        Migrants <- rbind(Migrants, MigrantPool[h, 1:n_Migrants[h] , ])
      }
      Migrants <- Migrants[sample(1:nrow(Migrants)) , ]
      
      
      #Assign Migrants to groups keeping group size constant
      #Migrants are not adapted
      
      for (h in 1:NGroups) {
        AlreadyGone <- sum(n_Migrants[which((1:NGroups)<h)])
        if (n_Migrants[h] > 0){
          
          for (i in (AlreadyGone+1):sum(n_Migrants[which((1:NGroups)<=h)])) {
            n[h, Migrants[i,3] , 1 , Migrants[i,1],Migrants[i,2]] <- n[h, Migrants[i,3] , 1 , Migrants[i,1],Migrants[i,2]] + 1
          }
        }
      }
      
      
      # learning
      # each juvenile gets a chance to learn, either individually or socially
      for (h in 1:NGroups) {
        for ( k in 1:2 ) {
          for (l in 1:2) {
            if ( n[h,1,1,k,l]>0 ){ 
              if ( k==1 ){
                
                #Individual Learners
                
                n_successful_ind <- rbinom( 1 , size=n[h,1,1,k,l] , prob=z)
                n[h,1,1,k,l] <- n[h,1,1,k,l] - n_successful_ind
                n[h,1,2,k,l] <- n_successful_ind
                
              } else {
                
                #Social Learners
                #Proportion of each age class
                Age_Prop <- c()
                Age_Prop[1] <- 0
                for (j in 2:max_age) {
                  Age_Prop[j]  <- sum(n[h,j,,,])/sum(n[h,2:max_age,,,])
                }
                
                n_successful_soc <- 0
                for (x in 1:n[h,1,1,k,l]) {
                  Age_Models <- sample(1:max_age, Nmodels, prob=Age_Prop, replace = TRUE)
                  
                  #Select age class depending on age bias
                  Selected <- ifelse(l==1, max(Age_Models), min(Age_Models))
                  
                  #Sample from selected age class proportional to proportion of adapted individuals
                  kk <- rbinom(1,size=1, prob = sum(n[h,Selected,2,,])/sum(n[h,Selected,,,]))
                  n_successful_soc <- n_successful_soc + kk
                }
                
                n_successful_soc <- rbinom(1, n_successful_soc, (1-SL_Error))
                
                n[h,1,1,k,l] <- n[h,1,1,k,l] - n_successful_soc
                n[h,1,2,k,l] <- n_successful_soc
                
              }
            }
          }#l
        }#k
      }#h
      
      # Store info in output objects
      
      PropIL[t] <- sum(n[,,,1,]/sum(n))
      PropOld[t] <- sum(n[,,,,1]/sum(n))
      p_hat[t] <- sum(n[,1,2,,])/sum(n[,1,,,])
      
      
    }#t
    
    Combined_list[[xsim]]<- list(PropIL= PropIL,
                                 PropOld=PropOld, 
                                 p_hat = p_hat) 
     
    
    
  }#xsim
  
  return(Combined_list)  
  
}


# pass to mclapply
result_spatial <- mclapply(
  1:nrow(seq_spatial) ,
  function(i) sim_spatial(seq_spatial$Nsim[i], seq_spatial$tmax[i], seq_spatial$NmaxPerGroup[i] , seq_spatial$max_age[i],
                          seq_spatial$sigma[i], seq_spatial$m[i], seq_spatial$z[i], seq_spatial$c[i], seq_spatial$mu_pi[i],
                          seq_spatial$mu_phi[i],seq_spatial$SL_Error[i], seq_spatial$Nmodels[i], seq_spatial$NGroups[i]),
  mc.cores=1)





######
#####
####
###
##
# ADULT LEARNING MODEL
##
###
####
#####
######




seq_adult<- expand.grid(Nsim=10, tmax=7000, Nmax=1000, max_age=80, sigma= c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), u = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                        z=c(0.5),c= c(0.05), mu_pi = 0.005,mu_phi= 0.005, SL_Error=c(0,0.3), Nmodels = 2, beta = c(0,0.01,0.1, 1))



#Define simulation function

sim_adult <- function(Nsim, tmax,Nmax ,max_age,sigma,u,z,c,mu_pi,mu_phi, SL_Error, Nmodels, beta) {
  
  #Create empty list for output matrices of all N=Nsim simulation runs
  Combined_list <- list()
  
  #Loop over all N=Nsim separate simulations
  for(xsim in 1:Nsim){
    
    #Create population array with age-strategy classes
    
    n <- array( 0 , dim=c( max_age , 2 , 2, 2) ) # age, adapted = 2, Learning Strategy , IL = 1, Age Bias, copy old = 1
    
    #Half IL and half SL
    n[,1,1,1] <- floor(0.25*Nmax/max_age)
    n[,1,1,2] <- floor(0.25*Nmax/max_age)
    n[,1,2,1] <- floor(0.25*Nmax/max_age)
    n[,1,2,2] <- floor(0.25*Nmax/max_age)
    N <- sum(n)
    
    #Output objects
    
    PropIL  <- c()
    PropOld <- c()
    p_hat <- c()
    
    
    #Define Fertilities
    b <- c(1,1)
    
    #Define Survival Probs
    s=c(0.9 * sigma,0.9)
    
    
    for ( t in 1:tmax ) {
      print(t)
      
      #Number of individuals
      N <- sum(n)
      
      #Does the environement change at time t
      ut <- ifelse( runif(1) < u , 1 , 0 )
      
      # life cycle:
      # age (survival)
      # reproduction
      # environment change
      # learning
      
      # aging
      # loop over age classes, in reverse
      for ( i in (max_age-1):1 ) {
        for ( j in 1:2 ) { #j indexes non-adapted/adapted
          for (k in 1:2) { #k indexes learning strat
            for ( l in 1:2 ){ #l indexes age bias
              if (i==1 & k==1){ #Individual learning juveniles pay recruitment cost
                n[ i+1 , j , k, l ] <- rbinom( 1 , size=n[ i , j , k,  l ] , prob=s[j]*(1-c) )
              } else {           
                n[ i+1 , j , k, l ] <- rbinom( 1 , size=n[ i , j , k,  l ] , prob=s[j] )
              }
            }#l
          }#k
        }#j
      }#i
      
      # reproduction --- fill age class 1
      # produce offspring and then truncate N to Nmax by sampling from all offspring produced
      
      for ( j in 1:2 ){
        for ( k in 1:2 ){
          for (l in 1:2) {
            #Empty juvenile slots
            n[ 1 , j , k , l ] <- 0 
          }
        }
      }
      
      # babies get stored in a long table, and later sample from it and inserted into n array
      babies <- matrix( NA , nrow=Nmax*2 , ncol=2 )
      
      # columns are [1] learning strategy [2] Age Bias
      n_babies <- 0
      n_parents <- 0
      for ( i in 2:max_age ) {
        for ( j in 1:2 ){
          for ( k in 1:2 ) {
            for (l in 1:2){
              n_parents <- n[ i , j , k, l ]
              if (n_parents > 0){
                nn <- rbinom( 1 , size=n_parents , prob=b[j] )
                if ( nn > 0 ) {
                  #Assign to baby matrix with mutation
                  babies[ (n_babies+1):(n_babies+nn) , 1 ] <- ifelse(k==1, sample(c(1,2), 1, prob = c(1-mu_pi, mu_pi)),sample(c(2,1), 1, prob = c(1-mu_pi, mu_pi)))
                  babies[ (n_babies+1):(n_babies+nn) , 2 ] <- ifelse(l==1, sample(c(1,2), 1, prob = c(1-mu_phi, mu_phi)),sample(c(2,1), 1, prob = c(1-mu_phi, mu_phi)))
                  n_babies <- n_babies + nn
                }
              }
            }#l
          }#k
        }#j
      }#i
      
      
      # put babies in first age class, according to learning strat inherited
      pop_size_without_babies <- sum(n)
      baby_slots <- Nmax - pop_size_without_babies
      baby_slots <- min( baby_slots , n_babies )
      
      
      # sample babies and put them into n[1,1,,] - all are born non-adapted
      if ( baby_slots > 0 ) {
        # shorten and shuffle babies list
        babies <- babies[ 1:n_babies , ]
        babies <- babies[ sample(1:n_babies) , ]
        for ( i in 1:baby_slots ) {
          # go in order and slot in babies
          n[ 1 , 1 , babies[i,1],babies[i,2]] <- n[ 1 , 1 , babies[i,1],babies[i,2]] + 1
        }
      }
      
      #environmental change
      #all adults become non-adapted
      if (ut == 1){
        for ( i in 1:max_age) {
          for ( k in 1:2 ) { #k indexes learning strat
            for (l in 1:2) { #l indexes age bias
              
              n[ i , 1 , k, l ] <- sum(n[ i ,  , k, l ])
              n[ i , 2 , k, l ] <- 0
              
            }#k
          }#j
        }#i
      }
      
      
      # learning
      # all gets a chance to learn, either individually or 
      for (i in 1:max_age) {
        
        for ( k in 1:2 ) {
          for (l in 1:2) {
            if ( n[i,1,k,l]>0 ){ 
              
              #Decide whether individual gets chance to learn
              if (rbinom(1,1, prob = exp(-beta*(i-1)))==1 ){
                
                if ( k==1 ){
                  
                  #Individual Learners
                  
                  n_successful_ind <- rbinom( 1 , size=n[i,1,k,l] , prob=z)
                  n[i,1,k,l] <- n[i,1,k,l] - n_successful_ind
                  n[i,2,k,l] <- n[i,2,k,l] + n_successful_ind
                  
                } else {
                  
                  #Social Learners
                  #Proportion of each age class
                  Age_Prop <- c()
                  Age_Prop[1] <- 0
                  for (j in 2:max_age) {
                    Age_Prop[j]  <- sum(n[j,,,])/sum(n[2:max_age,,,])
                  }
                  
                  n_successful_soc <- 0
                  for (x in 1:n[i,1,k,l]) {
                    Age_Models <- sample(1:max_age, Nmodels, prob=Age_Prop, replace = TRUE)
                    
                    #Select age class depending on age bias
                    Selected <- ifelse(l==1, max(Age_Models), min(Age_Models))
                    
                    #Sample from selected age class proportional to proportion of adapted individuals
                    kk <- rbinom(1,size=1, prob = sum(n[Selected,2,,])/sum(n[Selected,,,]))
                    n_successful_soc <- n_successful_soc + kk
                  }
                  
                  n_successful_soc <- rbinom(1, n_successful_soc, (1-SL_Error))
                  
                  n[i,1,k,l] <- n[i,1,k,l] - n_successful_soc
                  n[i,2,k,l] <- n[i,2,k,l] + n_successful_soc
                  
                }
              }
            }
          }#l
        }#k
      }#i
      
      # Store info in output objects
      
      PropIL[t] <- sum(n[,,1,]/sum(n))
      PropOld[t] <- sum(n[,,,1]/sum(n))
      p_hat[t] <- sum(n[1,2,,])/sum(n[1,,,])
      
      
    }#t
    
    Combined_list[[xsim]]<- list(PropIL= PropIL,
                                 PropOld=PropOld, 
                                 p_hat = p_hat) 
    
    
  }#xsim
  
  return(Combined_list)  
  
  
}


# pass to mclapply

library(parallel)

result_adult <- mclapply(
  1:nrow(seq_adult) ,
  function(i) sim_adult(seq_adult$Nsim[i], seq_adult$tmax[i], seq_adult$Nmax[i] , seq_adult$max_age[i],
                        seq_adult$sigma[i], seq_adult$u[i], seq_adult$z[i], seq_adult$c[i], seq_adult$mu_pi[i],
                        seq_adult$mu_phi[i],seq_adult$SL_Error[i], seq_adult$Nmodels[i], seq_adult$beta[i]),
  mc.cores=1)








