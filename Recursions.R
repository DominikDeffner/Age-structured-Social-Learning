

# Recursions to simulate age-structured social learning model

# Set IL_Only to "Yes" for population with only individual learners
# Set Stoch_E to "Yes" for stochastically changing environment

# Includes code for simple plot with time dynamics in the end

recurs <- function( tmax=1000 ,
                      max_age = 80  ,
                      b=c(0.35,0.35) ,
                      s=c(0.85,0.93),
                      u=0.01,
                      z=0.5, 
                      c=0.01,
                      phi = 1,
                      SL_Error = 0,
                      IL_Only = "No", 
                      Stoch_E = "Yes") {
  

  #Create population array with age-strategy classes
  
  n <- array( 0 , dim=c( max_age , 2 , 2) ) # age, adapted = 2, Learning Strategy , IL = 1
  
  #Half IL and half SL
  if ( IL_Only == "Yes"){
    
    if (max_age == 2){
      n[1,1,1] <- 1
      n[1,1,2] <- 0
    } else {
      n[,1,1] <- 1/max_age
      n[,1,2] <- 0
    }

  } else {
    
  if (max_age == 2){
    n[1,1,1] <- 0.5
    n[1,1,2] <- 0.5
  } else {
    n[,1,1] <- 0.5/max_age
    n[,1,2] <- 0.5/max_age
  }
  }
  
  N <- sum(n)
  
  #Output objects
  
  lambda <- c()
  lambdaSL <- c()
  lambdaIL <- c()
  Juveniles <- c()
  PropAdapt <- c()
  AdaptedPerAge <- matrix(nrow = max_age, ncol = tmax)
  PropIL <- c()
  EnvChange <- c()
  

  
  for ( t in 1:tmax ) {
    print(t)
    
    #Number of individuals
    N <- sum(n)
    
    if (max_age == 2){
    N_IL <- sum(n[1,,1])
    N_SL <- sum(n[1,,2])
    } else {
    N_IL <- sum(n[,,1])
    N_SL <- sum(n[,,2])
    }
    
    
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
             if (i==1 & k==1){ #Individual learning juveniles pay recruitment cost
               n[ i+1 , j , k]  <- n[ i , j , k] * s[j] * (1-c) 
             } else {           
                n[ i+1 , j , k] <- n[ i , j , k] * s[j]
             }
        }#k
      }#j
    }#i
    

    #Empty juvenile slots
    
    for ( j in 1:2 ){
      for ( k in 1:2 ){
        for (l in 1:2) {
        n[ 1 , j , k ] <- 0 
        }
       }
    }
    
    # reproduction --- fill age class 1

    # babies get stored in a vector
    
    classes <- expand.grid(age=2:max_age, j=1:2, k=1:2)
    babies <- rep(0, nrow(classes))
   
    for ( i in 2:max_age ) {
      for ( j in 1:2 ){
        for ( k in 1:2 ) {
          n_parents <- n[ i , j , k]
          if (n_parents > 0){
          nn <- n_parents * b[j]
          if ( nn > 0 ) {
            #Assign to vector 
            babies[which(classes$age==i & classes$j==j & classes$k==k)] <- babies[which(classes$age==i & classes$j==j & classes$k==k)] + nn
           }
          }
        }#k
      }#j
    }#i
    
    #Assign babies to juvenile class
    
    for ( k in 1:2 ) {
          n[1,1, k] <- sum(babies[which(classes$k==k)])
      
    }

    #environmental change
    
    if (Stoch_E == "Yes") {
      ut <- ifelse( runif(1) < u , 1 , 0 )
      EnvChange[t] <- ifelse(ut==1, 1, 0)
      if (ut == 1){
        for ( i in 1:max_age) {
          for ( k in 1:2 ) { #k indexes learning strat
            n[ i , 1 , k ] <- sum(n[ i ,  , k ])
            n[ i , 2 , k ] <- 0
          }#k
        }#j
      }#i
    } else {
      for ( i in 1:max_age) {
        for ( k in 1:2 ) { #k indexes learning strat
          
          n[ i , 1 , k ] <- n[ i , 1 , k ] + u * n[ i , 2 , k ]
          n[ i , 2 , k ] <- (1-u) * n[ i , 2 , k ]
          
        }#k
      }#j
    
    }
    
    
    # learning
    # each juvenile gets a chance to learn, either individually or socially
    for ( k in 1:2 ) {
       if ( n[1,1,k]>0 ){ 
        if ( k==1 ){
          
          #Individual Learners
          p_successful_ind <- n[1,1,k] * z
          n[1,1,k] <- n[1,1,k] - p_successful_ind
          n[1,2,k] <- p_successful_ind
          
      } else {
        
        #Social Learners
        #Proportion of each age class
        if (max_age == 2){
          p_successful_soc <- sum(n[2,2,])/sum(n[2,,])
        } else {
          
        a <- c()
        a[1] <- 0
        for (j in 2:max_age) {
        a[j]  <- sum(n[j,,])/sum(n[2:max_age,,])
        }
        
        #Proportion of adaptive behavior in each age class
        q <- c()
        for (j in 1:max_age) {
          q[j]  <- sum(n[j,2,])/sum(n[j,,])
        }
    
        p_successful_soc <- sum((a^2)*q) #Equal pairs
          
        for (i in 3:max_age) {
          for (j in 2:(i-1)) {
           p_successful_soc <- p_successful_soc + 2 * a[i]  *a[j] * ( q[i] * (phi/(phi+1))  + q[j] * (1/(phi+1)) ) 
          }
        }
                           
        }
        
        n_successful_soc <- n[1,1,k] * p_successful_soc * (1-SL_Error)
        n[1,1,k] <- n[1,1,k] - n_successful_soc
        n[1,2,k] <- n_successful_soc
         
         
        }
       }
    }#k
    
  # Store info in output objects
    
    if (max_age == 2){
      lambda[t] <- sum(n[1,,])
      lambdaIL[t] <- sum(n[1,,1]) / N_IL
      lambdaSL[t] <- sum(n[1,,2]) / N_SL
      
    } else {
      
      lambda[t] <- sum(n)
      lambdaIL[t] <- sum(n[,,1]) / N_IL
      lambdaSL[t] <- sum(n[,,2]) / N_SL
      
    }
    
   Juveniles[t] <- sum(n[1,,])
   PropAdapt[t] <- sum(n[,2,]/sum(n))
   PropIL[t] <- sum(n[,,1]/sum(n))

   for (x in 1:max_age) {
     AdaptedPerAge[x,t] <- sum(n[x,2,]/sum(n[x,,]))
   }
   
   
   #Renormalize population size
   
   if (max_age == 2){
     n[1,,] <- n[1,,]/sum(n[1,,])
   } else {
     n <- n/sum(n)
   }

   }#t
  
  return(list(n=n, 
              lambda=lambda, 
              lambdaIL=lambdaIL,
              lambdaSL=lambdaSL,
              Juveniles=Juveniles,
              Adapt=PropAdapt, 
              IL= PropIL,
              SL= 1- PropIL, 
              Change=EnvChange, 
              AdaptedPerAge=AdaptedPerAge))
  
}

result <- recurs()


par(mfrow=c(1,2))
plot(apply(result$AdaptedPerAge, 1, mean), type = "l", ylim = c(0,1), ylab = "Proportion Adapted", xlab = "Age")

plot(result$lambda/max(result$lambda), type= "l", ylim = c(0,1), ylab = "")
par(new=TRUE)
plot(result$SL,type = "l", ylim = c(0,1), col="green",  yaxt = "n", ylab = "")
par(new=TRUE)
plot(result$Adapt, type = "l", ylim = c(0,1), col = "red", yaxt = "n", ylab = "")
legend("topright", c("Fitness", "Prop Adapted", "Prop SL"), col = c("black", "red", "green"), lty = 1, cex = 0.6)








