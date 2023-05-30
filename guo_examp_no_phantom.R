#set seed
#set.seed(16)

#define function for finding PPC and PCR
FindRisksSys <- function(alpha, beta, cvec, nvec, p.rrl, p.arl, mcsamps, samp = NULL){
  #if we haven't taken a prior sample, take one now
  made_samp <- FALSE
  if(length(samp) == 0){
    samp <- list()
    for(i in 1:length(alpha)){
      samp[[i]] <- rbeta(mcsamps, alpha[i], beta[i])
    }
    made_samp <- TRUE
  }
  
  #define subsystem and system priors
  subsys1 <- (1 - (1 - samp[[1]])*(1 - samp[[2]]))
  subsys2 <- samp[[6]] * samp[[7]]
  subsys3 <- samp[[10]]*(1-(1-samp[[11]])*(1-samp[[12]]))
  sampSys <- subsys1 * 
    (1-(1-samp[[3]])*(1-samp[[4]])*(1-samp[[5]])) * 
    (1-(1-subsys2)*(1-samp[[8]]*samp[[9]])) *
    (1-(1-subsys3)*(1-samp[[13]]))
  
  #get probabilities of tests passing
  for(i in 1:(length(alpha))){
    assign(paste0('test',i), pbinom(cvec[i], nvec[i], 1-samp[[i]]))
  }
  testSub1 <- pbinom(cvec[14], nvec[14], 1 - subsys1)
  testSub2 <- pbinom(cvec[15], nvec[15], 1 - subsys2)
  testSub3 <- pbinom(cvec[16], nvec[16], 1 - subsys3)
  testSys <- pbinom(cvec[17], nvec[17], 1 - sampSys)
  
  #calculate overall TIP and risks
  TIP <- testSys * 
    testSub1 * 
    (1-(1-test1)*(1-test2)) *
    (1-(1-test3)*(1-test4)*(1-test5)) *
    (1-(1-test6 * test7 *testSub2) * (1-test8*test9)) * 
    (1-(1-test10*(1-(1-test11)*(1-test12))*testSub3) * (1-test13))
  numerPostConsumRisk <- mean((sampSys<=p.rrl)*TIP)
  denomPostConsumRisk <- ifelse(mean(TIP) == 0, 1, mean(TIP))
  numerPostProducRisk <- mean((sampSys>=p.arl)*(1-TIP))  
  denomPostProducRisk <- ifelse(denomPostConsumRisk==1,1,1-denomPostConsumRisk)
  
  #return different lists depending on if samps already exists
  if(made_samp){
    return(list(PPR=numerPostProducRisk/denomPostProducRisk,
                PCR=numerPostConsumRisk/denomPostConsumRisk,
                samps=list(samp)))
  }
  
  list(PPR=numerPostProducRisk/denomPostProducRisk,
       PCR=numerPostConsumRisk/denomPostConsumRisk)
}

# assign priors and other values

alpha_vec <- (c(25,25,20,20,11,11,11,15,15,15,15,20,20) + 1)
beta_vec <- (c(1,1,1,1,1,1,1,1,1,1,1,1,1))
# 
# alpha_vec_first <- (c(25,25,20,20,11,11,11,15,15,15,15,20,20,25,25,25,20) + 1)
# beta_vec_first <- (c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

# alpha_vec <- alpha_vec_first/(alpha_vec_first + beta_vec_first)
# beta_vec <- beta_vec_first/(alpha_vec_first + beta_vec_first)
# 
# alpha_vec <- c(rep(.5, 13),rep(10, 3),3)
# beta_vec <- rep(.5,17)


p.rrl <- .95
p.arl <- .9
mcsamps <- 100000

alpha <- 1
beta <- .05

#get everything ready for finding system test
cvec <- rep(0,17)
nvec <- c(rep(0,16), 1)
both <- TRUE
iter <- 0
iter_c <- 0
nlistsys_prel <- 1
clistsys_prel <- 0
samps <- NULL

#find system test
while(both){
  
  
  
  #check PCR and PPR, save prior sample if need be
  risk_list <- FindRisksSys(alpha_vec,beta_vec,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
  if(length(samps) == 0){
    samps <- risk_list$samps[[1]] 
  }
  
  #do flowchart
  if(risk_list['PPR'] > alpha){
    if(cvec[length(cvec)] < (nvec[length(nvec)] - 1)){
      cvec[length(cvec)] <- cvec[length(cvec)] + 1
    }
    else{
      nvec[length(nvec)] <- nvec[length(nvec)] + 1
      cvec[length(cvec)] <- cvec[length(cvec)] + 1
    }
    
  }
  else if(risk_list['PCR'] > beta){
    nvec[length(nvec)] <- nvec[length(nvec)] + 1
  }
  
  if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
    both <- FALSE
    print(paste0('nsys is ', nvec[length(nvec)]))
    print(paste0('csys is ', cvec[length(cvec)]))
    print(risk_list)
  }
  #save vector of system tests and failures
  clistsys_prel[iter_c + 1] <- cvec[length(cvec)]
  nlistsys_prel[iter_c + 1] <- nvec[length(nvec)]
  iter_c <- iter_c + 1
  iter <- iter + 1
  print(iter)
}

nlistcomp_prel <- lapply(1:(length(nvec)-1), function(x) nvec[x])
clistcomp_prel <- lapply(1:(length(cvec)-1), function(x) cvec[x])
j <- iter + 2
iter <- 0
both <- TRUE
clistsys_prel <- c(0, clistsys_prel)
nlistsys_prel <- c(0, nlistsys_prel)
prices <- c(10,10,9,9,9,6,6,6,6,8,5,5,19,25,15,21,200)
prices[-length(prices)] <- prices[-length(prices)]
price_order <- order(prices)
hold_iter <- 1
sys_only_price <- prices[length(prices)] * nvec[length(nvec)]
component_prices <- 0
hold_min <- sys_only_price
hold_min_test <- hold_min_fail <- 0
costs <- numeric()
stop_test <- FALSE
iter_hold <- 0
while (!stop_test) {
  cvec[length(cvec)] <- clistsys_prel[j - 1]
  nvec[length(nvec)] <- nlistsys_prel[j - 1]
  j <- j - 1
  
  both <- TRUE
  while(both){
    risk_list <- FindRisksSys(alpha_vec,beta_vec,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
    if(risk_list['PPR'] > alpha){
      nvec <- ifelse(nvec == 0, nvec + 1, nvec)
      cvec[-length(cvec)] <- cvec[-length(cvec)] + 1
      nvec[-length(nvec)] <- ifelse(nvec[-length(nvec)] == cvec[-length(cvec)], nvec[-length(nvec)] + 1, nvec[-length(nvec)])
      nlistcomp_prel <- lapply(1:(length(nvec)-1), function(x) c(nlistcomp_prel[[x]], nvec[x]))
      clistcomp_prel <- lapply(1:(length(cvec)-1), function(x) c(clistcomp_prel[[x]], cvec[x]))
    }
    else if(risk_list['PCR'] > beta){
      nvec[-length(nvec)] <- nvec[-length(nvec)] + 1
    }
    
    if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
      both <- FALSE
    }
    
  }
  
  for(i in 1:(length(nvec) - 1)){
    all2 <- TRUE
    if(nvec[which(price_order == (length(nvec) - i))] == 0){
      all2 <- FALSE
    }
    while(all2){
      #pseudo code
      #take away an n
      #check if n is in nlistcomp_prel to take away a c
      #check if tests pass
      #if tests don't pass, change all 2 and restore what just happened
      
      comp_loc <- which(price_order == (length(nvec) - i))
      c_check <- FALSE
      if((nvec[comp_loc] == nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]])]) & (length(nlistcomp_prel[[comp_loc]]) > 1)){
        cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]]) - 1]
        c_check <- TRUE
      }
      nvec[comp_loc] <- nvec[comp_loc] - 1
      risk_list <- FindRisksSys(alpha_vec,beta_vec,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
      if(!((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta))){
        nvec[comp_loc] <- nvec[comp_loc] + 1
        all2 <- FALSE
        if(c_check){
          cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]])]
        }
      }
      else{
        if(c_check){
          clistcomp_prel[[comp_loc]] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]]) - 1]
          nlistcomp_prel[[comp_loc]] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]]) - 1]
        }
      }
      if(nvec[comp_loc] == 0){
        all2 <- FALSE
      }
    }
  }
  component_prices <- sum(nvec * prices)
  costs[iter] <- component_prices
  iter_hold <- iter_hold + 1
  if(component_prices < hold_min){
    hold_min <- component_prices
    hold_min_test <- nvec
    hold_min_fail <- cvec
    iter_hold <- 0
  }
  if(iter_hold == 10){
    stop_test <- TRUE
  }
  if(nvec[length(nvec)] == 0){
    stop_test <- TRUE
  }
  iter <- iter + 1
  print(iter)
  print(paste0('component_price: ',component_prices))
  print(paste0('system_only_price: ',sys_only_price))
  print(paste0('min_value: ', hold_min))
  print(paste0('min_config_n: ', hold_min_test))
  print(paste0('min_config_c: ', hold_min_fail))
  print(paste0('n components is ', nvec))
  print(paste0('c components is ', cvec))
  
}
risk_list <- FindRisksSys(alpha_vec,beta_vec,hold_min_fail,hold_min_test,p.rrl,p.arl,mcsamps, samps)
print(risk_list)
saveRDS(costs, 'costs.RDS')