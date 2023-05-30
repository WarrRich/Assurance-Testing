#set seed
set.seed(16)

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
  
  #define system priors
  sampSys <- samp[[1]] * samp[[2]]
  
  #get probabilities of tests passing
  for(i in 1:(length(alpha))){
    assign(paste0('test',i), pbinom(cvec[i], nvec[i], 1-samp[[i]]))
  }
  testSys <- pbinom(cvec[length(alpha)], nvec[length(beta)], 1 - sampSys)
  
  #calculate overall TIP and risks
  TIP <- testSys * 
    test1 *
    test2
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

alpha_vec <- c(8,8,0)
beta_vec <- c(4,4,0)


p.rrl <- .9
p.arl <- .95
mcsamps <- 1000000

alpha <- .05
beta <- .05

#get everything ready for finding system test
cvec <- rep(0,3)
nvec <- c(rep(0,2), 1)
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

max_dist <- function(risk_list, alpha, beta){
  if((risk_list['PPR'] >= alpha) & (risk_list['PCR'] >= beta)){
    distance <- sqrt((risk_list[['PPR']] - alpha)^2 + (risk_list[['PCR']] - beta)^2)
  }
  else if(risk_list['PPR'] >= alpha){
    distance <- risk_list[['PPR']] - alpha
  }
  else{
    distance <- risk_list[['PCR']] - beta
  }
}

nlistcomp_prel <- lapply(1:(length(nvec)-1), function(x) nvec[x])
clistcomp_prel <- lapply(1:(length(cvec)-1), function(x) cvec[x])
j <- iter + 2
iter <- 0
both <- TRUE
clistsys_prel <- c(0, clistsys_prel)
nlistsys_prel <- c(0, nlistsys_prel)
prices <- c(1,1,4)
prices[-length(prices)] <- prices[-length(prices)]
price_order <- order(prices)
hold_iter <- 1
sys_only_price <- prices[length(prices)] * nvec[length(nvec)]
component_prices <- 0
old_price <- sys_only_price
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
    if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
      both <- FALSE
      next
    }
    
    cvec_2 <- c(ifelse(cvec[-length(cvec)] == 0,cvec[-length(cvec)],cvec[-length(cvec)] - 1),cvec[length(cvec)])
    risk_list_p2 <- FindRisksSys(alpha_vec,beta_vec,cvec_2,nvec,p.rrl,p.arl,mcsamps, samps)
    nvec_1 <- c(ifelse(nvec[-length(nvec)] <= (cvec[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
    cvec_1 <- c(cvec[-length(cvec)] + 1,cvec[length(cvec)])
    nvec_1 <- c(ifelse(nvec_1[-length(nvec)] <= (cvec_1[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
    risk_list_p1 <- FindRisksSys(alpha_vec,beta_vec,cvec_1,nvec_1,p.rrl,p.arl,mcsamps, samps)
    
    if((max_dist(risk_list,alpha,beta) <= max_dist(risk_list_p2,alpha,beta)) & (max_dist(risk_list,alpha,beta) <= max_dist(risk_list_p1,alpha,beta))){
      nvec[-length(nvec)] <- nvec[-length(nvec)] + 1
    }
    else if(max_dist(risk_list_p1,alpha,beta) < max_dist(risk_list_p2,alpha,beta)){
      cvec <- cvec_1
      nvec <- nvec_1
    }
    else{
      cvec <- cvec_2
    }
    nlistcomp_prel <- lapply(1:(length(nvec) - 1), function(x) c(nlistcomp_prel[[x]], nvec[x]))
    clistcomp_prel <- lapply(1:(length(cvec) - 1), function(x) c(clistcomp_prel[[x]], cvec[x]))
  }
  for(i in 1:(length(nvec) - 1)){
    all2 <- TRUE
    comp_loc <- which(price_order == (length(nvec) - i))
    
    while(all2){
      
      if(nvec[comp_loc] == 0){
        all2 <- FALSE
        next
      }
      
      nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]]) - 1]
      cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]]) - 1]
      
      risk_list <- FindRisksSys(alpha_vec,beta_vec,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
      
      if(!((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta))){
        nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]])]
        cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]])]
        all2 <- FALSE
      }
      else{
        clistcomp_prel[[comp_loc]] <- clistcomp_prel[[comp_loc]][1:(length(clistcomp_prel[[comp_loc]]) - 1)]
        nlistcomp_prel[[comp_loc]] <- nlistcomp_prel[[comp_loc]][1:(length(nlistcomp_prel[[comp_loc]]) - 1)]
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
  }
  if(component_prices <= old_price){
    iter_hold <- 0
  }
  old_price <- component_prices
  if(iter_hold == 5){
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
  print(risk_list)
}
risk_list <- FindRisksSys(alpha_vec,beta_vec,hold_min_fail,hold_min_test,p.rrl,p.arl,mcsamps, samps)
print(risk_list)
out_frame <- cbind(costs, nlistsys_prel[length(nlistsys_prel):2][2:(length(costs) + 1)], clistsys_prel[length(nlistsys_prel):2][2:(length(costs) + 1)])
saveRDS(out_frame, 'series_out.RDS')
