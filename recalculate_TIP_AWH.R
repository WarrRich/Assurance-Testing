
############################ Built TIP functions ############################
## TIP = x>= n-c for every component and subsystem and system 
## every time perform recalculate_tip, there are new n's and c's SOOOo binomial parts (TIP|priors) should be recalculated :) 
## This function will be utilized for algorithm 1 

## PARALLEL: calculate overall TIP and risks
# TIP <- testSys * 
#   (1 - (1 - test1) *
#      (1 - test2))

## SERIES: 
# TIP <- testSys * 
#   test1 *
#   test2

## COMBINED SERIES AND PARALLEL: 
# TIP <- testSys * testSS1 * testSS2
#   *(1 - (1 - test1) *
#      (1 - test2)) * test3 * test4 
# 


######### NOTE: Run piece by piece the find_risk_sys function in order to have these values below #########

all_component_names # has all ss1 ss2 and k lower level components 
test_function
test_function1 <- test_function[[1]]

recalculate_TIP <- function(priors_alpha, priors_beta, cvec, nvec, samp, sampSys, p.rrl, p.arl){
  #get probabilities of tests passing
  tests_vec <- list()
  for(i in 1:(length(priors_alpha))){
    tests_vec[i] <- assign(all_component_names[i], pbinom(cvec[i], nvec[i], 1-samp[[i]])) ## NOTE: check samp[[i]] length
  } # Prior probabilities of component test passing 
  # testSys <- pbinom(cvec[length(alpha)], nvec[length(beta)], 1 - sampSys) # Jace's code was like this, with 0's for the system priors. You'd think that wouldn't work, but R compensates
  testSys <- pbinom(cvec[length(cvec)], nvec[length(nvec)], 1 - sampSys) # last value of cvec is used
  # Prior probabilities of the system test passing
  
  #calculate overall TIP and risks
  # TIP <- testSys * testSS1 * testSS2
  #   *(1 - (1 - test1) *
  #      (1 - test2)) * test3 * test4 
  
  TIP <- testSys * eval(parse(text=test_function1))
  
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

