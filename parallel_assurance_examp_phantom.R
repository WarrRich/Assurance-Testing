# Defintion of function that takes as inputs the following variables and returns the posterior producers risk (PPR) and the posterior consumers risk (PCR) respectively
#      the objective is to find a test that satisfies all three of the following conditions:
#  1 - PPR is less than or equal to some specified value alpha 
#  2 - PCR is less than or equal to some specified value beta 
#  3 - The test has a smaller "cost" than the other tests that satisfy the two previous conditions
#       usually the cost is measured in number of system tests (or equivilant)
#
# Function inputs
# a1 and b1 are the beta(a1,b1) prior parameters for component 1
# a2 and b2 are the beta(a2,b2) prior parameters for component 2
# c1 and c2 are the number of acceptable test failures for component 1 and 2 respectively
# n1 and n2 are the number of assurance tests conducted for component 1 and 2 respectively
# csys and nsys are the number of acceptable test failures and assurance tests for the system respectively
# p.arl and p.rrl are the acceptable reliability level and the rejectable reliability level of the system respectively
# mcsamps is the number of Monte Carlo samples to approximate the posterior producers risk (PPR) and the posterior consumers risk (PCR) 
#       (higher values of mcsamps typically gives better accuracy)
#
# Function defintion for [TWO COMPONENTS IN SERIES]
FindRisksSys <- function(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps) {
  samp1 <- rbeta(mcsamps,a1,b1)  
  samp2 <- rbeta(mcsamps,a2,b2)
  sampp <- rbeta(mcsamps, a3, b3)
  sampSys <- (1 - (1 - samp1)*(1 - samp2)) * sampp
  TIP <- pbinom(csys,nsys,1-sampSys)*(1-(1-pbinom(c1,n1,1-samp1))*(1-pbinom(c2,n2,1-samp2)))
  numerPostConsumRisk <- mean((sampSys<=p.rrl)*TIP)
  denomPostConsumRisk <- mean(TIP)
  numerPostProducRisk <- mean((sampSys>=p.arl)*(1-TIP))  
  denomPostProducRisk <- ifelse(denomPostConsumRisk==1,1,1-denomPostConsumRisk)
  list(PPR=numerPostProducRisk/denomPostProducRisk,
       PCR=numerPostConsumRisk/denomPostConsumRisk)
}


################################
# An illustration with alpha = 0.05, beta = 0.05, and 
a1 <- 8; b1 <- 4; a2 <- 8; b2 <- 4; a3 <- 11; b3 <- 1
p.arl <- 0.95; p.rrl <- 0.9 
mcsamps <- 1000000 

# Start with 
c1 <- 0; c2 <- 0; n1 <- 1; n2 <- 1; csys <- 0; nsys <- 0 
FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)

# skipping some steps .........

c1 <- 0; c2 <- 0; n1 <- 64; n2 <- 64; csys <- 0; nsys <- 0
FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)
# thus conducting 64 component tests (for each component) will produce a test that meets the criteria


# in this case this is also approximately equliviant to 
c1 <- c2 <- 15; n1 <- n2 <- 21; csys <- 3; nsys <- 4
FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)


both <- TRUE
c1 <- c2 <- n1 <- n2 <- csys <- 0
nsys <- 1
alpha <- .05
beta <- .05
iter <- 0
nlistsys_prel <- numeric()
clistsys_prel <- numeric()
nlistsys_prel[1] <- nsys
clistsys_prel[1] <- csys
while(both){
  risk_list <- FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)
  if(risk_list['PPR'] > alpha){
    if(csys < (nsys - 1)){
      csys <- csys + 1
    }
    else{
      nsys <- nsys + 1
      csys <- csys + 1
    }
  }
  else if(risk_list['PCR'] > beta){
    nsys <- nsys + 1
  }
  
  if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
    both <- FALSE
    print(paste0('nsys is ', nsys))
    print(paste0('csys is ', csys))
    print(risk_list)
  }
  iter <- iter + 1
  print(iter)
  clistsys_prel[iter + 1] <- csys
  nlistsys_prel[iter + 1] <- nsys
}

both <- TRUE
c1 <- c2 <- nsys <- csys <- 0
n1 <- n2 <- 1
alpha <- .05
beta <- .05
iter <- 0
while(both){
  risk_list <- FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)
  if(risk_list['PPR'] > alpha){
    if(c1 < (n1 - 1)){
      c1 <- c2 <- c1 + 1
    }
    else{
      n1 <- n2 <- n1 + 1
      c1 <- c2 <- c1 + 1
    }
  }
  else if(risk_list['PCR'] > beta){
    n1 <- n2 <- n1 + 1
  }
  
  if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
    both <- FALSE
    print(paste0('n compnents is ', n1))
    print(paste0('c components is ', c1))
    print(risk_list)
  }
  iter <- iter + 1
  print(iter)
}

################################
# With some experientation in this example, it appears we can find a test for any values of alpha and beta 
#      (since we can get very small values of PPR and PCR simultaneously)


nsys <- 52
csys <- 0
c1 <- c2 <- n1 <- n2 <- 0
clist <- numeric()
nlist <- numeric()
nlistsys <- numeric()
clistsys <- numeric()
iter <- 0
alpha <- beta <- .05
while (nsys > 0) {
  risk_list <- FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)
  if(clistsys_prel[length(clistsys_prel) - iter] == clistsys_prel[length(clistsys_prel) - iter - 1]){
    nsys <- nsys - 1
    if ((csys == nsys) & (csys > 0)){
      csys <- csys - 1
    }
  }
  else {
    if(csys > 0){
      csys <- csys - 1
    }
    else{
      nsys <- nsys - 1
    }
  }
  both <- TRUE
  hold_iter <- 0
  while(both){
    risk_list <- FindRisksSys(a1,b1,a2,b2,a3,b3,c1,c2,n1,n2,csys,nsys,p.rrl,p.arl,mcsamps)
    if((iter == 0) & (hold_iter == 0)){
      n1 <- n2 <- 1
      hold_iter <- 1
    }
    else if(risk_list['PPR'] > alpha){
      if(c1 < (n1 - 1)){
        c1 <- c2 <- c1 + 1
      }
      else{
        n1 <- n2 <- n1 + 1
        c1 <- c2 <- c1 + 1
      }
    }
    else if(risk_list['PCR'] > beta){
      n1 <- n2 <- n1 + 1
    }
    
    if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
      both <- FALSE
      print(paste0('n components is ', n1))
      print(paste0('c components is ', c1))
      print(risk_list)
    }
  }
  clist <- c(clist, c1)
  nlist <- c(nlist, n1)
  nlistsys <- c(nlistsys, nsys)
  clistsys <- c(clistsys, csys)
  iter <- iter + 1
  print(iter)
}

plot(nlistsys, nlist
     , xlab = "number of system tests"
     , ylab = 'number of component tests'
     , main = 'System vs component tests for parallel with phantom component')
saveRDS(cbind(nlistsys,nlist), 'parallel_phant_out.RDS')
