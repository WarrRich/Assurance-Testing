### GRAND MASTER FUNCTION ###
set.seed(16)

### NOTE: Recommended going line by line through find_risk_sys 

## Run these lines if going line by line
# vecs with file.txt
nvec=c(rep(0,3), 1)  
cvec= rep(0,4)
# vecs with file_2.txt
nvec=c(rep(0,5), 1)  
cvec= rep(0,6)

## Can use file as a string or a .txt file
# file <- textConnection(
#   "BI2KE::series:cost=$1999
# Tires: cost=$33
# Brakes: cost=$45
# 
# Tires::Series
# BackTire: B(1,2), cost=$12
# FrontTire: B(1,2), cost=$12
# 
# Brakes::series
# FrontBrakes: B(3,2), cost=$12
# BackBrakes: B(1,4), cost=$Inf")

find_risk_sys <- function(file_or_file_path="file.txt",p.rrl,p.arl,alpha, beta,mcsamps, samps, nvec=c(rep(0,3),1), cvec=rep(0,4)) {
  current_dir <- getwd() ### Note: this is only working right now because there's a \n before Bike after "
  file_name <- file_or_file_path 
  # file_name <- "file.txt" # use this line if you're going line by line through the function
  # file_name <- "file_2.txt" # use this line if you're going line by line through the function
  ## Note that they need to name their file 'file.txt' if they choose to do this option
  file_path <- file.path(current_dir, file_name)
  
  if (file.exists(file_path)) {
    text <- suppressWarnings(readLines(file_path))
    # print("File read in as a file_path")
  } else {
    text <- suppressWarnings(readLines(file))
    # print("File read in as a text_connection")
  }
  
  if(length(text)==0)stop("Text file contained 0 elements") # WARNING: text file must not be empty
  
  # WARNING: Check for parallel|series pattern
  for (sentence in text) {
    if (stringr::str_count(sentence, "::") == 1) {
      if (!grepl("(?i)::(parallel|series)", sentence)) {
        cat("Error: The sentence does not match the pattern: '", sentence, "'\n")
        stop("Pattern not found in the sentence.")
      }
    }
  }
  strings <- character(length(text))
  
  # WARNING: check for non-valid characters in component names
  check_valid_characters <- function(string) {
    if (!grepl("^[[:alnum:]]+$", string)) {
      cat("Invalid characters detected in string:", string, "\n")
      stop("Invalid characters found. Only alphanumeric characters are allowed in system, subsystem, or component names.")
    }
  }
  verify_colon_strings <- function(strings) {
    for (string in strings) {
      if (grepl(":", string)) {
        parts <- unlist(strsplit(string, ":"))
        check_valid_characters(parts[1])
      }
    }
  }
  verify_colon_strings(strings)
  
  for (i in seq_along(text)) {
    strings[i] <- text[i]
  }
  
  subsys_sys <- grep("::", strings, value = TRUE) # Recognize subsystems by ::
  
  # Convert into an easier identifiable S() or P() structure - not computationally efficient, but worked with past code
  outputs <- character(length(strings))
  component_tmp <- character(length(strings))
  
  for (i in seq_along(strings)) {
    if (stringr::str_count(strings[i], "::") == 1) {
      if (grepl("(?i)::parallel", strings[i])) {
        outputs[i] <- "P("
      } else if (grepl("(?i)::series", strings[i])) {
        outputs[i] <- "S("
      }
      
      # Extract the word immediately before "::" and store it in component_tmp
      components <- strsplit(strings[i], "::")[[1]]
      component_tmp[i] <- trimws(components[length(components) - 1])
    }
    
    # Fill empty quotes with the first word of the line (without the colon)
    if (outputs[i] == "") {
      first_word <- sub(":$", "", strsplit(trimws(strings[i]), "\\s+")[[1]][1])
      outputs[i] <- paste0(outputs[i], first_word)
    }
  }
  
  
  # print(outputs) # Demonstrates section breaks, names in this order I created to match Jace's code (not the most computationally efficient)
  # print(component_tmp) # System, subsystems, with all breaks 
  component_tmp <- component_tmp[component_tmp != ""] # removes all breaks
  
  aggregated_outputs <- character(0)
  i_component <- 1  # Index for component_tmp
  for (i in seq_along(outputs)) {
    if (outputs[i] %in% c("P(", "S(")) {
      start <- i
      while (i <= length(outputs) && !grepl("NA", outputs[i]) && outputs[i] != "") {
        i <- i + 1
      }
      middle <- outputs[(start + 1):(i - 1)]
      first_word <- gsub("(^P\\()|(^S\\()", "", outputs[start])
      component <- ifelse(is.na(component_tmp[i_component]), "", component_tmp[i_component])
      aggregated_outputs <- c(aggregated_outputs, paste0(outputs[start], first_word, paste(middle, collapse = ", "), "):", component))
      i_component <- i_component + 1
    }
  }
  # Extract words
  wordExtract <- function(string) {
    middle <- stringr::str_extract(string, "\\(([^)]+)\\)")
    middleWords <- unlist(strsplit(middle, ",\\s*"))
    middleWords <- gsub("[\\(\\)]", "", middleWords)  # Remove parentheses
    endWord <- stringr::str_extract(string, ":([a-zA-Z0-9]+)$")
    endWord <- gsub(":", "", endWord)  # Remove colon
    return(c(endWord, middleWords))
  }
  
  words<- wordExtract(aggregated_outputs) # all words including duplicates
  needsBuilt<-stringr::str_sub(stringr::str_extract(aggregated_outputs, ":[a-zA-Z0-9]+"), 2, -1) # subsystem and system names only
  ready<-setdiff(words, needsBuilt) # lowest level components ONLY
  system_name <- needsBuilt[1] # first word in needsBuilt (system name should be first and specified to the user)
  all_component_names <- unique(words[-1]) # includes subsystem names 
  component_variables <- list()
  
  for (name in words) {
    name <- gsub("^\\s+|\\s+$", "", name)
    component_variables[[name]] <- name
  }
  for (name in names(component_variables)) {
    assign(name, component_variables[[name]])
  }
  
  # Recognize subsystems vs components 
  subsystem_names <- component_variables[1:length(aggregated_outputs)]
  
  # Create series function
  comps.in.series <- function(values) {
    result <- paste("(", paste(values, collapse = " * "), ")", sep = ' ')
    return(result)
  }
  
  # Create parallel function
  comps.in.parallel <- function(values) {
    result <- paste("(1 - ", paste("(1 -", compNeeded[-1], ")", collapse = " * "), paste( ")"), sep = ' ')
    return(result)
  }
  
  aggregated_outputs <- gsub("\\s+", "", aggregated_outputs) # this line currently relies on spacing in txt file. Note for user (or come back and troubleshoot)
  
  # merging_function represents the full system formula, store back into system name 
  i = 1
  merging_function <- list()
  while (i <= length(aggregated_outputs)) {
    compNeeded <- wordExtract(aggregated_outputs[i])
    if (substr(aggregated_outputs[i],1,1)%in%c("S", "s")) {
      merging_function[[i]] <- comps.in.series(compNeeded[-1])
    } else { 
      merging_function[[i]] <- comps.in.parallel(compNeeded[-1])
    }
    i <- i + 1
  }
  # merging_function 
  compNeeded<-compNeeded[-length(compNeeded)]
  names(merging_function) <- subsystem_names
  
  # Store the names of the components into their subsystems (where subsystem name is recognized as a variable)
  replace_variables <- function(expression, values) {
    for (var in names(values)) {
      # Use \\b to match whole words only
      expression <- gsub(paste0("\\b", var, "\\b"), values[[var]], expression)
    }
    return(expression)
  }
  
  for (name in names(merging_function)) {
    merging_function[[name]] <- replace_variables(merging_function[[name]], merging_function)
  }
  
  # print(merging_function) # outputs the formula to be evaluated
  
  
  #### TEST SET UP ####
  # NOTE: this will be similar to merging_function, but for tests, lowest level components won't take place of subsystems
  
  i = 1
  test_function <- list()
  while (i <= length(aggregated_outputs)) {
    compNeeded <- wordExtract(aggregated_outputs[i])
    if (substr(aggregated_outputs[i],1,1)%in%c("S", "s")) {
      test_function[[i]] <- comps.in.series(compNeeded[-1])
    } else { 
      test_function[[i]] <- comps.in.parallel(compNeeded[-1])
    }
    i <- i + 1
  }
  # test_function 
  compNeeded<-compNeeded[-length(compNeeded)] ## NOTE: what is this for??
  names(test_function) <- subsystem_names
  
  if (substr(aggregated_outputs[1], 1, 1) %in% c("S", "s")) {
    test_function[[1]] <- comps.in.series(do.call(c, test_function))
  } else {
    test_function[[1]] <- comps.in.series(do.call(c, test_function)) # verify with Dr. Warr that this is correct 
  }
  # test_function # then in recalculate_TIP it should follow this formula (with testSys * in front)
  
  
  #### EXTRACT PRIORS ####
  # Be aware that this will output in the order provided by the user
  priorsExtract <- function(string) {
    pattern <- "B\\((\\d+),(\\d+)\\)"
    matches <- stringr::str_match_all(string, pattern)
    if (length(matches) > 0) {
      extracted_numbers <- matches[[1]][, 2:3]
      return(extracted_numbers)
    } else {
      return(NULL)
    }
  }

  text1 <- paste(text, collapse = " ")
  priors <- priorsExtract(text1)
  colnames(priors) <- c("alpha", "beta")
  rownames(priors) <- ready
  # priors 
  
  priors_alpha <- as.numeric(priors[,1]) # use this in code later
  priors_beta <- as.numeric(priors[,2]) # use this in code later
 
  
  #### EXTRACT COST ####
  costExtract <- function(string) {
    pattern <- "\\$([0-9]+|Inf)"
    matches <- stringr::str_match_all(string, pattern)
    
    if (length(matches) > 0) {
      extracted_numbers <- as.numeric(matches[[1]][, 2])
      return(extracted_numbers)
    } else {
      stop("No cost values were found. Check costs formatting.")
    }
  }
  
  costs <- costExtract(text1)
  sys_only_price <- costs[1]
  prices <- costs[-1] # prices include all except for sys_only_price
  
  ## TIP
  # NOTE: test_function should take the place of tip_function, but isn't right now due to error with nvec and cvec. 
    # Ideally this is calculated in a separate function recalculate_TIP. 

  i = 1
  tip_function <- list()
  tests_prep <- list()
  all_component_names 
  test_prep <- paste0("test", 1:length(all_component_names))
  
  while (i <= length(aggregated_outputs)) {
    compNeeded <- wordExtract(aggregated_outputs[i])
    if (substr(aggregated_outputs[i],1,1)%in%c("S", "s")) {
      tip_function[[i]] <- comps.in.series(compNeeded[-1])
    } else { 
      tip_function[[i]] <- comps.in.parallel(compNeeded[-1])
    }
    i <- i + 1
  }
  names(tip_function) <- subsystem_names
  
  replace_variables <- function(expression, values) {
    for (var in names(values)) {
      # Use \\b to match whole words only
      expression <- gsub(paste0("\\b", var, "\\b"), values[[var]], expression)
    }
    return(expression)
  }
  
  for (name in names(tip_function)) {
    tip_function[[name]] <- replace_variables(tip_function[[name]], tip_function)
  }
  
  tip_function <- tip_function[1] # For this code to work, subsystem order must match. If Tires, then Brakes then you need to list them out Tires... then Brakes
  for (i in seq_along(ready)) {
    tip_function <- gsub(ready[i], test_prep[i], tip_function)
  } ### FINAL: this TIP function is wrong... 
  # tip_function
  # text <- gsub("::", "::B(0,0):", text) 
  
  ################### NOW, BEGIN CODE ###################
  #### NOTE: recalculate_TIP should take place of this when completed...
  #### NOTE: this is put in for writing sake, will need to put in code input
  made_samp <- FALSE
  samp <- NULL
  # mcsamps <- 10 # run this 
  #cvec <- rep(0,4) ### NOTE: this will change in recalculate_TIP as tests/failures are added
  #nvec <- c(rep(0,3), 1) ### NOTE: this will change in recalculate_TIP as tests/failures are added
  
  #cvec <- rep(0,length(ready)) ## NOTE: tried to make it dynamic, having issues...
  #nvec <- c(rep(0,(length(ready)-1)), 1) ## NOTE: tried to make it dynamic, having issues...
  
  if(length(samp) == 0){
    samp <- list()
    for(i in 1:length(priors_alpha)){
      samp[[i]] <- rbeta(mcsamps, priors_alpha[i], priors_beta[i])
    }
    made_samp <- TRUE
  }
  # made_samp
  
  variables <- setNames(samp, ready)
  evaluate_sys <- merging_function[1]
  # evaluate_sys <- test_function[1] # NOTE: it's not sure what to do with Tires * Brakes
  
  for (var in names(variables)) {
    evaluate_sys <- gsub(var, as.character(variables[var]), evaluate_sys)
  }
  
  sampSys <- eval(parse(text = evaluate_sys))
  #get probabilities of tests passing
  for(i in 1:(length(priors_alpha))){
    assign(paste0('test',i), pbinom(cvec[i], nvec[i], 1-samp[[i]])) ## NOTE: when I RERUN this code from Alg1, it says non-numeric argument to mathematical function? Why??
  } # Prior probabilities of component test passing 
  testSys <- pbinom(cvec[length(priors_alpha)], nvec[length(priors_beta)], 1 - sampSys)
  # Prior probabilities of the system test passing
  
  
  TIP <- testSys * eval(parse(text=tip_function)) #### NOTE: When test_function has correct nvec and cvec, this should change...
  # p.rrl=0.99 ## NOTE: these are just here for debugging last function
  # p.arl=0.99 ## NOTE: these are just here for debugging last function
  numerPostConsumRisk <- mean((sampSys<=p.rrl)*TIP)
  denomPostConsumRisk <- ifelse(mean(TIP) == 0, 1, mean(TIP))
  numerPostProducRisk <- mean((sampSys>=p.arl)*(1-TIP))  # NOTE: consistently getting 0 here... verify if correct
  denomPostProducRisk <- ifelse(denomPostConsumRisk==1,1,1-denomPostConsumRisk)
  
  if(made_samp){
    return(list(PPR=numerPostProducRisk/denomPostProducRisk,
                PCR=numerPostConsumRisk/denomPostConsumRisk,
                samps=list(samp))) #### NOTE: replace 'print' with 'return'
  }
  
  list(PPR=numerPostProducRisk/denomPostProducRisk,
       PCR=numerPostConsumRisk/denomPostConsumRisk)
  
  (list(PPR=numerPostProducRisk/denomPostProducRisk,
              PCR=numerPostConsumRisk/denomPostConsumRisk,
              samps=list(samp)))
  
  print(cvec)
  print(nvec)
  ## NOTE: Right now, I marked these as returning because we'll use them in fig 6 or algorithm 1
  ## NOTE: Eventually, we'll want everything in a big function so it will not need those values stored anywhere
  return(merging_function) 
  return(costs)
  return(prices)
  return(sys_only_price)
  return(priors)
}

find_risk_sys("file_2.txt",p.rrl=0.8,p.arl=0.5,alpha=0.5, beta=0.5, mcsamps=100, samps=NULL, cvec, nvec)

### NOTE: still having issues with length and values of nvec and cvec. When TIP_function is correct, this would be helpful...


### NOTE: we want to be able to plug in numbers and eval merging_function[1]. Here's some code that will be helpful for that:

# vector <- c(0.8,0.7,0.5,0.6, 0.4,0.5) # made up numbers for this example
# ready
# variables <- setNames(vector, ready)
# variables
# evaluate_sys <- merging_function[1]
# 
# for (var in names(variables)) {
#   evaluate_sys <- gsub(var, as.character(variables[var]), evaluate_sys)
# }
# 
# result <- eval(parse(text = evaluate_sys))
# print(result)

######################### fig 6 - need to rewrite TIP_function to be applied correctly.... ######################### 
iter <- 0
iter_c <- 0
nlistsys_prel <- 1
clistsys_prel <- 0
cvec <- rep(0,4) ## NOTE: hard coded right now 
nvec <- c(rep(0,3), 1) ## NOTE: hard coded right now
alpha <- .5 ## NOTE: hard coded right now
beta <- .5 ## NOTE: hard coded right now
samps <- NULL
both <- TRUE
p.arl <- 0.5
p.rrl <- 0.5


## when I change nvec and cvec to be ALL system and components, it runs for forever
while(both){
  
  
  
  #check PCR and PPR, save prior sample if need be
  risk_list <- find_risk_sys("file.txt",p.rrl,p.arl,mcsamps=10, samps) ##NOTE: in order for this to work, output needs to be correct for risk_list
  if(length(samps) == 0){
    samps <- risk_list$samps[[1]] ## NOTE: Is this just a starting val place? 
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


nlistcomp_prel <- lapply(1:(length(nvec)-1), function(x) nvec[x]) ## NOTE: figure out what this stands for lol
clistcomp_prel <- lapply(1:(length(cvec)-1), function(x) cvec[x]) ## NOTE: clist component preliminary? 
j <- iter + 2 ## NOTE: why is it iter (num of tests from above) + 2
iter <- 0
both <- TRUE
clistsys_prel <- c(0, clistsys_prel) ## clistsys_prel comes from above
nlistsys_prel <- c(0, nlistsys_prel)
# prices <- c(1,1,4)
# prices[-length(prices)] <- prices[-length(prices)] ## NOTE: this line is removing last value of prices - is that what I want it to do??
prices # includes all except sys_only_price
price_order <- order(prices) ## NOTE: Think about how do you want Inf to be treated in the code below - now it's the largest price
hold_iter <- 1
# sys_only_price <- prices[length(prices)] * nvec[length(nvec)] ## NOTE: I don't understand how this is sys only price
sys_only_price <- sys_only_price * nvec[length(nvec)] ## NOTE: I had to write this in - it's built in first function, make sure nvec is correct
component_prices <- 0 ## NOTE: why is component prices set to 0 here? 
old_price <- sys_only_price
hold_min <- sys_only_price
hold_min_test <- hold_min_fail <- 0
costs <- numeric() ## NOTE: is costs a new variable at this point? maybe rename this because you've already created a cost
stop_test <- FALSE
iter_hold <- 0 ## not to be confused with hold_iter <-1

#############################################################################################################
##################### Algorithm 1 once recalculate_TIP is used instead of find_risk_sys #####################
#############################################################################################################


while (!stop_test) {
  
  ## NOTE: section 1
  cvec[length(cvec)] <- clistsys_prel[j - 1]
  nvec[length(nvec)] <- nlistsys_prel[j - 1]
  j <- j - 1
  
  ## NOTE: section 2:
  both <- TRUE
  while(both){
    risk_list <- find_risk_sys(file_or_file_path="file.txt", priors_alpha,priors_alpha,cvec,nvec,p.rrl,p.arl,mcsamps, samps) # NOTE: this line may be redundant bc it'll be calculated above
    if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
      both <- FALSE
      next
    }
    
    cvec_2 <- c(ifelse(cvec[-length(cvec)] == 0,cvec[-length(cvec)],cvec[-length(cvec)] - 1),cvec[length(cvec)]) # currently outputting 4 0's, but shouldn't it have 0 0 0 -1
    risk_list_p2 <- find_risk_sys(priors_alpha,priors_beta,cvec=cvec_2,nvec=nvec,p.rrl,p.arl,mcsamps, samps) ## NOTE: does this not need to take in file_ of some kind? 
    nvec_1 <- c(ifelse(nvec[-length(nvec)] <= (cvec[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
    cvec_1 <- c(cvec[-length(cvec)] + 1,cvec[length(cvec)])
    nvec_1 <- c(ifelse(nvec_1[-length(nvec)] <= (cvec_1[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
    risk_list_p1 <- find_risk_sys(priors_alpha,priors_beta,cvec=cvec_1,nvec=nvec_1,p.rrl,p.arl,mcsamps, samps) #NOTE: do I need to reset samps every time or nah?
    
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
  ## NOTE: section 3
  for(i in 1:(length(nvec) - 1)){
    all2 <- TRUE
    comp_loc <- which(price_order == (length(nvec) - i)) ## NOTE: ask Dr. Warr how I should be thinking about Inf in price_order?
    
    while(all2){
      
      if(nvec[comp_loc] == 0){ ## NOTE: this line doesn't work because nvec is length 4??
        all2 <- FALSE
        next
      }
      
      nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]]) - 1] ## NOTE: Error in nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]]) -  : 
      ## replacement has length zero
      cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]]) - 1]
      
      risk_list <- find_risk_sys(priors_alpha,priors_beta,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
      
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
