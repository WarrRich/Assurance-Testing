library(ggplot2)
library(tidyverse)
library(extrafont)
loadfonts(device = "win")
setwd("C:/Users/ritch/Downloads/Assurance_Research")
files <- c('parallel', 'series', 'parallel_phant', 'series_phant')
num_sys <- c(65, 111, 52, 121)
dat_frames <- list()
for(i in 1:4){
  dat_frames[[i]] <- as.data.frame(readRDS(paste0(files[i],'_out.RDS')))
  if(sum(is.na(dat_frames[[i]]))) dat_frames[[i]][nrow(dat_frames[[i]]), c(2,3)] <- c(1,0)
  dat_frames[[i]]$System <- as.factor(ifelse(i %% 2 != 0, 'Parallel', 'Series'))
  dat_frames[[i]]$Phantom <- as.factor(ifelse(i > 2, 'Yes', 'No'))
  colnames(dat_frames[[i]]) <- c('cost', 'nsys', 'csys', 'System', 'Phantom')
  dat_frames[[i]]$ncomp <- dat_frames[[i]]$cost - 4 * dat_frames[[i]]$nsys
}

graph_frame <- bind_rows(dat_frames)



ggplot(data = graph_frame ) +
  geom_point(mapping = aes(x = nsys, y = ncomp, color = System, shape = Phantom), size = 3) +
  xlab('Number of system tests') + 
  ylab('Total number of component tests') + 
  theme_bw() +
  theme(text=element_text(size=25,  family="serif")) +
  scale_color_grey() 

costs_balanced <- readRDS('costs.RDS')
nsys_balanced <- 157:5
costs_unbalanced <- readRDS('costs2.RDS')
nsys_unbalanced <- 126:31

cost_frame <- data.frame(rbind(cbind(costs_balanced, nsys_balanced,rep(0, length(costs_balanced))), cbind(costs_unbalanced, nsys_unbalanced, rep(1, length(costs_unbalanced)))))
colnames(cost_frame) <- c('cost', 'nsys', 'balanced')


ggplot(data = cost_frame) +
  geom_line(mapping = aes(x = nsys, y = cost, color = as.factor(balanced)), size = 2) +
  xlab('Number of system tests') + 
  ylab('Cost in dollars') + 
  theme_bw() +
  theme(text=element_text(size=18,  family="serif")) +
  scale_color_grey(name = 'PPR Considered', labels = c('Yes', 'No'))# +
  # scale_color_discrete(name = 'PPR Considered', labels = c('Yes', 'No'))



