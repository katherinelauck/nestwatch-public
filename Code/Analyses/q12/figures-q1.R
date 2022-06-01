#### Build figures for Question 1
#### Author: Katherine Lauck
#### Last updated: 27 April 2021
## Needed: method for computing upper and lower confidence intervals


## Dependencies
library(tidyverse)
library(boot) # Needed for inv.logit function
library(ggpubr)
library(lme4)
library(scales)
library(forcats)

## Load baseline model results
m <- read_rds("results/q12/success~stdmax2laydate2way.AK.RDS")
m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS")

## Load data
data <- read_rds("Data/active/success-cleaned.rds")

temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

grid=expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1)

grid=cbind(grid[,1],grid[,1]^2,grid[,2:9])
colnames(grid)[1]<-"Tmax_std_gridmet"

colnames(grid)[2]="Tmax_std_gridmet_sq"

temp_predict <- function(model,grid) {
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, predicted = inv.logit(predicted), lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1)))) # bind into dataframe & return
}

fig_data <- temp_predict(m,grid) %>% mutate(facet = factor(lu,c("Forest","Ag","Natural_open","Human"),ordered = TRUE))

grid.quant=expand.grid(Tmax_std_gridmet = quantile(fig_data$temp,probs = c(.025,.5,.975),na.rm = TRUE),
                       NewLU1= c("Forest","Ag","Natural_open","Human"),
                       pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, 
                       substrate_binary=1,laydate_scaled=0,at_least_one_success=1)
grid.quant=cbind(grid.quant[,1],grid.quant[,1]^2,grid.quant[,2:9])
colnames(grid.quant)[1]<-"Tmax_std_gridmet"

colnames(grid.quant)[2]="Tmax_std_gridmet_sq"

quant <- temp_predict(m,grid.quant)

lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")
#show_col(hue_pal()(4))

### which interactions are significant?

ag <- read_rds("results/q12/success~stdmax2_ag.rds")
summary(ag)
forest <- read_rds("results/q12/success~stdmax2_forest.rds")
summary(forest)
human <- read_rds("results/q12/success~stdmax2_human.rds")
summary(human)
open <- read_rds("results/q12/success~stdmax2_open.rds")
summary(open)

f_labels <- data.frame(lu = factor(c("Forest", "Ag", "Natural_open", "Human")), label = c("**", "*", "","*")) ## significance labels for each facet

p <- ggplot(data = fig_data, aes(temp, predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = lu, fill = lu), linetype = 2, alpha = .2) + # plot confidence intervals
  geom_line(aes(color = lu), size = 1) + # overlay line
  facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use 
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  # xlab("Mean maximum temperature over nesting period (z-scaled)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
  scale_fill_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
  geom_text(x = -2, y = .25, aes(label = label), data = f_labels)

hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
  geom_histogram() + 
  xlab("Maximum temperature anomaly") +
  ylab("Frequency") +
  theme_classic()

plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v")

ggsave("figures/F1_q1-facet.png",plot, width = 3, height = 7.5)

# p <- ggplot(data = fig_data, aes(temp, predicted)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, color = lu, fill = lu), linetype = 2, alpha = .2) + # plot confidence intervals
#   geom_line(aes(color = lu), size = 1) + # overlay line
#   facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use 
#   ylab("Proportion of nests with at least one offspring surviving to fledging") +
#   xlab("Maximum temperature anomaly") +
#   theme_classic() +
#   theme(legend.position = "none") +
#   # theme(axis.title.x=element_blank(),
#   #       axis.text.x=element_blank(),
#   #       axis.ticks.x=element_blank()) +
#   scale_color_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
#   scale_fill_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF"))
# 
# prez.p <- p + facet_wrap(~lu, nrow = 2, ncol = 2, labeller = labeller(lu=lu.labs))
# ggsave("figures/q1-prez.plot.png",prez.p,width = 6, height = 6)
# 
# fig_data <- filter(fig_data,lu %in% c("Forest","Ag"))
# p <- ggplot(data = fig_data, aes(temp, predicted)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, color = lu, fill = lu), linetype = 2, alpha = .2) + # plot confidence intervals
#   geom_line(aes(color = lu), size = 1) + # overlay line
#   facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use 
#   ylab("Proportion of nests with at least\none offspring surviving to fledging") +
#   xlab("Maximum temperature anomaly") +
#   theme_classic() +
#   theme(legend.position = "none") +
#   # theme(axis.title.x=element_blank(),
#   #       axis.text.x=element_blank(),
#   #       axis.ticks.x=element_blank()) +
#   scale_color_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
#   scale_fill_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF"))
# 
# plot <- ggarrange(p, hist, heights = c(2,1), ncol = 1, nrow = 2, align = "v")
# ggsave("figures/q1-forest+ag.png", plot, width = 3, height = 7.5)
# qe.p <- p + facet_wrap(~lu, nrow = 1, ncol = 2, labeller = labeller(lu = lu.labs))
# ggsave("figures/q1-qe.png", qe.p, width = 6, height = 4)

### figure 1 but for min

## Load baseline model results
m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS")

## Load data
data <- read_rds("Data/active/success-cleaned.rds")

temp <- range(data$Tmin_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

grid=expand.grid(Tmin_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1)

grid=cbind(grid[,1],grid[,1]^2,grid[,2:9])
colnames(grid)[1]<-"Tmin_std_gridmet"

colnames(grid)[2]="Tmin_std_gridmet_sq"

temp_predict <- function(model,grid) {
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmin_std_gridmet, predicted = inv.logit(predicted), lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1)))) # bind into dataframe & return
}

fig_data <- temp_predict(m.min,grid)

## which land uses are significant?

ag <- read_rds("results/q12/success~stdmin2_ag.rds")
summary(ag)
forest <- read_rds("results/q12/success~stdmin2_forest.rds")
summary(forest)
human <- read_rds("results/q12/success~stdmin2_human.rds")
summary(human)
open <- read_rds("results/q12/success~stdmin2_open.rds")
summary(open)

f_labels <- data.frame(lu = factor(c("Forest", "Ag", "Natural_open", "Human")), label = c("*", "", "**","***")) ## significance labels for each facet

lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")
#show_col(hue_pal()(4))

p <- ggplot(data = fig_data, aes(temp, predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = lu, fill = lu), linetype = 2, alpha = .2) + # plot confidence intervals
  geom_line(aes(color = lu), size = 1) + # overlay line
  facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use 
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
  scale_fill_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) +
  geom_text(x = -3, y = .45, aes(label = label), data = f_labels)

hist <- ggplot(data = data, aes(x = Tmin_std_gridmet)) +
  geom_histogram() + 
  xlab("Temperature anomaly (min)") +
  ylab("Frequency") +
  theme_classic()

plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v")

ggsave("figures/FS1_q1-min.png",plot, width = 3, height = 7.5)
