#### Build figures for Question 2
#### Author: Katherine Lauck
#### Last updated: 27 April 2021
## Needed: method for computing upper and lower confidence intervals

## Dependencies
library(tidyverse)
library(boot) # Needed for inv.logit function

library(lme4)
library(RCurl)
library(png)
library(jpeg)
library(cowplot)
library(magick)
library(emmeans)
library(viridis)

## Load baseline model results
m <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # fill in Danny's conservation model

## Load data
data <- read_rds("Data/active/success-cleaned.rds")

temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

con <- c(scale(c(4,8,12,15),center = mean(data$ConservationScore), scale = sd(data$ConservationScore))) # Build dummy conservation score

grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),ConservationScore.scaled = con, pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1) %>%
  mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet) # build grid with predictors

con_predict <- function(model, grid) {
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, con = grid$ConservationScore.scaled*sd(data$ConservationScore) + mean(data$ConservationScore), predicted = inv.logit(predicted),lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1))))
}

grid.quant=expand.grid(Tmax_std_gridmet = quantile(temp,probs = c(.025,.5,.975),na.rm = TRUE),
                       NewLU1= c("Forest","Ag","Natural_open","Human"),
                       ConservationScore.scaled = con, 
                       pcpbefore_raw_gridmet = 0,
                       NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, 
                       substrate_binary=1,
                       laydate_scaled=0,
                       at_least_one_success=1) %>%
  mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet)
# grid.quant=cbind(grid.quant[,1],grid.quant[,1]^2,grid.quant[,2:9])
# colnames(grid.quant)[1]<-"Tmax_std_gridmet"
# 
# colnames(grid.quant)[2]="Tmax_std_gridmet_sq"

quant <- con_predict(m,grid.quant)


# Figure out in which land uses there is a significant effect of conscore - these results are not yet produced?

ag <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Ag.rds")
summary(ag)
forest <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Forest.rds")
summary(forest)
open <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Natural_open.rds")
summary(open)
human <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Human.rds")
summary(human)

f_labels <- data.frame(lu = factor(c("Forest", "Ag", "Natural_open", "Human")), label = c("+", "*", "+","")) ## significance labels, but need the individual land use models to compare all levels - emmeans can't handle interactions?


fig_data <- con_predict(m,grid)

lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")

p2 <- ggplot(data = fig_data, aes(x = temp, y = predicted)) +
  geom_line(aes(color = factor(con)), size = 1) + # overlay line
  facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  xlab("Maximum temperature anomaly") +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option="viridis"
                      ) +
  # geom_text(x = -2, y = .4, aes(label = label), data = f_labels) +
  theme(legend.position = "bottom"
        #,legend.key.size = unit(1, "cm")
        #,legend.key.width = unit(1,"cm")
        #,legend.spacing.x = unit(2,"cm")
        #,legend.spacing.y = unit(2,"cm")
        ,legend.text = element_text(margin = margin(t = 15,r = 45,b = 15, unit = "pt"))
        #,legend.box.just = "left"
        #,legend.justification = c("left", "bottom")
        ) +
  guides(color=guide_legend(ncol=2)) +
  labs(color = "Conservation score") +
  geom_text(x = -3, y = .45, aes(label = label), data = f_labels)

  # annotation_raster(hosp,xmin=nrow(fig_data)-1,
  #                   xmax = nrow(fig_data),
  #                   ymin = max(fig_data$upper)-(.05*max(fig_data$upper)),
  #                   ymax = max(fig_data$upper), interpolate = T)

# hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
#   geom_histogram() + 
#   xlab("Temperature anomaly") +
#   theme_classic() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

# plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v", common.legend = TRUE)

ggsave("figures/q2-facet.png",p2, width = 4, height = 7.5)

## BBS figure

# ## Load baseline model results
# m <- read_rds("results/q4/success~BBSstdmaxlaydate3way.LRT.AK.RDS") # fill in Alison's conservation model
# 
# ## Load data
# data <- read_rds("Data/active/success-cleaned.rds")
# 
# temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
# temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range
# high.mean.low <- function(x) {
#   c(mean(x,na.rm = TRUE)+sd(x,na.rm = TRUE),mean(x,na.rm = TRUE),mean(x,na.rm = TRUE)-sd(x,na.rm = TRUE))
# }
# 
# bbs <- c(scale(high.mean.low(data$Trend),center = mean(data$Trend, na.rm = TRUE), scale = sd(data$Trend,na.rm = TRUE))) # Build dummy conservation score
# 
# grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),Trend.scaled = bbs, pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1) %>%
#   mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet) # build grid with predictors
# 
# bbs_predict <- function(model, grid) {
#   mm=model.matrix(terms(model), grid)
#   predicted = mm %*% fixef(model) 
#   pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
#   return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, bbs = grid$Trend.scaled*sd(data$Trend,na.rm = TRUE) + mean(data$Trend,na.rm = TRUE), predicted = inv.logit(predicted),lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1))))
# }
# 
# # Figure out in which land uses there is a significant effect of bbs - these results are not yet produced?
# 
# ag <- read_rds("results/q4/bbs_ag.rds")
# summary(ag)
# forest <- read_rds("results/q4/bbs_forest.rds")
# summary(forest)
# open <- read_rds("results/q4/bbs_open.rds")
# summary(open)
# human <- read_rds("results/q4/bbs_human.rds")
# summary(human)
# 
# f_labels <- data.frame(lu = c("Forest", "Ag", "Natural_open", "Human"), label = c("*", "", "**","***")) ## significance labels, but need the individual land use models to compare all levels - emmeans can't handle interactions?
# 
# 
# fig_data <- bbs_predict(m,grid)
# 
# lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
# names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")
# 
# bbs <- ggplot(data = fig_data, aes(x = temp, y = predicted)) +
#   geom_line(aes(color = factor(bbs)), size = 1) + # overlay line
#   facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use
#   ylab("Proportion of nests with at least one offspring surviving to fledging") +
#   xlab("Maximum temperature anomaly") +
#   theme_classic() +
#   scale_color_viridis(discrete=TRUE, option="turbo",labels = c("Decline","Stable","Increase"),direction=-1) +
#   geom_text(x = 0, y = .6, aes(label = label), data = f_labels) +
#   theme(legend.position = "bottom"
#   #       #,legend.key.size = unit(1, "cm")
#   #       #,legend.key.width = unit(1,"cm")
#   #       #,legend.spacing.x = unit(2,"cm")
#   #       #,legend.spacing.y = unit(2,"cm")
#   #       ,legend.text = element_text(margin = margin(t = 15,r = 45,b = 15, unit = "pt"))
#          #,legend.box.just = "left"
#          #,legend.justification = c("left", "bottom")
#   ) +
#   # guides(color=guide_legend(ncol=2)) +
#   labs(color = "Population trend")
# # annotation_raster(hosp,xmin=nrow(fig_data)-1,
# #                   xmax = nrow(fig_data),
# #                   ymin = max(fig_data$upper)-(.05*max(fig_data$upper)),
# #                   ymax = max(fig_data$upper), interpolate = T)
# 
# # hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
# #   geom_histogram() + 
# #   xlab("Temperature anomaly") +
# #   theme_classic() +
# #   theme(axis.title.y=element_blank(),
# #         axis.text.y=element_blank(),
# #         axis.ticks.y=element_blank())
# 
# # plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v", common.legend = TRUE)
# 
# ggsave("figures/q2-bbs.png",bbs, width = 4, height = 7.5)

