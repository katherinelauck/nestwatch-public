#### Build figures for Question 3
#### Author: Katherine Lauck
#### Last updated: 17 May 2021

## Dependencies
library(tidyverse)
library(boot) # Needed for inv.logit function
library(viridis)
library(ggpubr)
library(lme4)
library(magick)
library(grid)
library(gridExtra)
library(cowplot)
library(ggimage)

######### Cavity

## Load baseline model results
m <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Full.rds")

## Load data
data <- read_rds("Data/active/success-cleaned.rds")

## Determine which land uses have significant effect of cavity

open <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Natural_open.rds")
human <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Human.rds")
forest <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Forest.rds")
ag <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Ag.rds")

summary(open)
summary(human)
summary(forest)
summary(ag)

### Only significant in Ag and Developed

temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

cavity <- c(1,0)

grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),cavity_binary = cavity, pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1) %>%
  mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet) # build grid with predictors

cavity_predict <- function(model, grid) {
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, cavity = factor(grid$cavity_binary), predicted = inv.logit(predicted),lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1))))
}

fig_data <- cavity_predict(m,grid)

fig_data <- fig_data %>%
  mutate(lu = factor(lu,levels = c("Forest","Ag","Natural_open","Human")))

f_labels <- data.frame(lu = factor(c("Forest", "Ag", "Natural_open", "Human")), label = c("", "**", "","**")) ## significance labels for each facet

lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")

cavity <- ggplot(data = fig_data, aes(x = temp, y = predicted)) +
  geom_line(aes(color = cavity), size = 1) + # overlay line
  geom_ribbon(aes(ymin = lower, ymax = upper, color = cavity, fill = cavity), linetype = 2, alpha = .2) + # plot confidence intervals
  facet_wrap(~factor(lu), nrow = 4, ncol = 1, labeller = labeller(`factor(lu)`=lu.labs)) + # facet by land use
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  xlab("Maximum temperature anomaly") +
  theme_classic() +
  theme(legend.position = "bottom"
        ,legend.text = element_text(margin = margin(r = 60, unit = "pt"))
        ) + 
  guides(color=guide_legend(title="Nest type"), fill=guide_legend(title="Nest type")) +
  # theme(axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank()) +
  geom_text(x = -2, y = .25, aes(label = label), data = f_labels)

# hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
#   geom_histogram() + 
#   xlab("Temperature anomaly") +
#   theme_classic() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

# plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v", common.legend = TRUE)

ggsave("figures/q3-cavity.png",cavity, width = 4, height = 7.5)

######### Substrate

## Load baseline model results
m <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Full.rds")

## Load data
data <- read_rds("Data/active/success-cleaned.rds")

## Determine which land uses have significant effect of substrate

open <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Natural_open.rds")
human <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Human.rds")
forest <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Forest.rds")
ag <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Ag.rds")

summary(open)
summary(human)
summary(forest)
summary(ag)

### Significant in Forest and Ag

temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

substrate <- c(1,0)

grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"),substrate_binary = substrate, pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0,laydate_scaled=0,at_least_one_success=1) %>%
  mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet) # build grid with predictors

substrate_predict <- function(model, grid) {
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, substrate = factor(grid$substrate_binary), predicted = inv.logit(predicted),lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1))))
}

fig_data <- substrate_predict(m,grid)
f_labels <- data.frame(lu = factor(c("Forest", "Ag", "Natural_open", "Human")), label = c("", "**", "","**")) ## significance labels for each facet

lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")

substrate <- ggplot(data = fig_data, aes(x = temp, y = predicted)) +
  geom_line(aes(color = substrate), size = 1) + # overlay line
  geom_ribbon(aes(ymin = lower, ymax = upper, color = substrate, fill = substrate), linetype = 2, alpha = .2) + # plot confidence intervals
  facet_wrap(~factor(lu), nrow = 4, ncol = 1, labeller = labeller(`factor(lu)`=lu.labs)) + # facet by land use
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  xlab("Maximum temperature anomaly") +
  theme_classic() +
  theme(legend.position = "bottom") + 
  guides(color=guide_legend(title="Nest in nestbox"), fill=guide_legend(title="Nest in nestbox")) +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  geom_text(x = -2, y = .25, aes(label = label), data = f_labels) + 
  scale_fill_viridis(direction = -1, option = "turbo", discrete = TRUE) +
  scale_color_viridis(direction = -1, option = "turbo", discrete = TRUE)

# hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
#   geom_histogram() + 
#   xlab("Temperature anomaly") +
#   theme_classic() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

# plot <- ggarrange(p, hist, heights = c(4,1), ncol = 1, nrow = 2, align = "v", common.legend = TRUE)

ggsave("figures/FS3_q3-substrate.png",substrate, width = 4, height = 7.5)

# substrate_bbs <- ggarrange(bbs,substrate + theme(axis.title.y=element_blank(),
#                                          axis.text.y=element_blank(),
#                                          axis.ticks.y=element_blank()), ncol = 2, nrow = 1, align = "hv")
# 
# ggsave("figures/FS3_bbs+substrate.png",substrate_bbs, width = 8, height = 7.5)

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

library(metR)

p2 <- ggplot(data = fig_data, aes(x = temp, y = predicted)) +
  geom_line(aes(color = factor(con)), size = 1) + # overlay line
  facet_wrap(~lu, nrow = 4, ncol = 1, labeller = labeller(lu=lu.labs)) + # facet by land use
  ylab("Proportion of nests with at least one offspring surviving to fledging") +
  xlab("Maximum temperature anomaly") +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option="viridis",labels = c("","","","")
  ) +
  # geom_text(x = -2, y = .4, aes(label = label), data = f_labels) +
  theme(legend.position = "bottom"
        #,legend.key.size = unit(1, "cm")
        #,legend.key.width = unit(1,"cm")
        #,legend.spacing.x = unit(2,"cm")
        #,legend.spacing.y = unit(2,"cm")
        ,legend.text = element_text(margin = margin(r = 35, unit = "pt"))
        #,legend.box.just = "left"
        #,legend.justification = c("left", "bottom")
  ) +
  guides(color=guide_legend(ncol=4,title.position = "top")) +
  theme(legend.margin = margin(t = 0,b = 45)) +
  labs(color = "Increasing conservation concern") +
  geom_text(x = -3, y = .45, aes(label = label), data = f_labels) +
  theme(legend.background = element_rect(fill = "transparent",color = "transparent"),
        legend.box.background = element_rect(fill = "transparent",color = "transparent"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.spacing.y = unit(.004,"cm"))

legend <- ggdraw() + draw_plot(get_legend(p2))
ggsave("figures/con_legend.png",legend,width = 4, height = 7.5/5, bg = "transparent")

p2_nolegend <- p2 + labs(color = "") + guides(color = guide_legend(override.aes = list(alpha = 0)))






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

# ggsave("figures/q2-facet.png",p2, width = 4, height = 7.5)

pub_plot <- ggarrange(p2_nolegend, cavity + theme(axis.title.y=element_blank(),
                                         axis.text.y=element_blank(),
                                         axis.ticks.y=element_blank()), ncol = 2, nrow = 1, align = "hv")

arrow_black <- 
  ggplot() +
  geom_arrow(aes(x = 0, y = 0, dx = 8.2,dy = 0),size = 6,arrow.length = 2.5, lineend = "butt",color = "black",arrow.type = "open",arrow.angle = 30,alpha = .75) +
  theme_void() + 
  theme(legend.position = "none")

arrow_white <- 
  ggplot() +
  geom_arrow(aes(x = .3, y = 0, dx = 8.15,dy = 0),size = 4,arrow.length = 2.4, color = "white",arrow.type = "open",arrow.angle = 30) +
  theme_void() + 
  theme(legend.position = "none")


p3 <- ggdraw() +
  draw_plot(pub_plot) +
  draw_plot(arrow_black,x = -.23,y = -.402) +
  draw_plot(arrow_white,x = -.228,y = -.402) +
  draw_plot(legend,x = -.2,y = -.435)

ggsave("figures/conscore+cavity.png",p3, width = 8, height = 7.5)

fig <- image_read("figures/conscore+cavity.png")



hosp <- image_read("figures/HOSP.png") %>%
  image_scale("190")
basw <- image_read("figures/BASW.png") %>%
  image_scale("260")
cbbh <- image_read("figures/CBBH.png") %>%
  image_scale("260")
oati <- image_read("figures/OATI.png") %>%
  image_scale("210")
nest <- image_read("figures/NOMO_Nest.jpg") %>%
  image_scale("280") %>% 
  image_annotate("Cup",size = 40, gravity = "southwest", color = "black", boxcolor = rgb(1,1,1,.7))
cav <- image_read("figures/TRES_Nest.jpg") %>%
  image_scale("268") %>% 
  image_annotate("Cavity",size = 40, gravity = "southwest", color = "black", boxcolor = rgb(1,1,1,.7))


image_composite(fig,hosp,offset = "+105-2035") %>% 
  image_composite(basw,offset = "+300-2052") %>%
  image_composite(cbbh,offset = "+600-2045") %>%
  image_composite(oati,offset = "+900-2025") %>%
  image_composite(nest,offset = "+1685-2000") %>%
  image_composite(cav,offset = "+2075-2000") %>%
  image_write("figures/F2_conscore+cavity.png",
              flatten = TRUE)

