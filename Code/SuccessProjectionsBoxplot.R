rm(list=ls())

library(tidyverse)
library(cowplot)
library(gt)

df <- read.csv("Data/active/success_projections_computed.csv")

colnames(df)
table(df$NewLU1)

df2 <- df %>%
  select(contains('y.dif.average.mean'), NewLU1) %>%
  #pivot_longer(everything(),names_to='scenario', values_to='dif.success') %>%
  pivot_longer(-NewLU1,names_to='scenario', values_to='dif.success') %>%
  mutate(scenario = as.factor(scenario)) %>%
  as.data.frame()

levels(df2$scenario)
df2$scenario <- factor(df2$scenario, levels=c("y.dif.average.mean.rcp.45m", "y.dif.average.mean.rcp.45e",
                                               "y.dif.average.mean.rcp.85m", "y.dif.average.mean.rcp.85e"))
levels(df2$scenario)
labs.scenario <- c("RCP 4.5\n2050","RCP 4.5\n2100","RCP 8.5\n2050","RCP 8.5\n2100")
names(labs.scenario) <- c("y.dif.average.mean.rcp.45m", "y.dif.average.mean.rcp.45e",
                          "y.dif.average.mean.rcp.85m", "y.dif.average.mean.rcp.85e")
  
labs.LU <- c("Agriculture", "Forest", "Developed", 'Natural open')
names(labs.LU) <- c("Ag", "Forest", "Human", 'Natural_open')

p1 <- ggplot(df2, aes(x=scenario, y=dif.success)) +
  geom_violin(color='grey', fill='grey')+
  #geom_violin(color='grey', aes(fill=as.factor(scenario)))+
  geom_violin(aes(fill=as.factor(scenario), color=as.factor(scenario)))+
  geom_boxplot(width=.1, color='gray20', fill='gray20',
               outlier.shape = NA) + #remove outliers
  #geom_boxplot(color='gray20', outlier.alpha = .2) + 
  geom_hline(yintercept=0, linetype="dashed", color = "blue") +
  stat_summary(fun=median, geom="point", size=.8, color="white")+
  scale_x_discrete(labels=labs.scenario) +
  # scale_fill_manual(values = c("orange","orange","purple","purple")) +
  # scale_color_manual(values = c("orange","orange","purple","purple")) +
  scale_fill_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  scale_color_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  labs(x='\nClimate Scenario',
       y='Difference in nest success')+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.position = "none") +
  facet_grid(rows = vars(NewLU1),
             labeller = labeller(NewLU1 = labs.LU))
p1

#p2 <- ggplot(df2, aes(x=NewLU1, y=dif.success)) +
#   geom_violin(color='grey', fill='grey')+
#   geom_boxplot(width=.1, color='gray20', fill='gray20',
#                outlier.shape = NA) + #remove outliers
#   #geom_boxplot(color='gray20', outlier.alpha = .2) + 
#   geom_hline(yintercept=0, linetype="dashed", color = "blue") +
#   stat_summary(fun=median, geom="point", size=1.2, color="white")+
#   scale_x_discrete(labels=labs.LU) +
#   labs(x='\nLand Use',
#        y='Difference in nest success')+
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(colour = "black", size=1)) +
#   facet_grid(rows = vars(scenario),
#              labeller = labeller(scenario = labs.scenario))
# p2

df3 <- df %>%
  select(contains('y.dif.average.10'), contains('y.dif.average.90'), NewLU1) %>%
  mutate(y.dif.average.conf.rcp.45m = abs(y.dif.average.90.rcp.45m - y.dif.average.10.rcp.45m),
         y.dif.average.conf.rcp.45e = abs(y.dif.average.90.rcp.45e - y.dif.average.10.rcp.45e),
         y.dif.average.conf.rcp.85m = abs(y.dif.average.90.rcp.85m - y.dif.average.10.rcp.85m),
         y.dif.average.conf.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  select(contains('conf'), NewLU1) %>%
  pivot_longer(-NewLU1,names_to='scenario', values_to='dif.success.conf') %>%
  mutate(scenario = as.factor(scenario)) %>%
  as.data.frame()

#reorder climate scenarios
levels(df3$scenario)
df3$scenario <- factor(df3$scenario, levels=c("y.dif.average.conf.rcp.45m", "y.dif.average.conf.rcp.45e",
                                              "y.dif.average.conf.rcp.85m", "y.dif.average.conf.rcp.85e"))
levels(df3$scenario)

levels(df3$scenario)
labs.scenario <- c("RCP 4.5\n2050","RCP 4.5\n2100","RCP 8.5\n2050","RCP 8.5\n2100")
names(labs.scenario) <- c("y.dif.average.conf.rcp.45m", "y.dif.average.conf.rcp.45e",
                          "y.dif.average.conf.rcp.85m", "y.dif.average.conf.rcp.85e")

p3 <- ggplot(df3, aes(x=scenario, y=dif.success.conf)) +
  #geom_violin(color='grey', aes(fill=as.factor(scenario)))+
  geom_violin(aes(fill=as.factor(scenario), color=as.factor(scenario)))+
  #geom_violin(color='grey', fill='grey')+
  geom_boxplot(width=.1, color='gray20', fill='gray20',
               outlier.shape = NA) + #remove outliers
  #geom_boxplot(color='gray20', outlier.alpha = .2) + 
  #geom_hline(yintercept=0, linetype="dashed", color = "blue") +
  stat_summary(fun=median, geom="point", size=.8, color="white")+
  scale_x_discrete(labels=labs.scenario) +
  # scale_fill_viridis(discrete = TRUE) +
  # scale_color_viridis(discrete = TRUE) +
  # scale_fill_manual(values = c("orange","orange","purple","purple")) +
  # scale_color_manual(values = c("orange","orange","purple","purple")) +
  scale_fill_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  scale_color_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  labs(x='\nClimate Scenario',
       y='Uncertainty')+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.position = "none") +
  facet_grid(rows = vars(NewLU1),
             labeller = labeller(NewLU1 = labs.LU))
p3


plot <- plot_grid(p1, p3,
          ncol = 2, nrow=1,
          labels = c("A","B"), label_size = 20, scale=.9)
plot

ggsave('figures/violinplots.projections.png',
       width = 8, height = 5) 
