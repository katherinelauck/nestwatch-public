##### Plot of Moran's I p-value, betas, beta p-values vs N across thinned models
##### Author: Katherine Lauck
##### Last updated: 2023 Feb 13

require(ggplot2)

source('Code/helper-functions.R')

morani_tab <- track_region_models()

fig_data <- morani_tab %>%
  filter(model %in% str_subset(morani_tab$model,"mainv1_[015]{1,2}perhex"))

oneperhex <- fig_data %>% 
  filter(model %in% str_subset(fig_data$model,"mainv1_1perhex")) %>%
  pivot_longer(cols = c(beta_ag:beta_human_p,p.value)) %>%
  mutate(LU = str_extract(name,pattern = "(ag)|(forest)|(open)|(human)")) %>%
  mutate(type = ifelse(str_detect(name,"_p"),"p",ifelse(str_detect(name,"beta"),"beta","morani")))
  
(p <- ggplot(data = filter(oneperhex,name != 'p.value'), mapping = aes(x = N,y = value,color = type)) +
  geom_line(data = filter(oneperhex,name == "p.value") %>% dplyr::select(!LU)) +
  geom_line() +
  facet_wrap(~ LU))

ggsave("figures/moranivsbeta_1perhex.png",plot = p,width = 5,height = 3)

fiveperhex <- fig_data %>% 
  filter(model %in% str_subset(fig_data$model,"mainv1_5perhex")) %>%
  pivot_longer(cols = c(beta_ag:beta_human_p,p.value)) %>%
  mutate(LU = str_extract(name,pattern = "(ag)|(forest)|(open)|(human)")) %>%
  mutate(type = ifelse(str_detect(name,"_p"),"p",ifelse(str_detect(name,"beta"),"beta","morani")))

(p <- ggplot(data = filter(fiveperhex,name != 'p.value'), mapping = aes(x = N,y = value,color = type)) +
    geom_line(data = filter(fiveperhex,name == "p.value") %>% dplyr::select(!LU)) +
    geom_line() +
    facet_wrap(~ LU))

ggsave("figures/moranivsbeta_5perhex.png",plot = p,width = 5,height = 3)

tenperhex <- fig_data %>% 
  filter(model %in% str_subset(fig_data$model,"mainv1_10perhex")) %>%
  pivot_longer(cols = c(beta_ag:beta_human_p,p.value)) %>%
  mutate(LU = str_extract(name,pattern = "(ag)|(forest)|(open)|(human)")) %>%
  mutate(type = ifelse(str_detect(name,"_p"),"p",ifelse(str_detect(name,"beta"),"beta","morani")))

(p <- ggplot(data = filter(tenperhex,name != 'p.value'), mapping = aes(x = N,y = value,color = type)) +
    geom_line(data = filter(tenperhex,name == "p.value") %>% dplyr::select(!LU)) +
    geom_line() +
    facet_wrap(~ LU))

ggsave("figures/moranivsbeta_10perhex.png",plot = p,width = 5,height = 3)
