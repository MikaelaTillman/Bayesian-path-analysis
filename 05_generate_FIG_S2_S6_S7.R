pacman::p_load(ggplot2, Hmisc, dplyr, rstan, rstanarm, brms, doBy)
data2 <- data %>% 
         mutate(age = ifelse(age >
                               9, 9, age)
  ) %>% 
  select(winter_dens, summer_dens, success, spring, fall, prev_fall, age, yr, individ_dbid)

data2.age5 <- data2 %>% 
  mutate(age = 5)

#spring
stan.lmer <- stan_lmer(spring ~ factor(success) + factor(age) + (1|yr)-1 + (1|individ_dbid), data = data2, subset =  !yr %in% c(2002), iter = 5000)
posterior_spring_adj =  posterior_predict(stan.lmer, data2.age5) 
spring_adj_mean <- apply(posterior_spring_adj, 2, mean)

data3.age5 <- data2.age5 %>% 
  mutate(spring_adj = spring_adj_mean)

# data3.age5 %>% 
#   ggplot(aes(dens, spring_adj, color = as.factor(success))) + geom_point() +
#   geom_smooth(method=lm, se=FALSE, size=1) 
  

stan.data <- stan_glm(spring_adj ~ factor(yr)-1, family=gaussian, data=data3.age5, subset=  !yr %in% c(2002), iter = 5000)

spring.data <- data.frame( summary(stan.data, digits=4, regex_pars='^f', 
                                   probs = c(0.50, 0.025, 0.975))[,c(4:6)])

spring.data$Year <- as.numeric( gsub('\\D+','', rownames(spring.data)) )
names(spring.data) <- c('y','low','high','Year')

spring_time <- ggplot(spring.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous( limits=c(55, 85)) +
  labs( y =  bquote(Spring~body~mass[t]~(kg))) +
  scale_x_continuous() +
  theme(           axis.title.x = element_blank(),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans"),
  )  
spring_time

# fall
stan.lmer.fall <- stan_lmer(fall ~ factor(success) + factor(age) + factor(yr) - 1 + (1|individ_dbid), data = data2, subset=  !yr %in% c(2002), iter = 5000)

posterior_fall_adj =  posterior_predict(stan.lmer.fall, data2.age5) 
fall_adj_mean <- apply(posterior_fall_adj, 2, mean)

data4.age5 <- data3.age5 %>% 
  mutate(fall_adj = fall_adj_mean)

stan.data.fall <- stan_glm(fall_adj_mean ~ factor(yr)-1, family = gaussian, data = data4.age5, subset =  !yr %in% c(2002), iter = 5000)
                           
fall.data <- data.frame( summary(stan.data.fall, digits = 4, regex_pars = '^f', 
                                   probs = c(0.50, 0.025, 0.975))[,c(4:6)]);fall.data

fall.data$Year <- as.numeric( gsub('\\D+','', rownames(fall.data)) )
names(fall.data) <- c('y','low','high','Year')

fall_time <- ggplot(fall.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous( limits=c(65, 85)) +
  labs( y =  bquote(Autumn~body~mass[t]~(kg))) +
  scale_x_continuous() +
  ylim(62,82) +
  theme(           axis.title.x = element_blank(),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans"),)  
fall_time

# prev_fall
stan.lmer.prev_fall <- stan_lmer(prev_fall ~ factor(success) + factor(age) + factor(yr)-1 + (1|individ_dbid), data = data2, subset =!yr %in% c(2002), iter = 5000)

posterior_prev_fall_adj =  posterior_predict(stan.lmer.prev_fall, data2.age5) 
prev_fall_adj_mean <- apply(posterior_prev_fall_adj, 2, mean)

data5.age5 <- data4.age5 %>% 
  mutate(prev_fall_adj = prev_fall_adj_mean)

#stan.data <- stan_glm((as.numeric(prev_fall))~factor(yr)-1, family=gaussian, data=data4.age5, subset=  !yr %in% c(2002))
stan.data.prev_fall <- stan_glm(prev_fall_adj_mean ~ factor(yr)-1, family = gaussian, data = data5.age5, subset = !yr %in% c(2002), iter=5000)

prev_fall.data <- data.frame( summary(stan.data.prev_fall, digits = 4, regex_pars = '^f', 
                                 probs = c(0.50, 0.025, 0.975))[,c(4:6)]);prev_fall.data

prev_fall.data$Year <- as.numeric( gsub('\\D+','', rownames(prev_fall.data)) )
names(prev_fall.data) <- c('y','low','high','Year')

#change over winter and summer
#data2 <- data %>% 
data2.adj <- data5.age5 %>%   
  mutate(#winter = spring - prev_fall,
         #summer = fall - spring,
         winter_adj = spring_adj_mean - prev_fall_adj_mean,
         summer_adj = fall_adj_mean - spring_adj_mean)

#winter
stan.data.winter <- stan_glm(winter_adj ~ factor(yr)-1, family = gaussian, data = data2.adj, subset = !yr %in% c(2002))
winter.data <- data.frame( summary(stan.data.winter, digits=4, regex_pars='^f', 
                                   probs = c(0.50, 0.025, 0.975))[,c(4:6)]);winter.data

winter.data$Year <- as.numeric( gsub('\\D+','', rownames(winter.data)) )
names(winter.data) <- c('y','low','high','Year')

winter_time <- ggplot(winter.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous( limits=c(-11, 6)) +
  labs( y =  bquote(Body~mass~change~winter~(kg))) +
  scale_x_continuous() +
  ylim(-11, 16) +
  theme(           axis.title.x = element_text(vjust = 0, size=12),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans"))  
winter_time

#summer
stan.data <- stan_glm(summer_adj ~ factor(yr) - 1, family = gaussian, data = data2.adj, subset = !yr %in% c(2002))
summer.data <- data.frame( summary(stan.data, digits = 4, regex_pars = '^f', 
                                   probs = c(0.50, 0.025, 0.975))[,c(4:6)]);summer.data

summer.data$Year <- as.numeric( gsub('\\D+','', rownames(summer.data)) )
names(summer.data) <- c('y','low','high','Year')

summer_time <- ggplot(summer.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous( limits=c(-4, 16)) +
  labs( y =  bquote(Body~mass~change~summer~(kg))) +
  scale_x_continuous() +
  ylim(-11, 16) +
  theme(           axis.title.x = element_text(vjust = 0, size=12),
                   axis.title.y = element_text(vjust = 2, size=12),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans"))  
summer_time

#success
stan.lmer.success <- stan_glmer(success ~ factor(age)-1 + (1|yr)+(1|individ_dbid), family = binomial, data = data2, subset = !yr %in% c(2002), iter = 5000)

posterior_success_adj =  posterior_predict(stan.lmer.success, data2.age5) 
success_adj_mean <-  apply(posterior_success_adj, 2, mean)

data6.age5 <- data5.age5 %>% 
  mutate(success_adj_mean = as.numeric(success_adj_mean))

success.data <- doBy::summary_by(success_adj_mean ~ yr, data = data5.age5, FUN = gmodels::ci)
names(success.data) <- c('Year','y', 'low','high','Error')

success_time <- ggplot(success.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") +
  geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") +
  ggtitle("") +
  labs( y =  bquote(Reproductive~success[t])) +
  theme(           axis.title.x = element_blank(),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans")) #transparent legend panel
success_time

######
#####
####
###
##
#

#env predictors, no need correcting for age
#poulation density in winter
stan.data <- stan_glm(winter_dens~factor(yr)-1, family = gaussian, data=data, subset=  !yr %in% c(2002))
winter_dens_time_STAN <- performance::r2_bayes(stan.data)

winter_dens.data <- data.frame(
  # plogis(
  summary(stan.data, digits=4, regex_pars='^f', probs = c(0.50, 0.025, 0.975)
  )[,c(4:6)])#)
#;dens.data
winter_dens.data$Year <- as.numeric( gsub('\\D+','', rownames(winter_dens.data)) )
names(winter_dens.data) <- c('y','low','high','Year')

winter_dens_time <- ggplot(winter_dens.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") +  ggtitle("") +
  scale_y_continuous( limits=c(1, 4.1)) +
  labs( y =  bquote(Population~density~winter[t]*~(ind./km^2))) +
  ylim(1, 7) +
  theme(           axis.title.x = element_blank(),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.y = element_text(size= 12)) #transparent legend panel

winter_dens_time

#summer dens
stan.data <- stan_glm(summer_dens~factor(yr)-1, family = gaussian, data=data, subset=  !yr %in% c(2002))
summer_dens_time_STAN <- performance::r2_bayes(stan.data)

summer_dens.data <- data.frame(
  # plogis(
  summary(stan.data, digits=4, regex_pars='^f', probs = c(0.50, 0.025, 0.975)
  )[,c(4:6)])#)
#;dens.data
summer_dens.data$Year <- as.numeric( gsub('\\D+','', rownames(summer_dens.data)) )
names(summer_dens.data) <- c('y','low','high','Year')

summer_dens_time <- ggplot(summer_dens.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") +  ggtitle("") +
  #scale_y_continuous( limits=c(1, 4.1)) +
  labs( y =  bquote(Population~density~summer[t]*~(ind./km^2))) +
  ylim(1, 7) +
  theme(           axis.title.x = element_blank(),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.title.y = element_text(vjust = 2, size=12,),
                   axis.text.y = element_text(size= 12)) #transparent legend panel

summer_dens_time

ggarrange(winter_dens_time, summer_dens_time)

#max_snow_m
data <- data %>% 
  mutate(max_snow_m = max_snow/1000)                                         #scaling max snow depth to meters
  
stan.data <- stan_glm(max_snow_m~factor(yr)-1, family=gaussian, data=data, subset=  !yr %in% c(2002))
max_snow_m.data <- data.frame(
  # plogis(
  summary(stan.data, digits=4, regex_pars='^f', probs = c(0.50, 0.025, 0.975)
  )[,c(4:6)])#)
max_snow_m.data$Year <- as.numeric( gsub('\\D+','', rownames(max_snow_m.data)) )
names(max_snow_m.data) <- c('y','low','high','Year')

max_snow_m_time <- ggplot(max_snow_m.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") +
  geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous( limits=c(0.5, 3.9)) +
  labs( y =  bquote(Snow~depth[t]~(m))) +
  theme(           axis.title.x = element_text(vjust = 0, size=12),
                   axis.title.y = element_text(vjust = 2, size=12),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans")) 
max_snow_m_time

#spring onset
stan.data <- stan_glm(spr~factor(yr)-1, family=gaussian, data=data, subset=  !yr %in% c(2002))
spr.data <- data.frame(
  # plogis(
  summary(stan.data, digits=4, regex_pars='^f', probs = c(0.50, 0.025, 0.975)
  )[,c(4:6)])#)
spr.data$Year <- as.numeric( gsub('\\D+','', rownames(spr.data)) )
names(spr.data) <- c('y','low','high','Year')

spr_time <- ggplot(spr.data, aes(Year, y), colour = "deepskyblue4") +
  #  geom_smooth(method=lm, se=FALSE, col="orange2", size=1) +
  geom_point(color = "deepskyblue4") + geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  scale_y_continuous(breaks = (c(153, 160, 167, 174, 181)),
                                    labels = c("1/6","8/6", "15/6", "22/6", "29/6")) +
  labs(y = bquote(Spring~onset[t]~(date))) +
  theme(           axis.title.x = element_text(vjust = 0, size=12),
                   axis.title.y = element_text(vjust = 2, size=12),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans")) 
spr_time

#max_hundred
data <- data %>% 
    mutate(max_hundred = max*100)                                                 #scaling max. EVI

stan.data <- stan_glm((as.numeric(max_hundred))~factor(yr)-1, family=gaussian, data=data, subset=  !yr %in% c(2002))
max_hundred.data <- data.frame( summary(stan.data, digits=4, regex_pars='^f', 
                                 probs = c(0.50, 0.025, 0.975)
)[,c(4:6)])#)
max_hundred.data$Year <- as.numeric( gsub('\\D+','', rownames(max_hundred.data)) )
names(max_hundred.data) <- c('y','low','high','Year')

max_hundred_time <- ggplot(max_hundred.data, aes(Year, y), colour = "deepskyblue4") +
  geom_point(color = "deepskyblue4") +
  geom_line(color = "deepskyblue4", size = 1) +
  geom_linerange(aes(Year, ymin = low, ymax = high), color = "deepskyblue4") + 
  theme(legend.position = "none") + ggtitle("") +
  #scale_y_continuous(limits=c(0, 30)) +
  labs(y = bquote(Plant~productivity[t]~(EVI))) +
    theme(           axis.title.x = element_text(vjust = 0, size=12),
                   axis.title.y = element_text(vjust = 2, size=12),
                   axis.text.x = element_text(size= 12, hjust = 0.5),
                   axis.text.y = element_text(size= 12),
                   text = element_text(family = "sans")) 
max_hundred_time

############
###########
#########
#######
#####
###
##
data_adjusted <- data2.adj 
data_adjusted<- data_adjusted %>%
  purrr::modify_at(c("age", "success"), factor) #need to factorise age again when R restarted

data_count <- data_adjusted %>% #to make size of dots in graph
  group_by( success, winter_dens) %>%
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(winter_dens) %>% 
  mutate(freq = n / sum(n))

dataprevfall <- data_adjusted %>% #split up lactating barren
  group_by(winter_dens, success) %>%
  dplyr::mutate(mean_prev_fall_adj = mean(prev_fall_adj)) %>% 
  select(winter_dens, mean_prev_fall_adj, success)

dataspring <- data_adjusted %>%  #split up lactating barren
  group_by(winter_dens, success) %>%
  dplyr::mutate(mean_spring_adj = mean(spring_adj)) %>% 
  select(winter_dens, mean_spring_adj, success)

datafall <- data_adjusted %>%  #split up lactating barren
  group_by(winter_dens, success) %>%
  dplyr::  mutate(mean_fall_adj = mean(fall_adj)) %>% 
  select(winter_dens, mean_fall_adj, success)

datawinter <- data_adjusted %>% #split up lactating barren
  group_by(winter_dens, success) %>%
  mutate(winter_adj = spring_adj-prev_fall_adj) %>% 
  dplyr::mutate(mean_winter_adj = mean(winter_adj)) %>% 
  select(winter_dens, mean_winter_adj, success)

datasummer <- data_adjusted %>% #split up lactating barren
  group_by(winter_dens, success) %>%
  mutate(summer_adj = fall_adj - spring_adj) %>% 
  dplyr::mutate(mean_summer_adj = mean(summer_adj)) %>% 
  select(winter_dens, mean_summer_adj, success)

data_count <-   left_join(data_count, dataprevfall, by = c("winter_dens", "success")) 
names(data_count)
data_count <-   dplyr::left_join(data_count, dataspring, by = c("winter_dens", "success")) %>% 
  distinct()
data_count <-   left_join(data_count, datafall, by = c("winter_dens", "success")) %>% 
  distinct()
data_count <-   left_join(data_count, datawinter, by = c("winter_dens", "success")) %>% 
  distinct()
data_count <-   left_join(data_count, datasummer, by = c("winter_dens", "success")) %>% 
  distinct() %>% 
  purrr::modify_at(c("success"), factor)

data_lact_bar <- data_adjusted %>% 
  group_by(winter_dens, success) %>% 
  mutate(summerdata_adj = fall_adj - spring_adj) %>% 
  mutate(winter_adj = spring_adj - prev_fall_adj) %>% 
  dplyr::mutate(upper_spring_adj  =   quantile(spring_adj, 0.75), 
    lower_spring_adj =   quantile(spring_adj, 0.25),
    mean_spring_adj   =   mean(spring_adj)) %>% 
  dplyr::mutate(upper_summer_adj  =   quantile(summer_adj, 0.75), 
    lower_summer_adj   =   quantile(summer_adj, 0.25),
    mean_summer_adj   =   mean(summer_adj)) %>% 
  dplyr:: mutate(upper_winter_adj =   quantile(winter_adj, 0.75), 
    lower_winter_adj =   quantile(winter_adj, 0.25),
    mean_winter_adj  =   mean(winter_adj)) %>% 
  dplyr::mutate(upper_fall_adj    =   quantile(fall_adj, 0.75), 
    lower_fall_adj    =   quantile(fall_adj, 0.25),
    mean_fall_adj     =   mean(fall_adj)) %>% 
  purrr::modify_at(c("success"), factor)


p <- ggplot (data_lact_bar, aes(winter_dens, spring_adj)) +
  geom_smooth(method = "lm", se = TRUE, aes(color = success))+
  geom_point(aes(group = success, color = success),  position = position_dodge(width = 0.09)
             , size = 0.8) + 
  scale_color_manual(values = c("deepskyblue4", "orange2"),  labels = c("Barren", "Lactating")) +
  labs( y =  bquote(Spring~body~mass[t]~(kg))) +
  geom_point(inherit.aes = FALSE, data = data_count, 
             aes(x = winter_dens, y = mean_spring_adj, color = success, size = freq), show.legend = TRUE, 
             position = position_dodge(width = 0.09),
             alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_spring_adj, ymax = upper_spring_adj, 
                    group = success, color = success), alpha = 0.02,
                size = 2.5, position = position_dodge(width = 0.09)) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 12),
        axis.text.x = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank() ) +
  ylim(54, 94) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
spring_success_plot <- p
spring_success_plot

p <- ggplot (data_lact_bar, aes(winter_dens, fall_adj)) +
  geom_smooth(method = "lm", se = TRUE, aes(color = success))+
  geom_point(aes(group = success, color = success),  position = position_dodge(width = 0.09)
             , size = 0.8) + 
  scale_color_manual(values = c("deepskyblue4", "orange2"),  labels = c("Barren", "Lactating")) +
  labs( y =  bquote(Autumn~body~mass[t]~(kg))) +
  geom_point(inherit.aes = FALSE, data = data_count, 
             aes(x = winter_dens, y = mean_fall_adj, color = success, size = freq), show.legend = TRUE, 
             position = position_dodge(width = 0.09),
             alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_fall_adj, ymax = upper_fall_adj, 
                    group = success, color = success), alpha = 0.02,
                size = 2.5, position = position_dodge(width = 0.09)) +  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 12),
        axis.text.x = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank() ) +
  ylim(54, 94) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
fall_success_plot <- p
fall_success_plot

p <- ggplot (data_lact_bar, aes(winter_dens, spring_adj-prev_fall_adj)) +
  geom_smooth(method = "lm", se = TRUE, aes(color = success))+
  geom_point(aes(group = success, color = success),  position = position_dodge(width = 0.09)
             , size = 0.8) + 
  scale_color_manual(values = c("deepskyblue4", "orange2"),  labels = c("Barren", "Lactating")) +
  labs( y =  bquote(Body~mass~change~winter~(kg))) +
  geom_point(inherit.aes = FALSE, data = data_count, 
             aes(x = winter_dens, y = mean_winter_adj, color = success, size = freq), show.legend = TRUE, 
             position = position_dodge(width = 0.09),
             alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_winter_adj, ymax = upper_winter_adj, 
                    group = success, color = success), alpha = 0.02,
                size = 2.5, position = position_dodge(width = 0.09)) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 12),
        axis.text.x = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank() ) +
  ylim(-16, 23) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
winter_success_plot <- p
winter_success_plot

p <- ggplot (data_lact_bar, aes(winter_dens, fall_adj - spring_adj)) +
  geom_smooth(method = "lm", se = TRUE, aes(color = success))+
  geom_point(aes(group = success, color = success),  position = position_dodge(width = 0.09)
             , size = 0.8) + 
  scale_color_manual(values = c("deepskyblue4", "orange2"),  labels = c("Barren", "Lactating")) +
  labs( y =  bquote(Body~mass~change~summer~(kg))) +
  geom_point(inherit.aes = FALSE, data = data_count, 
             aes(x = winter_dens, y = mean_summer_adj, color = success, size = freq), show.legend = TRUE, 
             position = position_dodge(width = 0.09),
             alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_summer_adj, ymax = upper_summer_adj, 
                    group = success, color = success), alpha = 0.02,
                size = 2.5, position = position_dodge(width = 0.09)) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 12),
        axis.text.x = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank() ) +
  ylim(-16, 23) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
summer_success_plot <- p
summer_success_plot
