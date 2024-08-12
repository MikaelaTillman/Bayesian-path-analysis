library(dplyr);library(tidyverse) # libraries used for tidying data

data <-read.delim("data.txt") 

data <- data %>% 
  select(!X) %>% 
  mutate(prev_fall_std_2_sd = (prev_fall - mean(prev_fall)) / (2*sd(prev_fall))) %>%  #standardising
  mutate(spring_std_2_sd = (spring - mean(spring)) / (2*sd(spring))) %>%              #standardising
  mutate(fall_std_2_sd = (fall - mean(fall)) / (2*sd(fall))) %>%                      #standardising
  mutate(prev_fall_cent = scale(prev_fall, center = T, scale = F)) %>%                #centering
  mutate(spring_cent = scale(spring, center = T, scale = F)) %>%                      #centering
  mutate(fall_cent = scale(fall, center = T, scale = F)) %>%                          #centering
  mutate(success_cent = scale(success, center = T, scale = F)) %>%                    #centering
 
  mutate(max_snow_std_2_sd = (max_snow - mean(max_snow)) / (2*sd(max_snow))) %>%      #standardising
  mutate(max_snow_m = max_snow/1000) %>%                                              #scaling max snow depth to meters
  mutate(max_snow_m_cent = max_snow_m - mean(max_snow_m)) %>%                         #centering

  mutate(spr_ten = spr/10) %>%                                                        #scaling spring onset to per 10 days
  mutate(max_hundred = max*100) %>%                                                   #scaling max. EVI
  mutate(spr_std_2_sd = (spr - mean(spr)) / (2*sd(spr))) %>%                          #standardising 
  mutate(max_std_2_sd = (max - mean(max)) / (2*sd(max))) %>%                          #standardising 
  mutate(max_hundred_cent = scale(max_hundred, center = T, scale = F)) %>%            #centering
  mutate(spr_ten_cent = spr_ten - mean(spr_ten)) %>%                                  #centering
 
  mutate(winter_dens_std_2_sd = (winter_dens - mean(winter_dens)) / (2*sd(winter_dens))) %>%                      #standardising 
  mutate(winter_dens_cent = winter_dens - mean(winter_dens))  %>%                                              #centering

mutate(summer_dens_std_2_sd = (summer_dens - mean(summer_dens)) / (2*sd(summer_dens))) %>%                      #standardising 
  mutate(summer_dens_cent = summer_dens - mean(summer_dens))                                               #centering
names(data)
countid<- data %>% count(individ_dbid) #233 in, 1-13 get information on data structure
countid <- countid %>% 
  mutate(n = n+1)
countyr<- data %>% count(yr) # 18 in 21-65 get information on data structure

mean(countid$n)
sd(countid$n)
range(countid$n)


mean(countyr$n)
sd(countyr$n)
range(countyr$n)



(View(countid))
