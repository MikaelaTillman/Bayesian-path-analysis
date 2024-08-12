#prev_fall on fall
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_springstd2sd_prev_fall_std_2_sd,
         b1 = b_fallstd2sd_spring_std_2_sd,
         b2 = b_fallstd2sd_success_cent,
         d21 = b_successcent_spring_std_2_sd/4,
         c_prime =b_fallstd2sd_prev_fall_std_2_sd)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ddirect_effect = c_prime,
         etotal_effect = c_prime + a1b1  + ba1d21b2)

post %>% 
  pivot_longer(a1b1:etotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 4)

ind_fall_fall <- post %>% 
  pivot_longer(a1b1:etotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 4)

ind_fall_fall <-ind_fall_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_fall_fall <-as.data.frame(ind_fall_fall )
x <- cbind(c("xind_prev_fall_spring_fall", "vind_prev_fall_spring_succ_fall", "udir_prev_fall_fall", "ttotal_prev_fall_fall"))
ind_prev_fall_fall <- cbind(ind_fall_fall,x)
xtable(ind_prev_fall_fall, digits = 4)

#dens on fall
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_springstd2sd_winter_dens_std_2_sd,
         b1 = b_fallstd2sd_spring_std_2_sd,
         a2 = b_successcent_winter_dens_std_2_sd/4,
         b2 = b_fallstd2sd_success_cent,
         d21 = b_successcent_spring_std_2_sd/4)

post %>% 
  pivot_longer(a1:d21) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ca2b2  = a2*b2,
         ftotal_effect =  a1b1  + ba1d21b2 + ca2b2)

ind_dens_fall <- post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_dens_fall <- ind_dens_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_dens_fall <-as.data.frame(ind_dens_fall )

ind_dens_fall <- post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

x <- cbind(c("sind_dens_spring_fall", "rind_dens_spring_succ_fall", "qind_dens_success_fall",  "ototal_dens_fall"))
ind_dens_fall <- cbind(ind_dens_fall,x)
xtable(ind_dens_fall)
xtable(ind_dens_fall, digits = 4)

#Max_snow on fall
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_springstd2sd_max_snow_std_2_sd,
         b1 = b_fallstd2sd_spring_std_2_sd,
         a2 = b_successcent_max_snow_std_2_sd/4,
         b2 = b_fallstd2sd_success_cent,
         d21 = b_successcent_spring_std_2_sd/4,
         c_prime =b_fallstd2sd_max_snow_std_2_sd)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ca2b2  = a2*b2,
         edirect_effect = c_prime,
         ftotal_effect = edirect_effect + a1b1  + ba1d21b2 + ca2b2)

post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_snow_fall <- post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_snow_fall <-ind_snow_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_snow_fall <-as.data.frame(ind_snow_fall )

x <- cbind(c("nind_snow_spring_fall", "mind_snow_spring_succ_fall", "lind_snow_success_fall", "kdir_snow_fall", "jtotal_snow_fall"))
ind_snow_fall <- cbind(ind_snow_fall,x)
xtable(ind_snow_fall)
xtable(ind_snow_fall, digits = 4)

#spring BM indirect effect on fall BM
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_successcent_spring_std_2_sd/4,
         b1 = b_fallstd2sd_success_cent,
         c_prime =  b_fallstd2sd_spring_std_2_sd)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spring_fall <- post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spring_fall <-ind_spring_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_spring_fall <-as.data.frame(ind_spring_fall )
x <- cbind(c("iind_spring_succes_fall", "hdir_spring_fall", "gtotal_spring_fall"))
ind_spring_fall <- cbind(ind_spring_fall,x)
xtable(ind_spring_fall)
xtable(ind_spring_fall)
xtable(ind_spring_fall, digits = 4)

#spring onset indirect effect on fall BM
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_successcent_spr_std_2_sd/4,
         b1 = b_fallstd2sd_success_cent,
         c_prime =  b_fallstd2sd_spr_std_2_sd)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spr_fall <- post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)


ind_spr_fall <-ind_spr_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_spr_fall <-as.data.frame(ind_spr_fall )
x <- cbind(c("find_spr_fall", "edir_spr_fall", "dtotal_spr_fall"))
ind_spr_fall <- cbind(ind_spr_fall,x)
xtable(ind_spr_fall)
xtable(ind_spr_fall, digits = 4)

#max EVIindirect effect on fall BM
post <- 
  posterior_samples(mod_std) %>% 
  mutate(a1 = b_successcent_max_std_2_sd/4,
         b1 = b_fallstd2sd_success_cent,
         c_prime =  b_fallstd2sd_max_std_2_sd)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_max_fall <-
  post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_max_fall <-ind_max_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_max_fall <-as.data.frame(ind_max_fall )
x <- cbind(c("cind_max_fall", "bdir_max_fall", "atotal_max_fall"))
ind_max_fall <- cbind(ind_max_fall,x)
xtable(ind_max_fall)
xtable(ind_max_fall, digits = 4)

indirect_fall <- rbind(ind_prev_fall_fall, ind_dens_fall, ind_snow_fall, ind_spring_fall, ind_spr_fall, ind_max_fall)

Predictor <- rbind("Autumn BM(t-1)", "Autumn BM(t-1)", "Autumn BM(t-1)", "Autumn BM(t-1)", 
                   "Population density winter(t)", "Population density winter(t)", "Population density winter(t)","Population density winter(t)",
                   "Snow depth(t)", "Snow depth(t)", "Snow depth(t)","Snow depth(t)","Snow depth(t)",
                   "Spring BM(t)", "Spring BM(t)", "Spring BM(t)",
                   "Spring onset(t)", "Spring onset(t)", "Spring onset(t)", 
                   "Peak plant prod.(t)","Peak plant prod.(t)","Peak plant prod.(t)")

Path <- rbind("fmediated_by_spring", "emediated by_spring_and_success", "bdirect","atotal",
              "fmediated_by_spring", "emediated by_spring_and_success", "dmediated_by_success",  "atotal",
              "fmediated_by_spring", "emediated by_spring_and_success", "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal")

indirect_fall <- cbind(indirect_fall, Predictor, Path)

neworder <- c("Autumn BM(t-1)", "Population density winter(t)","Snow depth(t)", "Spring BM(t)", "Spring onset(t)", "Peak plant prod.(t)")
library(plyr)  ## or dplyr (transform -> mutate)
indirect_fall <- arrange(transform(indirect_fall,
                  Predictor = factor(Predictor, levels = neworder)),Predictor)
indirect_plot_fall<-  
  ggplot(indirect_fall, aes(value, Path)) +
  theme_gray(base_size = 10) + geom_vline(xintercept = 0) +
  theme(        axis.ticks.y=element_blank() ) +
  geom_pointrange(aes(xmin = .lower, xmax = .upper),color = "deepskyblue4", position = position_dodge2(width = 1)) +
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 4, size = 10)) +
  labs(title = "Relative effects on autumn body mass") +
  facet_wrap( ~Predictor ) +
  scale_x_continuous(name ="Effect size", 
                     limits=c(-0.4, 0.65), breaks = c(-0.3, 0, 0.3, 0.6)) +
    scale_y_discrete( name = NULL, labels = 
                      c( "fmediated_by_spring"             = "Indirect effect mediated by spring BM",
                         "emediated by_spring_and_success" = "Indirect effect mediated by spring BM & repr. success",
                         "bdirect"                         = "Direct effect",
                         "atotal"                          = "Total effect",
                         "dmediated_by_success"            = "Indirect effect mediated by repr. success"
                         )) +
  theme(strip.text = element_text(size= 9.5),
        axis.title.x = element_text(vjust = 0, size= 12),
        axis.text.x = element_text(size= 12,hjust = 0.5),
        axis.text.y = element_text(size= 12),
        text = element_text(family = "sans")) 
indirect_plot_fall

#absolute
#fall on fall
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_spring_prev_fall_cent,
         b1 = b_fall_spring_cent,
         b2 = b_fall_success_cent,
         d21 = b_success_spring_cent/4,
         c_prime =b_fall_prev_fall_cent)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ddirect_effect = c_prime,
         etotal_effect = c_prime + a1b1  + ba1d21b2)

post %>% 
  pivot_longer(a1b1:etotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 4)

ind_fall_fall <- post %>% 
  pivot_longer(a1b1:etotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 4)

ind_fall_fall <-ind_fall_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_fall_fall <-as.data.frame(ind_fall_fall )
x <- cbind(c("xind_prev_fall_spring_fall", "vind_prev_fall_spring_succ_fall", "udir_prev_fall_fall", "ttotal_prev_fall_fall"))
ind_prev_fall_fall <- cbind(ind_fall_fall,x)
xtable(ind_prev_fall_fall, digits = 4)

#dens on fall
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_spring_winter_dens_cent,
         b1 = b_fall_spring_cent,
         a2 = b_success_winter_dens_cent/4,
         b2 = b_fall_success_cent,
         d21 = b_success_spring_cent/4)

post %>% 
  pivot_longer(a1:d21) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ca2b2  = a2*b2,
         ftotal_effect = a1b1  + ba1d21b2 + ca2b2)

post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_dens_fall <- ind_dens_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_dens_fall <-as.data.frame(ind_dens_fall )

ind_dens_fall <- post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

x <- cbind(c("sind_dens_spring_fall", "rind_dens_spring_succ_fall", "qind_dens_success_fall", "ototal_dens_fall"))
ind_dens_fall <- cbind(ind_dens_fall,x)
xtable(ind_dens_fall)
xtable(ind_dens_fall, digits = 4)

#Max_snow on fall
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_spring_max_snow_m_cent,
         b1 = b_fall_spring_cent,
         a2 = b_success_max_snow_m_cent/4,
         b2 = b_fall_success_cent,
         d21 = b_success_spring_cent/4,
         c_prime =b_fall_max_snow_m_cent)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1,
         ba1d21b2 = a1 * d21 * b2,
         ca2b2  = a2*b2,
         edirect_effect = c_prime,
         ftotal_effect = edirect_effect + a1b1  + ba1d21b2 + ca2b2)

post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_snow_fall <- post %>% 
  pivot_longer(a1b1:ftotal_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_snow_fall <-ind_snow_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_snow_fall <-as.data.frame(ind_snow_fall )

x <- cbind(c("nind_snow_spring_fall", "mind_snow_spring_succ_fall", "lind_snow_success_fall", "kdir_snow_fall", "jtotal_snow_fall"))
ind_snow_fall <- cbind(ind_snow_fall,x)
xtable(ind_snow_fall)
xtable(ind_snow_fall, digits = 4)

#spring BM indirect effect on fall BM
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_success_spring_cent/4,
         b1 = b_fall_success_cent,
         c_prime =  b_fall_spring_cent)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spring_fall <- post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spring_fall <-ind_spring_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_spring_fall <-as.data.frame(ind_spring_fall )
x <- cbind(c("iind_spring_succes_fall", "hdir_spring_fall", "gtotal_spring_fall"))
ind_spring_fall <- cbind(ind_spring_fall,x)
xtable(ind_spring_fall)
xtable(ind_spring_fall)
xtable(ind_spring_fall, digits = 4)

#spring onset indirect effect on fall BM
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_success_spr_ten_cent/4,
         b1 = b_fall_success_cent,
         c_prime =  b_fall_spr_ten_cent)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spr_fall <- post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_spr_fall <-ind_spr_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_spr_fall <-as.data.frame(ind_spr_fall )
x <- cbind(c("find_spr_fall", "edir_spr_fall", "dtotal_spr_fall"))
ind_spr_fall <- cbind(ind_spr_fall,x)
xtable(ind_spr_fall)
xtable(ind_spr_fall, digits = 4)

#max EVIindirect effect on fall BM
post <- 
  posterior_samples(mod_cent) %>% 
  mutate(a1 = b_success_max_hundred_cent/4,
         b1 = b_fall_success_cent,
         c_prime =  b_fall_max_hundred_cent)

post %>% 
  pivot_longer(a1:c_prime) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is_double, round, digits = 5)

post <-
  post %>%  
  mutate(a1b1    = a1 * b1) %>% 
  mutate(c_prime = c_prime) %>%
  mutate(total_effect = c_prime + a1b1)

post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_max_fall <-
  post %>% 
  pivot_longer(c_prime:total_effect) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  select(name:.upper) %>% 
  mutate_if(is_double, round, digits = 5)

ind_max_fall <-ind_max_fall  %>% mutate_if(is.numeric, ~round(., 5))
ind_max_fall <-as.data.frame(ind_max_fall )
x <- cbind(c("cind_max_fall", "bdir_max_fall", "atotal_max_fall"))
ind_max_fall <- cbind(ind_max_fall,x)
xtable(ind_max_fall)
xtable(ind_max_fall, digits = 4)

indirect_fall <- rbind(ind_prev_fall_fall, ind_dens_fall, ind_snow_fall, ind_spring_fall, ind_spr_fall, ind_max_fall)

Predictor <- rbind("Autumn BM(t-1) [kg]", "Autumn BM(t-1) [kg]", "Autumn BM(t-1) [kg]", "Autumn BM(t-1) [kg]",  
                   "Pop. dens. winter(t) [ind./sq.km]", "Pop. dens. winter(t) [ind./sq.km]", "Pop. dens. winter(t) [ind./sq.km]",  "Pop. dens. winter(t) [ind./sq.km]",
                   "Snow depth(t) [m]", "Snow depth(t) [m]", "Snow depth(t) [m]","Snow depth(t) [m]","Snow depth(t) [m]",
                   "Spring BM(t) [kg]", "Spring BM(t) [kg]", "Spring BM(t) [kg]",
                   "Spring onset(t) [10 days]", "Spring onset(t) [10 days]", "Spring onset(t) [10 days]", 
                   "Peak plant prod.(t) [% EVI]","Peak plant prod.(t) [% EVI]","Peak plant prod.(t) [% EVI]")

Path <- rbind("fmediated_by_spring", "emediated by_spring_and_success", "bdirect","atotal",
              "fmediated_by_spring", "emediated by_spring_and_success", "dmediated_by_success", "atotal",
              "fmediated_by_spring", "emediated by_spring_and_success", "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal",
              "dmediated_by_success", "bdirect", "atotal")

indirect_fall <- cbind(indirect_fall, Predictor, Path)

neworder <- c("Autumn BM(t-1) [kg]", "Pop. dens. winter(t) [ind./sq.km]",
              "Snow depth(t) [m]", "Spring BM(t) [kg]", 
              "Spring onset(t) [10 days]", "Peak plant prod.(t) [% EVI]")
library(plyr)  ## or dplyr (transform -> mutate)
indirect_fall <- arrange(transform(indirect_fall,
                                   Predictor = factor(Predictor, levels = neworder)),Predictor)

indirect_plot_fall_abs <-  
  ggplot(indirect_fall, aes(value, Path)) +
  theme_gray(base_size = 10) + geom_vline(xintercept = 0) +
  theme(        axis.ticks.y=element_blank() ) +
  geom_pointrange(aes(xmin = .lower, xmax = .upper),color = "deepskyblue4", position = position_dodge2(width = 1)) +
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 4, size = 10)) +
  labs(title = "Absolute effects on autumn body mass") +
  facet_wrap( ~Predictor ) +
  scale_x_continuous(name ="Effect size", 
                     limits=c(-2.33, 5), breaks = c(-2, 0, 2, 4)) +
  scale_y_discrete( name = NULL, labels = 
                      c( "fmediated_by_spring"             = "Indirect effect mediated by spring BM",
                         "emediated by_spring_and_success" = "Indirect effect mediated by spring BM & repr. success",
                         "bdirect"                         = "Direct effect",
                         "atotal"                          = "Total effect",
                         "dmediated_by_success"            = "Indirect effect mediated by repr. success"
                      )) +
  theme(strip.text = element_text(size= 9.5),
        axis.title.x = element_text(vjust = 0, size= 12),
        axis.text.x = element_text(size= 12, hjust = 0.5),
        axis.text.y = element_text(size= 12),
        text = element_text(family = "sans")) 
indirect_plot_fall_abs
