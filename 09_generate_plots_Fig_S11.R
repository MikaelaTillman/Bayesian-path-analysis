library(xtable)
library(tidybayes)
library(purrr)
#prev fall success
  post <- 
    posterior_samples(mod_std) %>% 
    mutate(a1 = b_springstd2sd_prev_fall_std_2_sd,
           b1 = b_successcent_spring_std_2_sd/4)
  post %>% 
    pivot_longer(a1:b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    mutate_if(is_double, round, digits = 4)
  
  post <-
    post %>%  
    mutate(a1b1    = a1 * b1)
  
  
  post %>% 
    pivot_longer(a1b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 4)
  
  ind_prev_fall_succ <-post %>% 
    pivot_longer(a1b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 4)
  
  ind_prev_fall_succ<-ind_prev_fall_succ %>% mutate_if(is.numeric, ~round(., 4))
  ind_prev_fall_succ<-as.data.frame(ind_prev_fall_succ)
  x <- cbind("gind_succ_prev_fall")
  ind_prev_fall_succ <- cbind(ind_prev_fall_succ,x)
  xtable(ind_prev_fall_succ, digits = 4)
  
  #density indirect effect on success
  post <- 
    posterior_samples(mod_std) %>% 
    mutate(a1 = b_springstd2sd_winter_dens_std_2_sd,
           b1 = b_successcent_spring_std_2_sd/4,
           c_prime =  b_successcent_winter_dens_std_2_sd/4)
  
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
  
  ind_winter_dens_succ <-post %>% 
    pivot_longer(c_prime:total_effect) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 5)
  
  ind_winter_dens_succ<-ind_winter_dens_succ %>% mutate_if(is.numeric, ~round(., 5))
  ind_winter_dens_succ<-as.data.frame(ind_winter_dens_succ)
  x <- cbind(c("find_succ_winter_dens", "edir_succ_winter_dens", "dtotal_succ_winter_dens"))
  ind_winter_dens_succ <- cbind(ind_winter_dens_succ,x)
  xtable(ind_winter_dens_succ, digits = 4)
  
  #max snow indirect effect on success
  post <- 
    posterior_samples(mod_std) %>% 
    mutate(a1 = b_springstd2sd_max_snow_std_2_sd,
           b1 = b_successcent_spring_std_2_sd/4,
           c_prime =  b_successcent_max_snow_std_2_sd/4)
  
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
  
  ind_snow_succ <-post %>% 
    pivot_longer(c_prime:total_effect) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 5)
  
  ind_snow_succ<-ind_snow_succ %>% mutate_if(is.numeric, ~round(., 5))
  ind_snow_succ<-as.data.frame(ind_snow_succ)
  x <- cbind(c("cind_succ_snow", "bdir_succ_snow", "atotal_succ_snow"))
  ind_snow_succ <- cbind(ind_snow_succ,x)
  xtable(ind_snow_succ, digits = 4)
  
  #Dot and 
  Predictor <- rbind("Autumn BM(t-1)", "Population density winter(t)","Population density winter(t)", "Population density winter(t)", "Snow depth(t)", "Snow depth(t)", "Snow depth(t)")
  indirect <- rbind(ind_prev_fall_succ, ind_winter_dens_succ, ind_snow_succ)
  Path <- rbind("mediated_by_spring", "mediated_by_spring",
                "bdirect","atotal","mediated_by_spring",
                "bdirect","atotal")
  indirect <- cbind(indirect, Predictor, Path)
  
  neworder <- c("Autumn BM(t-1)", "Population density winter(t)","Snow depth(t)")
  library(plyr)  ## or dplyr (transform -> mutate)
  indirect <- arrange(transform(indirect,
                 Predictor = factor(Predictor, levels = neworder)),Predictor)
  
  indirect_plot_success <-  
    ggplot(indirect, aes(value, Path)) +
    theme_gray(base_size = 10) + geom_vline(xintercept = 0) +
    theme(        axis.ticks.y=element_blank()#,strip.text.x = element_blank()
                  ) +
    geom_pointrange(aes(xmin = .lower, xmax = .upper),color = "deepskyblue4", position = position_dodge2(width = 1)) +
    labs(title = "Relative effects on the probability of reproductive success") +
    facet_wrap( ~Predictor,  ) +
    scale_x_continuous(name ="Effect size",
                       limits=c(-0.85,0.55), n.breaks = 5) +
    scale_y_discrete( name = NULL, labels = 
                        c( "mediated_by_spring"             = "Indirect effect mediated by spring BM",
                           "bdirect"                         = "Direct effect",
                           "atotal"                          = "Total effect"
                        )) +
    theme(strip.text = element_text(size= 10),
          axis.title.x = element_text(vjust = 0, size= 12),
          axis.text.x = element_text(size= 12, hjust = 0.5),
          axis.text.y = element_text(size= 12),
          text = element_text(family = "sans")) 
  indirect_plot_success

  
#absolute
  #prev fall success
  post <- 
    posterior_samples(mod_cent) %>% 
    mutate(a1 = b_spring_prev_fall_cent,
           b1 = b_success_spring_cent/4)
  post %>% 
    pivot_longer(a1:b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    mutate_if(is_double, round, digits = 4)
  
  post <-
    post %>%  
    mutate(a1b1    = a1 * b1)
  
  post %>% 
    pivot_longer(a1b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 4)
  
  ind_prev_fall_succ <-post %>% 
    pivot_longer(a1b1) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 4)
  
  ind_prev_fall_succ<-ind_prev_fall_succ %>% mutate_if(is.numeric, ~round(., 4))
  ind_prev_fall_succ<-as.data.frame(ind_prev_fall_succ)
  x <- cbind("gind_succ_prev_fall")
  ind_prev_fall_succ <- cbind(ind_prev_fall_succ,x)
  xtable(ind_prev_fall_succ, digits = 4)
  
  #density indirect effect on success
  post <- 
    posterior_samples(mod_cent) %>% 
    mutate(a1 = b_spring_winter_dens_cent,
           b1 = b_success_spring_cent/4,
           c_prime =  b_success_winter_dens_cent/4)
  
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
  
  ind_winter_dens_succ <-post %>% 
    pivot_longer(c_prime:total_effect) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 5)
  
  ind_winter_dens_succ<-ind_winter_dens_succ %>% mutate_if(is.numeric, ~round(., 5))
  ind_winter_dens_succ<-as.data.frame(ind_winter_dens_succ)
  x <- cbind(c("find_succ_winter_dens", "edir_succ_winter_dens", "dtotal_succ_winter_dens"))
  ind_winter_dens_succ <- cbind(ind_winter_dens_succ,x)
  xtable(ind_winter_dens_succ, digits = 4)
  
  #max snow indirect effect on success
  post <- 
    posterior_samples(mod_cent) %>% 
    mutate(a1 = b_spring_max_snow_m_cent,
           b1 = b_success_spring_cent/4,
           c_prime =  b_success_max_snow_m_cent/4)
  
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
  
  ind_snow_succ <-post %>% 
    pivot_longer(c_prime:total_effect) %>% 
    group_by(name) %>% 
    mean_qi(value) %>% 
    select(name:.upper) %>% 
    mutate_if(is_double, round, digits = 5)
  
  ind_snow_succ<-ind_snow_succ %>% mutate_if(is.numeric, ~round(., 5))
  ind_snow_succ<-as.data.frame(ind_snow_succ)
  x <- cbind(c("cind_succ_snow", "bdir_succ_snow", "atotal_succ_snow"))
  ind_snow_succ <- cbind(ind_snow_succ,x)
  xtable(ind_snow_succ, digits = 4)
  
  #Dot and 
  Predictor <- rbind("Autumn BM(t-1) [kg]", "Pop. dens. winter(t) [ind. /sq.km]","Pop. dens. winter(t) [ind. /sq.km]", "Pop. dens. winter(t) [ind. /sq.km]", "Snow depth(t) [m]", "Snow depth(t) [m]", "Snow depth(t) [m]")
  indirect <- rbind(ind_prev_fall_succ, ind_winter_dens_succ, ind_snow_succ)
  Path <- rbind("mediated_by_spring", "mediated_by_spring",
                "bdirect","atotal","mediated_by_spring",
                "bdirect","atotal")
  indirect <- cbind(indirect, Predictor, Path)
  
  neworder <- c("Autumn BM(t-1) [kg]", "Pop. dens. winter(t) [ind. /sq.km]","Snow depth(t) [m]")
  library(plyr)  ## or dplyr (transform -> mutate)
  indirect <- arrange(transform(indirect,
                                Predictor = factor(Predictor, levels = neworder)),Predictor)
  
  indirect_plot_success_abs <-  
    ggplot(indirect, aes(value, Path)) +
    theme_gray(base_size = 10) + geom_vline(xintercept = 0) +
    theme(        axis.ticks.y=element_blank()#,strip.text.x = element_blank()
    ) +
    geom_pointrange(aes(xmin = .lower, xmax = .upper),color = "deepskyblue4", position = position_dodge2(width = 1)) +
    labs(title = "Absolute effects on the probability of  reproductive success", size = 12) +
    facet_wrap( ~Predictor ) +
    scale_x_continuous(name ="Effect size", 
                       limits=c(-0.55,0.33), breaks = c(-0.4, -0.2, 0, 0.2)) +
    scale_y_discrete( name = NULL, labels = 
                        c( "mediated_by_spring"             = "Indirect effect mediated by spring BM",
                           "bdirect"                         = "Direct effect",
                           "atotal"                          = "Total effect"
                        )) +
    theme(strip.text = element_text(size= 10),
          axis.title.x = element_text(vjust = 0, size= 12),
          axis.text.x = element_text(size= 12, hjust = 0.5),
          axis.text.y = element_text(size= 12),
          text = element_text(family = "sans")) 
  indirect_plot_success_abs
  
  