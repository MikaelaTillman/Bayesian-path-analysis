library(bayesplot)
est_spring <-  mcmc_areas(mod_std,
                          #point_est = "mean",
                          prob = 0.95,
                          prob_outer = 1,
                          pars = c("b_springstd2sd_winter_dens_std_2_sd",
                            "b_springstd2sd_max_snow_std_2_sd",
                            "b_springstd2sd_prev_fall_std_2_sd")) + 
  geom_vline(xintercept = 0) +
  ggtitle("Relative effects") + 
  scale_x_continuous(breaks = c(-1, -.5, 0, 0.5, 1)) +
  scale_y_discrete(name=,labels = 
                     c( "b_springstd2sd_prev_fall_std_2_sd" = bquote(Autumn~body~mass[t-1]),
                        "b_springstd2sd_max_snow_std_2_sd" = bquote(Snow~depth[t]),
                        "b_springstd2sd_winter_dens_std_2_sd" =  bquote(Population~density~winter[t]))) +
  labs(y = bquote(Spring~body~mass[t])) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans"))
est_spring

est_success <-  mcmc_areas(mod_std,
                           point_est = "mean",
                           prob = 0.95,
                           prob_outer = 1,
                           pars = c(   "b_successcent_winter_dens_std_2_sd",
                                       "b_successcent_spr_std_2_sd",
                                       "b_successcent_max_snow_std_2_sd",
                                       "b_successcent_max_std_2_sd",
                                       "b_successcent_spring_std_2_sd")) + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = 
                     c("b_successcent_spring_std_2_sd" =  bquote(Spring~body~mass[t]),
                       "b_successcent_spr_std_2_sd" = bquote(Spring~onset[t]),
                       "b_successcent_max_std_2_sd" =bquote(Plant~productivity[t]),
                       "b_successcent_max_snow_std_2_sd" =  bquote(Snow~depth[t]),
                   "b_successcent_winter_dens_std_2_sd" = bquote(Population~density~winter[t]))) +
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4),
                     labels = c("-1", "-0.5", 
                                "0", "0.5", 
                                "1")) +
  labs(y = bquote(Reproductive~success[t])) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans"))
est_success

est_fall <-  mcmc_areas(mod_std,
                        point_est = "mean",
                        prob = 0.95,
                        prob_outer = 1,
                        pars = c("b_fallstd2sd_success_cent",
                                 "b_fallstd2sd_summer_dens_std_2_sd",
                                 "b_fallstd2sd_spr_std_2_sd",
                                 "b_fallstd2sd_max_std_2_sd",
                                 "b_fallstd2sd_max_snow_std_2_sd",
                                 "b_fallstd2sd_spring_std_2_sd",
                                 "b_fallstd2sd_prev_fall_std_2_sd")) + 
  geom_vline(xintercept = 0) +
  xlim(-4,4) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)#, labels = c("-1", "-0.5", 
                                                               # "0", "0.5", 
                                                                #"1")
) +
  scale_y_discrete(labels = 
                     c( "b_fallstd2sd_prev_fall_std_2_sd" = bquote(Autumn~body~mass[t-1]),
                        "b_fallstd2sd_spring_std_2_sd" =  bquote(Spring~body~mass[t]),
                        "b_fallstd2sd_max_snow_std_2_sd" =  bquote(Snow~depth[t]),
                        "b_fallstd2sd_max_std_2_sd" =  bquote(Plant~productivity[t]),
                        "b_fallstd2sd_spr_std_2_sd" =  bquote(Spring~onset[t]),
                        "b_fallstd2sd_success_cent" =  bquote(Reproductive~success[t]),
                        "b_fallstd2sd_summer_dens_std_2_sd" = bquote(Population~density~summer[t]))) +
  labs(y = bquote(Autumn~body~mass[t])) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans"))
est_fall

#absolute effects
est_spring_abs <-  mcmc_areas(mod_cent,
                              #point_est = "mean",
                              prob = 0.95,
                              prob_outer = 1,
                              pars = c("b_spring_winter_dens_cent",
                                       "b_spring_max_snow_m_cent",
                                "b_spring_prev_fall_cent" )) + 
  geom_vline(xintercept = 0) +
  ggtitle("Absolute effects") + 
  scale_y_discrete(name= NULL,labels = 
                     c( "b_spring_prev_fall_cent" = bquote(Autumn~body~mass[t-1]~(kg)),
                        "b_spring_max_snow_m_cent" = bquote(Snow~depth[t]~(m)),
                        "b_spring_winter_dens_cent" = bquote(Population~density~winter[t]*~(ind./km^2)))) +
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans"))
est_spring_abs

est_success_abs <-  mcmc_areas(mod_cent,
                               point_est = "mean",
                               prob = 0.95,
                               prob_outer = 1,
                               pars = c( "b_success_winter_dens_cent",
                                "b_success_spr_ten_cent",
                                "b_success_max_snow_m_cent",
                                "b_success_max_hundred_cent",
                                 "b_success_spring_cent" )) + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(name= NULL,labels = 
                     c(  "b_success_spring_cent" = bquote(Spring~body~mass[t]~(kg)),
                         "b_success_spr_ten_cent" = bquote(Spring~onset[t]~(ten~days)),
                         "b_success_max_hundred_cent" = bquote(Plant~productivity[t]~(EVI)),
                         "b_success_max_snow_m_cent" =  bquote(Snow~depth[t]~(m)),
                         "b_success_winter_dens_cent" =    bquote(Population~density~winter[t]*~(ind./km^2)))) +

  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4),
                     labels = c("-1", "-0.5", 
                                "0", "0.5", 
                                "1")) +
    theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 4, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans")) 
est_success_abs


est_fall_abs <-  mcmc_areas(mod_cent,
                            point_est = "mean",
                            prob = 0.95,
                            prob_outer = 1,
                            pars = c("b_fall_success_cent",
                                     "b_fall_summer_dens_cent",
                                     "b_fall_spr_ten_cent",
                                     "b_fall_max_hundred_cent",
                                     "b_fall_max_snow_m_cent",
                                     "b_fall_spring_cent",
                                    "b_fall_prev_fall_cent")) + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(name= NULL,labels = 
                     c( "b_fall_prev_fall_cent" =   bquote(Autumn~body~mass[t-1]~(kg)),
                        "b_fall_spring_cent" =  bquote(Spring~body~mass[t]~(kg)),
                        "b_fall_spr_ten_cent" =  bquote(Spring~onset[t]~(ten~days)),
                        "b_fall_max_hundred_cent" =  bquote(Plant~productivity[t]~(EVI)),
                        "b_fall_max_snow_m_cent" =  bquote(Snow~depth[t]~(m)),
                        "b_fall_success_cent" =  bquote(Reproductive~success[t]),
                        "b_fall_summer_dens_cent" =    bquote(Population~density~summer[t]*~(ind./km^2)))) + 
  theme(axis.title.x = element_text(vjust = 0, size = 12),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "sans"))

est_fall_abs

