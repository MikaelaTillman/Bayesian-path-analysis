setwd("C:/Users/ann.tillman/OneDrive - NINA/R-directory/Paper_1/final_R/git_hub/01_data")
load("mod_std.RData")
load("mod_cent.RData")

#pacman::p_load(brms, dplyr, tidyverse, piecewiseSEM, bayesplot, loo, lme4, rstanarm, multcompView, Matrix)

library(piecewiseSEM)
library(lme4)
library(brms)
library(tidyverse)
data <- data%>%
  purrr::modify_at(c("age"), factor) #need to factorise age again when R restarted

#checking conditional independencies follwing Shipley
psem_2_sd = psem(
  lmer(spring_std_2_sd ~ prev_fall_std_2_sd + max_snow_std_2_sd + winter_dens_std_2_sd + age + (1|individ_dbid) + (1|yr), na.action = na.omit, 
        data = data),
  lmer(fall_std_2_sd ~ prev_fall_std_2_sd + max_snow_std_2_sd +  summer_dens_std_2_sd  + spring_std_2_sd  + spr_std_2_sd + max_std_2_sd  + success  + age +(1|individ_dbid)+(1|yr), na.action = na.omit, 
        data = data),
  glmer(success ~ spring_std_2_sd + max_snow_std_2_sd + max_std_2_sd + spr_std_2_sd + winter_dens_std_2_sd + age +(1|individ_dbid)+(1|yr), family=binomial(link = "logit"),
        na.action = na.omit, 
        data = data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
  ))

piecewiseSEM::dSep(psem_2_sd, .progressBar = FALSE)
fisherC(psem_2_sd)
summary(psem_2_sd, intercept=TRUE)

#mod std 2 sd logit
#running standardised model in brms, for relative results 
spring_std <- bf(spring_std_2_sd ~ prev_fall_std_2_sd + winter_dens_std_2_sd + max_snow_std_2_sd + age  +(1|individ_dbid)+(1|yr))
success_logit <- bf(success_cent ~  winter_dens_std_2_sd + max_snow_std_2_sd + spring_std_2_sd + spr_std_2_sd + max_std_2_sd  +  age  + (1|individ_dbid)+(1|yr), family = bernoulli(link="logit"))
fall_std <- bf(fall_std_2_sd ~  prev_fall_std_2_sd + summer_dens_std_2_sd + max_snow_std_2_sd +  spring_std_2_sd  + spr_std_2_sd +  max_std_2_sd + success_cent +  age +(1|individ_dbid)+(1|yr))

mod_std <- brm(spring_std +
                       success_logit+
                       fall_std +
                       set_rescor(FALSE), 
                     data = data,
                     cores = 4, chains = 4, iter = 10000)
save(mod_std, file = "mod_std.Rdata")
loo_mod_std_2_sd.logit  <- loo(mod_std_2_sd.logit) #checking loo
fixef(mod_std_2_sd.logit)
loo_k_fold_std <- kfold(mod_std_2_sd.logit, K=10) #checking loo kfold, similar results
loo_k_fold_std
pareto_k_ids(loo_k_fold_std, threshold = 0.7) #finding id for Pareto K >  0.7, see script 03_diagnostics.R for more info

#running centered model in brms, for absolute results 
spring_cent <- bf(spring ~ prev_fall_cent + winter_dens_cent + max_snow_m_cent + age  +(1|individ_dbid)+(1|yr))
success_logit_cent <- bf(success ~  winter_dens_cent + max_snow_m_cent + spring_cent + spr_ten_cent + max_hundred_cent  +  age  + (1|individ_dbid)+(1|yr), family = bernoulli(link="logit"))
fall_cent <- bf(fall ~  prev_fall_cent + spring_cent  +  summer_dens_cent + max_snow_m_cent + spr_ten_cent +  max_hundred_cent + success_cent +  age +(1|individ_dbid)+(1|yr))

mod_cent <- brm(spring_cent +
                        success_logit_cent+
                        fall_cent +
                        set_rescor(FALSE), 
                      data=data,
                      cores=4, chains = 4, iter =10000)
save(mod_cent, file = "mod_cent.RData")

loo_cent<- loo(mod_cent.logit)
pareto_k_ids(loo_cent, threshold = 0.7)
save(mod_cent.logit, file = "mod_cent.RData")


#mod original values
#running model original values in brms, for partial residua plots
spring <- bf(spring ~ prev_fall + winter_dens  + max_snow_m  + age  +(1|individ_dbid)+(1|yr))
success_logit <- bf(success ~  winter_dens  + max_snow_m  + spring  + spr  + max_hundred   +  age  + (1|individ_dbid)+(1|yr), family = bernoulli(link="logit"))
fall <- bf(fall ~  prev_fall  + spring   +  summer_dens  + max_snow_m  + spr  +  max_hundred  + success  +  age +(1|individ_dbid)+(1|yr))

mod_org <- brm(spring +
                        success_logit +
                        fall +
                        set_rescor(FALSE), 
                      data=data,
                      cores=4, chains = 4, iter =10000)
save(mod_org, file = "mod_org.RData")
loo_org <- loo(mod_org.logit)
pareto_k_ids(loo_org, threshold = 0.7)
print(mod_org.logit)
pareto_k_ids(loo_mod_std_2_sd.logit, threshold = 0.7)
save(mod_org.logit, file = "mod_org.RData")


#get residuals used for partial resuduals plot
res_std <- residuals(mod_std, method = "posterior_predict")
res_cent <- residuals(mod_cent, method = "posterior_predict")
res_org <- residuals(mod_org, method = "posterior_predict")

a <- fixef(mod_cent)
a <- as.data.frame(a)
a <- a %>% 
  select(!Est.Error) %>% 
  mutate(marg.eff = Estimate/4) %>% 
  mutate(marg.lower = Q2.5/4) %>% 
  mutate(marg.upper = Q97.5/4) %>% 
  round(digits = 2)
View(a)  
