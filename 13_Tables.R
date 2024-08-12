#Summary tables
data <- data %>% 
  mutate(summer_change = fall - spring) %>% 
  mutate(winter_change = spring-prev_fall)

summary_table<- data %>%
  select(spring, fall, success, prev_fall, summer_change, winter_change, dens,max_snow, spr, max) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()

summary_table <- 
  summary_table %>%
  select(var=rowname, min, max, mean, sd,range) %>%
  print()

summary_table<-summary_table %>% mutate_if(is.numeric, ~round(., 2))
summary_table<-as.data.frame(summary_table)

xtable(summary_table)

#summary table year
library(gapminder)
library(dplyr)

a <- data %>% 
  group_by(yr = ceiling(yr/1) * 1) %>% 
  summarize(yr= paste(first(yr)),
            success = mean(success)) %>%
  ungroup

est_mod_std.logit<-summary(mod_std.logit)$fixed #make a dataframe first
colnames(est_mod_std.logit)<-c("Estimate", "Est.Error","lower_CI" ,"upper_CI","Rhat", "Bulk_ESS","Tail_ESS")
est_mod_std.logit<-round(est_mod_std.logit, digits=5)
response = c("Spring", "Spring","Spring","Spring","Success","Success","Success","Success","Success","Success", "Fall","Fall","Fall","Fall","Fall","Fall","Fall","Fall")
est_mod_std.logit_spring_int <- est_mod_std.logit[1,]
est_mod_std.logit_spring <- est_mod_std.logit[4:6,]
est_mod_std.logit_succ_int <- est_mod_std.logit[2,]
est_mod_std.logit_succ <- est_mod_std.logit[14:18,]
est_mod_std.logit_fall_int <- est_mod_std.logit[3,]
est_mod_std.logit_fall <- est_mod_std.logit[26:32,]
est_mod_std.logit <- rbind(est_mod_std.logit_spring_int,est_mod_std.logit_spring,est_mod_std.logit_succ_int,est_mod_std.logit_succ,est_mod_std.logit_fall_int,est_mod_std.logit_fall)
est_mod_std.logit <- rownames_to_column(est_mod_std.logit, var = "rowname")
est_mod_std.logit <- cbind(response, est_mod_std.logit)

xtable(est_mod_std.logit)
names(est_mod_std.logit)

est_mod_cent.logit<-summary(mod_cent.logit)$fixed #make a dataframe first
colnames(est_mod_cent.logit)<-c("Estimate", "Est.Error","lower_CI" ,"upper_CI","Rhat", "Bulk_ESS","Tail_ESS")
names(est_mod_cent.logit)
est_mod_cent.logit<-round(est_mod_cent.logit, digits=4)
response = c("Spring", "Spring","Spring","Spring","Success","Success","Success","Success","Success","Success", "Fall","Fall","Fall","Fall","Fall","Fall","Fall","Fall")
est_mod_cent.logit_spring_int <- est_mod_cent.logit[1,]
est_mod_cent.logit_spring <- est_mod_cent.logit[4:6,]
est
est_mod_cent.logit_succ_int <- est_mod_cent.logit[2,]
est_mod_cent.logit_succ_int
est_mod_cent.logit_succ <- est_mod_cent.logit[14:18,]
est_mod_cent.logit_fall_int <- est_mod_cent.logit[3,]
est_mod_cent.logit_fall <- est_mod_cent.logit[26:32,]
est_mod_cent.logit <- rbind(est_mod_cent.logit_spring_int,est_mod_cent.logit_spring,est_mod_cent.logit_succ_int,est_mod_cent.logit_succ,est_mod_cent.logit_fall_int,est_mod_cent.logit_fall)
est_mod_cent.logit <- rownames_to_column(est_mod_cent.logit, var = "rowname")
est_mod_cent.logit <- cbind(response, est_mod_cent.logit)
xtable(est_mod_cent.logit_succ, digits = 0)
###partial derivatives, centered logit
est_mod_cent.logit2 <- est_mod_cent.logit %>% 
  mutate(est4 = Estimate/4)
est_mod_cent.logitest_mod_cent.logit_fall <- rbind(est_mod_cent.logit_fall_int, est_mod_cent.logit_fall)
est_mod_cent.logit_fall
xtable(est_mod_cent.logit_fall, digits = 4)
2 <- est_mod_cent.logit2 %>% 
  mutate(lci4 = lower_CI/4)
est_mod_cent.logit2 <- est_mod_cent.logit2 %>% 
  mutate(uci4 = upper_CI/4)
est_mod_cent.logit2

est_mod_cent.logit_spring <- rbind(est_mod_cent.logit_spring_int, est_mod_cent.logit_spring)
est_mod_cent.logit_spring
xtable(est_mod_cent.logit_spring, digits = 4)

est_mod_cent.logit_fall <- rbind(est_mod_cent.logit_fall_int, est_mod_cent.logit_fall)
est_mod_cent.logit_fall <- est_mod_cent.logit_fall %>% 
  select(Estimate, lower_CI, upper_CI, Rhat, Bulk_ESS, Tail_ESS)
xtable(est_mod_cent.logit_fall, digits = 0)

est_mod_cent.logit_success <- rbind(est_mod_cent.logit_succ_int, est_mod_cent.logit_succ)
est_mod_cent.logit_success

est_mod_cent.logit_success <- est_mod_cent.logit_success %>% 
  mutate(est4 = Estimate/4) %>% 
  mutate(lci4 = lower_CI/4) %>% 
  mutate(uci4 = upper_CI/4)
 
est_mod_cent.logit_success <- est_mod_cent.logit_success %>% 
  select(est4, lci4, uci4, Rhat, Bulk_ESS, Tail_ESS)
xtable(est_mod_cent.logit_success, digits = 4)
  
xtable(est_mod_cent.logit_fall, digits = 4)success <- est_mod_cent.logitsuccess %>% 
  mutate(lci4 = lower_CI/4)
est_mod_cent.logitsuccess <- est_mod_cent.logitsuccess %>% 
  mutate(uci4 = upper_CI/4)
est_mod_cent.logitsuccess


xtable(est_mod_cent.logit_success, digits = 4)

xtable(est_mod_cent.logit_spring, digits = 0)





est_mod_std.logit<-summary(mod_std_2_sd.logit)$fixed #make a dataframe first
est_mod_std.logit
colnames(est_mod_std.logit)<-c("Estimate", "Est.Error","lower_CI" ,"upper_CI","Rhat", "Bulk_ESS","Tail_ESS")
names(est_mod_std.logit)
est_mod_std.logit<-round(est_mod_std.logit, digits=4)
response = c("Spring", "Spring","Spring","Spring","Success","Success","Success","Success","Success","Success", "Fall","Fall","Fall","Fall","Fall","Fall","Fall","Fall")
est_mod_std.logit_spring_int <- est_mod_std.logit[1,]
est_mod_std.logit_spring <- est_mod_std.logit[4:6,]
est_mod_cent.logit_spring_age <- est_mod_cent.logit[7:13,]
est_mod_cent.logit_spring_age <- est_mod_cent.logit[7:13,]

xtable(est_mod_cent.logit_spring_age, digits = 0)
est_mod_std.logit_succ_int <- est_mod_std.logit[2,]
est_mod_std.logit_succ <- est_mod_std.logit[14:18,]
xtable(est_mod_std.logit_succ, digits = 0)
est_mod_cent.logit_succ_age <- est_mod_cent.logit[19:25,]
est_mod_cent.logit_succ_age <- est_mod_cent.logit[19:25,]
xtable(est_mod_cent.logit_succ_age, digits = 0)

xtable(est_mod_std.logit_succ_age, digits = 0)
est_mod_std.logit_fall_int <- est_mod_std.logit[3,]
est_mod_std.logit_fall <- est_mod_std.logit[26:32,]
est_mod_cent.logit_fall_age <- est_mod_cent.logit[33:41,]
xtable(est_mod_cent.logit_fall_age, digits = 0)
xtable(est_mod_std.logit_fall, digits = 0)
est_mod_std.logit <- rbind(est_mod_std.logit_spring_int,est_mod_std.logit_spring,est_mod_std.logit_succ_int,est_mod_std.logit_succ,est_mod_std.logit_fall_int,est_mod_std.logit_fall)
est_mod_std.logit <- rownames_to_column(est_mod_std.logit, var = "rowname")
est_mod_std.logit <- cbind(response, est_mod_std.logit)

###partial derivatives, stdered logit
est_mod_std.logit2 <- est_mod_std.logit %>% 
  mutate(est4 = Estimate/4)
est_mod_std.logitest_mod_std.logit_fall <- rbind(est_mod_std.logit_fall_int, est_mod_std.logit_fall)
est_mod_std.logit_fall
xtable(est_mod_std.logit_fall, digits = 4)
2 <- est_mod_std.logit2 %>% 
  mutate(lci4 = lower_CI/4)
est_mod_std.logit2 <- est_mod_std.logit2 %>% 
  mutate(uci4 = upper_CI/4)
est_mod_std.logit2

est_mod_std.logit_spring <- rbind(est_mod_std.logit_spring_int, est_mod_std.logit_spring)
est_mod_std.logit_spring
xtable(est_mod_std.logit_spring, digits = 0)
xtable(est_mod_std.logit_spring_age, digits = 0)

est_mod_std.logit_fall <- rbind(est_mod_std.logit_fall_int, est_mod_std.logit_fall)
est_mod_std.logit_fall <- est_mod_std.logit_fall %>% 
  select(Estimate, lower_CI, upper_CI, Rhat, Bulk_ESS, Tail_ESS)
xtable(est_mod_std.logit_fall, digits = 4)

est_mod_std.logit_success <- rbind(est_mod_std.logit_succ_int, est_mod_std.logit_succ)
est_mod_std.logit_success

est_mod_std.logit_success <- est_mod_std.logit_success %>% 
  mutate(est4 = Estimate/4) %>% 
  mutate(lci4 = lower_CI/4) %>% 
  mutate(uci4 = upper_CI/4)

est_mod_std.logit_success <- est_mod_std.logit_success %>% 
  select(est4, lci4, uci4, Rhat, Bulk_ESS, Tail_ESS)
xtable(est_mod_std.logit_success, digits = 4)

xtable(est_mod_std.logit_fall, digits = 4)success <- est_mod_std.logitsuccess %>% 
  mutate(lci4 = lower_CI/4)
est_mod_std.logitsuccess <- est_mod_std.logitsuccess %>% 
  mutate(uci4 = upper_CI/4)
est_mod_std.logitsuccess


xtable(est_mod_std.logit_success, digits = 4)
xtable(mod_ce)


