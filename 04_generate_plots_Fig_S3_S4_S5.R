library(boot)
library(ggdist)
###Simulations from model to check model performance
pp_pred <- posterior_predict(mod_org,ndraws = 50)
postpdata <- data%>%
  purrr::modify_at(c("age"), factor) #need to factorise age again when R restarted

#simulate from model, x variables centered
#S1
#spring
simul.spr <- data.frame(value = as.numeric(pp_pred[,,"spring"]),
                        simul = rep(1:50,times = 814), age= rep(data$age, each = 50), 
                        max_snow_m = rep(data$max_snow_m, each = 50),
                        winter_dens = rep(data$winter_dens, each = 50),
                        prev_fall = rep(data$prev_fall, each = 50))

p <- ggplot(simul.spr, aes(y = value, x = prev_fall)) + 
  geom_point(aes(col = value), size = 0.1, show.legend = FALSE, alpha = 0.1) 
p <- p + labs(y = bquote(Spring~body~mass[t]~(kg)), x = bquote(Autumn~body~mass[t-1]~(kg))) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
 # geom_line(stat = "summary", fun = "mean" )
#p <- p + geom_smooth(method = "lm", se = TRUE, size = 0.5, color="red")
spring_prev_fall_sim <-p
spring_prev_fall_sim

#spring winter_dens
p <- ggplot(simul.spr, aes(y = value, x = winter_dens)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
scale_x_continuous(breaks=seq(1, 5,by=0.5))
p <- p + labs(y = "", 
             x = bquote(Population~density~winter[t]*~(ind./km^2)))
scale_y_continuous(limits=c(45,95),breaks = c(50,60, 70, 80, 90))
p <- p +   scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5,4))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = FALSE,  size = 0.5, color="red")
spring_winter_dens_sim <- p
spring_winter_dens_sim

#spring snow
p <- ggplot(simul.spr, aes(y = value, x = max_snow_m)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
scale_y_continuous(breaks=seq(40,110,by=10))
p <- p + labs(y ="", x =   bquote(Snow~depth[t]~(m))) +
scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
spring_max_snow_sim <- p 
spring_max_snow_sim

#Fall simulations
simul.fall <- data.frame(value=as.numeric(pp_pred[,,"fall"]),
                         simul=rep(1:50,times = 814), age=rep(data$age,each=50),
                         prev_fall  =rep(data$prev_fall  , each=50),
                         summer_dens =rep(data$summer_dens , each=50),
                         max_snow_m =rep(data$max_snow_m , each=50),
                         spring   = rep(data$spring  , each=50), 
                         spr =rep(data$spr , each=50),                         
                         max_hundred =rep(data$max_hundred , each=50),
                         success  =rep(data$success  , each=50))

#S2
#fall prev fall
p <- ggplot(simul.fall, aes(y = value, x = prev_fall)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y = bquote(Autumn~body~mass[t]~(kg)), x = bquote(Autumn~body~mass[t-1]~(kg))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = FALSE,  size = 0.5, color="red")
fall_prev_fall_sim <- p
fall_prev_fall_sim

#fall summer_dens
p <- ggplot(simul.fall, aes(y = value, x = summer_dens)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y = "", 
              x = bquote(Population~density~summer[t]*~(ind./km^2)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
p <- p +   scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5,4))
fall_summer_dens_sim <- p
fall_summer_dens_sim 

#fall snow
p <- ggplot(simul.fall, aes(y = value, x = max_snow_m)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y ="", x =   bquote(Snow~depth[t]~(m))) 
p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
fall_snow_sim <- p
fall_snow_sim 

#fall spring
p <- ggplot(simul.fall, aes(y = value, x = spring)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) +
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y = bquote(Autumn~body~mass[t]~(kg)), x = bquote(Spring[t]~(kg))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
fall_spring_sim <- p
fall_spring_sim

#fall spring onset
p <- ggplot(simul.fall, aes(y = value, x = spr)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) + 
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y ="", x =   bquote(Spring~onset[t]~(date))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
p <- p +   scale_x_continuous(breaks = c(152, 159, 166, 173, 181),
                              labels = c("1/6", "8/6", 
                                         "15/6", "22/6", 
                                         "29/6"))
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
fall_spr_sim <- p
fall_spr_sim

#fall max
p <- ggplot(simul.fall, aes(y = value, x = max_hundred)) + 
  geom_point(size = 0.1, aes(color = value), show.legend = FALSE, alpha = 0.1) + 
  scale_y_continuous(breaks = c(40,60, 80, 100))
p <- p + labs(y ="", x =   bquote(Plant~productivity[t]~(EVI))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               text = element_text(family = "sans")) 
#p <- p + geom_smooth(method = "lm", se = TRUE,  size = 0.5, color="red")
fall_max_sim <- p
fall_max_sim

#S3 
#success
simul.success <- data.frame(value = as.numeric(pp_pred[,,"success"]),
                         simul = rep(1:50, times = 814), age = rep(data$age, each =50),
                         prev_fall  = rep(data$prev_fall , each = 50),
                         winter_dens = rep(data$winter_dens, each = 50),
                         max_snow_m = rep(data$max_snow_m, each = 50),
                         spring  = rep(data$spring , each = 50), 
                         spr = rep(data$spr, each = 50),                         
                         max_hundred = rep(data$max_hundred, each = 50),
                         fall  = rep(data$fall , each = 50))
#success spring
#simul.success <- simul.success %>% 
#  mutate(value_invlogit = invlogit(value))

p <- ggplot(simul.success, aes(y = value, x = spring)) +
  geom_dots(data = simul.success, aes(y = as.numeric(value == "1"), x = spring,
                             side = ifelse(value == "1", "bottom", "top"),
                             scale = 0.3), color = "deepskyblue4", alpha = 0.1) 
p <- p + labs(y = "" , x = bquote(Spring~body~mass[t]~(kg)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10))
p <- p + geom_smooth(method = "glm", method.args= list(family="binomial"), se = TRUE,  size = 0.5, color="red")
succ_spring_sim <- p
succ_spring_sim 

p <- ggplot(simul.success, aes(y = value, x =  winter_dens)) +
  geom_dots(data = simul.success, aes(y = as.numeric(value == "1"), x =  winter_dens,
                                      side = ifelse(value == "1", "bottom", "top"),
                                      scale = 0.3), color = "deepskyblue4", alpha = 0.1) 
p <- p + labs(y = bquote(Reproductive~success[t]), 
              x =  bquote(Population~density~winter[t]*~(ind./km^2))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10))
p <- p +   scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5,4))
p <- p + geom_smooth(method = "glm", method.args= list(family="binomial"), se = TRUE,  size = 0.5, color="red")
succ_winter_dens_sim <- p
succ_winter_dens_sim

p <- ggplot(simul.success, aes(y = value, x = max_snow_m)) +
  geom_dots(data = simul.success, aes(y = as.numeric(value == "1"), x = max_snow_m,
                                      side = ifelse(value == "1", "bottom", "top"),
                                      scale = 0.3), color = "deepskyblue4", alpha = 0.1) 

p <- p + labs(y = "", 
              x = bquote(Snow~depth[t]~(m)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10))
p <- p + geom_smooth(method = "glm", method.args= list(family="binomial"), se = TRUE,  size = 0.5, color="red")
succ_snow_sim <- p
succ_snow_sim

p <- ggplot(simul.success, aes(y = value, x = spr)) +
  geom_dots(data = simul.success, aes(y = as.numeric(value == "1"), x = spr,
                                      side = ifelse(value == "1", "bottom", "top"),
                                      scale = 0.3), color = "deepskyblue4", alpha = 0.1) 

p <- p + labs(y = bquote(Reproducitve~success[t]), 
              x = bquote(Spring~onset[t]~(date)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10))
p <- p +   scale_x_continuous(breaks = c(152, 159, 166, 173, 181),
                              labels = c("1/6", "8/6", 
                                         "15/6", "22/6", 
                                         "29/6"))
p <- p + geom_smooth(method = "glm", method.args= list(family="binomial"), se = TRUE,  size = 0.5, color="red")
succ_spr_sim <- p
succ_spr_sim

p <- ggplot(simul.success, aes(y = value, x = max_hundred)) +
  geom_dots(data = simul.success, aes(y = as.numeric(value == "1"), x = max_hundred,
                                      side = ifelse(value == "1", "bottom", "top"),
                                      scale = 0.3), color = "deepskyblue4") 
p <- p + labs(y = "", 
              x = bquote(Plant~productivity[t]~(EVI)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 10),
               axis.title.y = element_text(vjust = 2, size = 10),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10))
p <- p + geom_smooth(method = "glm", method.args= list(family="binomial"), se = TRUE,  size = 0.5, color="red")
succ_max_hundred_sim <- p
succ_max_hundred_sim

