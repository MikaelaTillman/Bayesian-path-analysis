jitter <- position_jitter(width = 0.008)

load("mod_org.RData")

#get residuals used for partial resuduals plot
res_org <- residuals(mod_org, method = "posterior_predict")

re<-ranef(mod_org) 
reID<-as.data.frame(re$individ_dbid) 
rownames(reID)[which.min(abs(reID[,1]))] #10594
rownames(reID)[which.min(abs(reID[,5]-0))] #12534
rownames(reID)[which.min(abs(reID[,9]-0))] #11200

reYR<-as.data.frame(re$yr)
rownames(reYR)[which.min(abs(reYR[,1]))] #spring2017
rownames(reYR)[which.min(abs(reYR[,5]-0))] #success 2003
rownames(reYR)[which.min(abs(reYR[,9]-0))] #fall 2013

#spring prev fall
newdata2 <- data.frame(prev_fall = data$prev_fall,
                                  winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                                  max_snow_m = mean(data$max_snow_m),
                                  spring = mean(data$spring) , spr = mean(data$spr), summer_dens = mean(data$summer_dens),
                                  max_hundred = mean(data$max_hundred), success = 1, fall = mean(data$fall)
                                  ,  age = 5, individ_dbid = 10594, yr =2017)
pred2 <- predict(mod_org, newdata2, re_formula = NA, response = spring)
data$part.res.spring <- pred2[,1,1] + res_org[,1,1]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = prev_fall, y =part.res.spring)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_point(size = 0.03, color = "deepskyblue4", show.legend = FALSE) 
p <- p + geom_line(data = pred2, aes(x = prev_fall, y = Estimate.spring), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = prev_fall, ymin = Q2.5.spring, ymax = Q97.5.spring, 
                         ),fill = "deepskyblue4", alpha = 0.2) +
labs( y =  expression(Springbody~mass[t]~(kg)),
x = bquote(Autumn~body~mass[t-1]~(kg))) 
p <- p + theme(axis.title.x = element_text(vjust =0, size = 14),
               axis.title.y = element_text(vjust = 2, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
spring_prev_fall_partial_plot <- p
spring_prev_fall_partial_plot

#spring winter_dens
newdata2 <- data.frame(winter_dens = data$winter_dens, summer_dens = mean(data$summer_dens),
                       prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       max_hundred = mean(data$max_hundred), success = 1, fall = mean(data$fall)
                       ,  age = 5, individ_dbid = 10594, yr =2017)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = spring)
data$part.res.spring <- pred2[,1,1] + res_org[,1,1]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = winter_dens, y =part.res.spring)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_jitter(size = 0.03, color = "deepskyblue4", show.legend = FALSE, position = jitter) 
p <- p + geom_line(data = pred2, aes(x = winter_dens, y = Estimate.spring), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = winter_dens, ymin = Q2.5.spring, ymax = Q97.5.spring, 
                     ),fill = "deepskyblue4", alpha = 0.2, xmax = 3.6, xmin = 1)
p <- p + labs(y =  "",
              x = bquote(Population~density~winter[t]*~(ind./km^2))) +
scale_y_continuous(limits=c(45,95),breaks = c(50, 60, 70, 80, 90))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
p <- p +   scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5,4))
spring_winter_dens_partial_plot <- p
spring_winter_dens_partial_plot

#spring max_snow_m
newdata2 <- data.frame(max_snow_m = data$max_snow_m,
                       prev_fall = mean(data$prev_fall),
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       max_hundred = mean(data$max_hundred), success = 1, fall = mean(data$fall)
                       ,  age = 5, individ_dbid = 10594, yr =2017)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = spring)
data$part.res.spring<-pred2[,1,1]+res_org[,1,1]
pred2 <- cbind(newdata2, pred2)


p <- ggplot(data, aes(x = max_snow_m, y =part.res.spring)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_jitter(size = 0.03, color = "deepskyblue4", show.legend = FALSE, position = jitter) 
p <- p + geom_line(data = pred2, aes(x = max_snow_m, y = Estimate.spring), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = max_snow_m, ymin = Q2.5.spring, ymax = Q97.5.spring, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y = "",
              x = bquote(Snow~depth[t]~(m))) +
  scale_y_continuous(limits=c(45,95),breaks = c(50,60, 70, 80, 90))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) +
  scale_y_continuous(limits=c(45,95),breaks = c(50,60, 70, 80, 90))
spring_snow_partial_plot <- p
spring_snow_partial_plot

#fall prev_fall
newdata2 <- data.frame(prev_fall = data$prev_fall,
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       max_snow_m = mean(data$max_snow_m),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       max_hundred = mean(data$max_hundred), success = 1
                       ,  age = 5, individ_dbid = 11200, yr = 2017)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = fall)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = prev_fall, y = part.res.fall)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_point(size = 0.03, color = "deepskyblue4", show.legend = FALSE) 
p <- p + geom_line(data = pred2, aes(x = prev_fall, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = prev_fall, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y =  bquote(Autumn~body~mass[t]~(kg)),
              x = bquote(Autumn~body~mass[t-1]~(kg))) + 
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
  p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 2, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
fall_prev_fall_partial_plot <- p
fall_prev_fall_partial_plot

#fall summer_dens
newdata2 <- data.frame(summer_dens = data$summer_dens, winter_dens = mean(data$winter_dens),
                       prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       max_hundred = mean(data$max_hundred), success = 1
                       ,  age = 5, individ_dbid = 11200, yr = 2013)

pred2 <- predict(mod_org, newdata2, re_formula = NA,  response = fall)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = summer_dens, y =part.res.fall)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_jitter(size = 0.03, color = "deepskyblue4", show.legend = FALSE, position = jitter) 
p <- p + geom_line(data = pred2, aes(x = summer_dens, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = summer_dens, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y =  "",
              x = bquote(Population~density~summer[t]*~(ind./km^2))) +   
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
p <- p +   scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5,4))
fall_summer_dens_partial_plot <- p
fall_summer_dens_partial_plot

#fall max_snow_m
newdata2 <- data.frame(max_snow_m = data$max_snow_m,
                       prev_fall = mean(data$prev_fall), 
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       max_hundred = mean(data$max_hundred), success = 1
                       ,  age = 5, individ_dbid = 11200, yr = 2013)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = fall)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = max_snow_m, y =part.res.fall)) +
  geom_jitter(size = 0.03, color = "deepskyblue4", show.legend = FALSE, position = jitter) 
p <- p + geom_line(data = pred2, aes(x = max_snow_m, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = max_snow_m, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y = "",
              x = bquote(Snow~depth[t]~(m))) +   
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
fall_max_snow_m_partial_plot <- p
fall_max_snow_m_partial_plot

#fall spring
newdata2 <- data.frame(spring = data$spring,
                       prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                       spr = mean(data$spr) ,
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       max_hundred = mean(data$max_hundred), success = 1
                       ,  age = 5, individ_dbid = 11200, yr = 2013)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = fall)
str(pred2)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = spring, y =part.res.fall)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_point(size = 0.03, color = "deepskyblue4", show.legend = FALSE) 
p <- p + geom_line(data = pred2, aes(x = spring, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = spring, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y = bquote(Autumn~body~mass[t]*~(kg)), 
              x = bquote(Spring~body~mass[t]*~(kg))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 2, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) +
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
fall_spring_partial_plot <- p
fall_spring_partial_plot

#fall spr
newdata2 <- data.frame(spr = data$spr,
                       prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                       spring = mean(data$spring) ,
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       max_hundred = mean(data$max_hundred), success = 1
                       ,  age = 5, individ_dbid = 11200, yr = 2013)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = fall)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = spr, y =part.res.fall)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_point(size = 0.03, color = "deepskyblue4", show.legend = FALSE) 
p <- p + geom_line(data = pred2, aes(x = spr, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = spr, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y = "",
              x = bquote(Spring~onset[t]*~(date))) +   
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
p <- p +   scale_x_continuous(breaks = c(152, 159, 166, 173, 181),
                              labels = c("1/6", "8/6", 
                                         "15/6", "22/6", 
                                         "29/6"))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
fall_spr_partial_plot <- p
fall_spr_partial_plot

#fall max_hundred
newdata2 <- data.frame(max_hundred = data$max_hundred,
                       prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                       spring = mean(data$spring) , spr = mean(data$spr), 
                       winter_dens = mean(data$winter_dens),summer_dens = mean(data$summer_dens),
                       success = 1,  age = 5, individ_dbid = 11200, yr = 2013)

pred2 <- predict(mod_org, newdata2, re_formula = NA, response = fall)
data$part.res.fall<-pred2[,1,3]+res_org[,1,3]
pred2 <- cbind(newdata2, pred2)

p <- ggplot(data, aes(x = max_hundred, y =part.res.fall)) +# stat_smooth(method="lm", se=FALSE) + #rawdata
  geom_point(size = 0.03, color = "deepskyblue4", show.legend = FALSE) 
p <- p + geom_line(data = pred2, aes(x = max_hundred, y = Estimate.fall), col = "red", size = 0.7)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred2, 
                     aes(x = max_hundred, ymin = Q2.5.fall, ymax = Q97.5.fall, 
                     ),fill = "deepskyblue4", alpha = 0.2)
p <- p + labs(y =  "",
              x = bquote(Plant~productivity[t]*~(EVI))) +   
  scale_y_continuous(limits=c(55,90),breaks = c(50,60, 70, 80, 90))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
fall_max_hundred_partial_plot <- p
fall_max_hundred_partial_plot

