stepsize2<-0.1
library(ggdist)
library(data.table)
library(dplyr)
data_count <- data %>% #to make size of dots in graph
  group_by( success, yr) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(yr) %>% 
  mutate(freq = n / sum(n)) %>% 
  filter(success == 1) %>% 
  select(yr, freq)

mean_data <- data %>% 
  ungroup() %>% 
  group_by(yr) %>% 
  select(yr, prev_fall, spring, fall, success, winter_dens, max_snow_m, spr, max_hundred) %>% 
  mutate(mean_prev_fall = mean(prev_fall)) %>% 
  mutate(mean_spring = mean(spring)) %>%
  mutate(mean_fall = mean(fall)) %>% 
  distinct(yr, mean_prev_fall, mean_spring, mean_fall, winter_dens, max_snow_m, spr, max_hundred) 

mean_data2 <- left_join(mean_data, data_count, join_by("yr"))

#succes winter_dens
newdata <- data.frame(winter_dens = seq(1.076087,3.82-stepsize2, by=stepsize2),
                      prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                      spring = mean(data$spring) , spr = mean(data$spr), summer_dens = mean(data$summer_dens),
                      max_hundred = mean(data$max_hundred), success = 1
                      ,  age = 5, individ_dbid = 8730, yr = 2013)
low.quantile<-function(x){quantile(x, probs=0.025)}
upper.quantile<-function(x){quantile(x, probs=0.975)}

# note the use of posterior_epred, gives estimates for the mean...
pred.line <- posterior_epred(mod_org, newdata, re_formula = NA, resp="success")
pred.line.mean <- colMeans(pred.line)
est.pred.line.qlow <- apply(pred.line, MARGIN = 2, FUN = low.quantile)
est.pred.line.qhigh <- apply(pred.line, MARGIN = 2, FUN = upper.quantile)
pred.l <- data.frame(cbind(pred.line.mean, est.pred.line.qlow, est.pred.line.qhigh), winter_dens = newdata$winter_dens)

p <- ggplot(mean_data2, aes(x = winter_dens, y = freq)) 
p <- p + geom_dots(inherit.aes = FALSE, data = data, 
                   aes(y = as.numeric(success == "1"), x = winter_dens,
                       side = ifelse(success == "1", "bottom", "top"), 
                       scale = 0.3),  stackdir = "down")
p <- p + geom_point(aes(size = freq), color = "deepskyblue4", alpha = 1, show.legend = FALSE)+ 
  scale_size(range = c(.1, 4))
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred.l, 
                     aes(x = winter_dens, ymin = est.pred.line.qlow, ymax = est.pred.line.qhigh),
                     fill = "deepskyblue4", alpha = 0.2)
p <- p +geom_line(inherit.aes = FALSE , data = pred.l, 
                  aes(x = winter_dens, y = pred.line.mean), col = "red", size= 0.7)
p <- p +   labs(y = bquote(Reproductive~success[t]), x = bquote(Population~density~winter[t]*~(ind./km^2))) 
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 2, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
succ_winter_dens_plot <- p
succ_winter_dens_plot

newdata <- data.frame(max_snow_m = seq(0.66,3.4-stepsize2, by = stepsize2),
           prev_fall = mean(data$prev_fall), winter_dens = mean(data$winter_dens), summer_dens = mean(data$summer_dens),
           spring = mean(data$spring) , spr = mean(data$spr), 
           max_hundred = mean(data$max_hundred), success = 1
           ,  age = 5, individ_dbid = 8730, yr = 2013)                    
                                          
low.quantile<-function(x){quantile(x, probs=0.025)}
upper.quantile<-function(x){quantile(x, probs=0.975)}

# note the use of posterior_epred, gives estimates for the mean...
pred.line <- posterior_epred(mod_org, newdata, re_formula = NA, resp="success")
pred.line.mean <- colMeans(pred.line)
est.pred.line.qlow <- apply(pred.line, MARGIN = 2, FUN = low.quantile)
est.pred.line.qhigh <- apply(pred.line, MARGIN = 2, FUN = upper.quantile)
pred.l <- data.frame(cbind(pred.line.mean, est.pred.line.qlow, est.pred.line.qhigh), max_snow_m = newdata$max_snow_m)

p <- ggplot(mean_data2, aes(x = max_snow_m, y = freq)) 
p <- p + geom_dots(inherit.aes = FALSE, data = data, 
                   aes(y = as.numeric(success == "1"), x = max_snow_m,
                       side = ifelse(success == "1", "bottom", "top"), 
                       scale = 0.3),  stackdir = "down")
p <- p + geom_point(aes(size = freq), color = "deepskyblue4", alpha = 1, show.legend = FALSE)+ 
  scale_size(range = c(.1, 4))
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred.l, 
                     aes(x = max_snow_m, ymin = est.pred.line.qlow, ymax = est.pred.line.qhigh),
                     fill = "deepskyblue4", alpha = 0.2)
p <- p +geom_line(inherit.aes = FALSE , data = pred.l, 
                  aes(x = max_snow_m, y = pred.line.mean), col = "red", size= 0.7)
p <- p +   labs(y = "", x = bquote(Snow~depth[t]~(m)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
succ_snow_plot <- p
succ_snow_plot

#succ spring
c(40, 45, 50, 55, 60, 65, 70, 75, 80,85, 90, 95)
succ_spring <- table(data$success,cut(data$spring, breaks = 18 ))
succ_spring <- as.data.frame(succ_spring)
colnames(succ_spring)<-c("success","spring","freq")
setDT(succ_spring)
succ_spring[, rate := prop.table(freq), by = list(spring)]
succ_spring<- succ_spring %>% 
  filter(success != "0")
succ_spring
x <- c(41.4, 44.3, 47.15, 50, 52.85, 55.7, 58.55, 61.4, 64.25, 67.15, 70, 72.85, 75,70, 78.55, 81.4, 84.25, 87.15)
succ_spring <- cbind(succ_spring,x)
succ_spring <- succ_spring %>% 
  select(-c(spring))
colnames(succ_spring) <- c("success", "freq", "rate", "spring")
succ_spring

newdata <- data.frame(spring = seq(40,91.4-stepsize2, by=stepsize2),
                      prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                      winter_dens = mean(data$winter_dens), summer_dens = mean(data$summer_dens),spr = mean(data$spr), 
                      max_hundred = mean(data$max_hundred), success = 1
                      ,  age = 5, individ_dbid = 8730, yr = 2013)

low.quantile<-function(x){quantile(x, probs=0.025)}
upper.quantile<-function(x){quantile(x, probs=0.975)}

# note the use of posterior_epred, gives estimates for the mean...
pred.line <- posterior_epred(mod_org, newdata, re_formula = NA, resp="success")
pred.line.mean <- colMeans(pred.line)
est.pred.line.qlow <- apply(pred.line, MARGIN = 2, FUN = low.quantile)
est.pred.line.qhigh <- apply(pred.line, MARGIN = 2, FUN = upper.quantile)
pred.l <- data.frame(cbind(pred.line.mean, est.pred.line.qlow, est.pred.line.qhigh), spring = newdata$spring)

p <- ggplot(succ_spring, aes(x = spring, y = rate))
p <- p + geom_dots(inherit.aes = FALSE, data = data, 
                   aes(y = as.numeric(success == "1"), x = spring,
                       side = ifelse(success == "1", "bottom", "top"), 
                       scale = 0.3),  stackdir = "down")
p <- p + geom_point(aes(size = freq), color = "deepskyblue4" , alpha = 1, show.legend = FALSE)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred.l, 
                     aes(x = spring, ymin = est.pred.line.qlow, ymax = est.pred.line.qhigh),
                     fill = "deepskyblue4", alpha = 0.2)
p <- p +geom_line(inherit.aes = FALSE , data = pred.l, 
                  aes(x = spring, y = pred.line.mean), col = "red", size= 0.7)
p <- p +   labs(y = "", x = bquote(Spring~body~mass[t]~(kg)))
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
succ_spring_plot <- p
succ_spring_plot

#succ spr
newdata <- data.frame(spr = seq(151.69,180.39-stepsize2, by=stepsize2),
                      prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                      spring = mean(data$spring) , max_hundred = mean(data$max_hundred), 
                      winter_dens = mean(data$winter_dens), summer_dens = mean(data$summer_dens), success = 1
                      ,  age = 5, individ_dbid = 8730, yr = 2013)

low.quantile<-function(x){quantile(x, probs=0.025)}
upper.quantile<-function(x){quantile(x, probs=0.975)}

# note the use of posterior_epred, gives estimates for the mean...
pred.line <- posterior_epred(mod_org, newdata, re_formula = NA, resp="success")
pred.line.mean <- colMeans(pred.line)
est.pred.line.qlow <- apply(pred.line, MARGIN = 2, FUN = low.quantile)
est.pred.line.qhigh <- apply(pred.line, MARGIN = 2, FUN = upper.quantile)
pred.l <- data.frame(cbind(pred.line.mean, est.pred.line.qlow, est.pred.line.qhigh), spr = newdata$spr)

p <- ggplot(mean_data2, aes(x = spr, y = freq)) +
  scale_size(range = c(.1, 4)) +
geom_dots(inherit.aes = FALSE, data = data, 
                   aes(y = as.numeric(success == "1"), x = spr,
                       side = ifelse(success == "1", "bottom", "top"), 
                       scale = 0.3),  stackdir = "down") 
  p <- p +   labs(y = bquote(Reproductive~success[t]), x = bquote(Spring~onset[t]~(date))) +
geom_point(aes(size = freq), color = "deepskyblue4", alpha = 1, show.legend = FALSE) +
geom_ribbon(inherit.aes = FALSE , data = pred.l, 
                     aes(x = spr, ymin = est.pred.line.qlow, ymax = est.pred.line.qhigh),
                     fill = "deepskyblue4", alpha = 0.2) +
geom_line(inherit.aes = FALSE , data = pred.l, 
                  aes(x = spr, y = pred.line.mean), col = "red", size= 0.7) +
scale_x_continuous(breaks = c(152, 159, 166, 173, 181),
                                labels = c("1/6", "8/6", 
                                           "15/6", "22/g", 
                                           "29/6"))+
theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 2, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
succ_spr_plot <- p
succ_spr_plot

#succ max
newdata <- data.frame(max_hundred = seq(25.2,37.2-stepsize2, by=stepsize2),
                      prev_fall = mean(data$prev_fall), max_snow_m = mean(data$max_snow_m),
                      spring = mean(data$spring) , spr = mean(data$spr), 
                      winter_dens = mean(data$winter_dens), summer_dens = mean(data$summer_dens), success = 1
                      ,  age = 5, individ_dbid = 8730, yr = 2013)

low.quantile<-function(x){quantile(x, probs=0.025)}
upper.quantile<-function(x){quantile(x, probs=0.975)}

# note the use of posterior_epred, gives estimates for the mean...
pred.line <- posterior_epred(mod_org, newdata, re_formula = NA, resp="success")
pred.line.mean <- colMeans(pred.line)
est.pred.line.qlow <- apply(pred.line, MARGIN = 2, FUN = low.quantile)
est.pred.line.qhigh <- apply(pred.line, MARGIN = 2, FUN = upper.quantile)
pred.l <- data.frame(cbind(pred.line.mean, est.pred.line.qlow, est.pred.line.qhigh), max_hundred=newdata$max_hundred)

p <- ggplot(mean_data2, aes(x = max_hundred, y = freq)) +
  scale_size(range = c(.1, 4))
p <- p + geom_dots(inherit.aes = FALSE, data = data, 
                   aes(y = as.numeric(success == "1"), x = max_hundred,
                       side = ifelse(success == "1", "bottom", "top"), 
                       scale = 0.3),  stackdir = "down")
  p <- p +   labs(y = "", x = bquote(Plant~productivity[t]~(EVI))) 
p <- p + geom_point(aes(size = freq), color = "deepskyblue4", alpha = 1, show.legend = FALSE)
p <- p + geom_ribbon(inherit.aes = FALSE , data = pred.l, 
                     aes(x = max_hundred, ymin = est.pred.line.qlow, ymax = est.pred.line.qhigh),
                     fill = "deepskyblue4", alpha = 0.2)
p <- p +geom_line(inherit.aes = FALSE , data = pred.l, 
                  aes(x = max_hundred, y = pred.line.mean), col = "red", size= 0.7)
p <- p + theme(axis.title.x = element_text(vjust = 0, size = 14),
               axis.title.y = element_text(vjust = 4, size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               text = element_text(family = "sans")) 
succ_max_plot <- p
succ_max_plot

