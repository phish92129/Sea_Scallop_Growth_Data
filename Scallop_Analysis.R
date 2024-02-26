# This code encompasses all analysis aspects and includes some graphics to support
# the model diagnostics and for showcasing decision process.  Load STE.R and run
# prior to this code (ie run this code second).


library(MuMIn)
library(glmmTMB)
library(lme4)
library(tidyr)
library(agricolae)
library(MASS)
library(viridis)
library(car)
library(merTools)
library(ggeffects)
# Analysis remove seed

A1 <- subset(STE.n.dat.t, Trial != 'Seed')

# Assess normal distribution visually
qqp(A1$LGR, "norm")

# Create global model
gamma.A1 <- glmmTMB(LGR ~  (1|Site) + (1|Cohort) + mean_sh * Trial * wgdd.mean,
                        family= gaussian, data = A1)

summary(gamma.A1)

# Run model selection process
MXPARAMS<-7 #limit max parameters to about 1:10 per vars:total N in dataset
options(na.action = "na.fail")
gamma.A1_dredgemm <- dredge(gamma.A1, rank="AIC",   
                            extra=alist(AIC))      #use beta="sd" if cont. vars are scaled    
write.csv(gamma.A1_dredgemm, file= "gammaA1_dredgemm.csv") #complete model selection output
gamma.A1_subset <- subset(gamma.A1_dredgemm, delta <2) #confidence set model output
write.csv(gamma.A1_subset, file="gammaA1_subset.csv")
RI_gamma.A1 <- sw(gamma.A1_subset) #outputs variable importance if >1 model in conf.set
print(RI_gamma.A1)

# Lowest AIC model
gamma.A1.top <- glmmTMB(LGR ~ (1|Site) + (1|Cohort) + (mean_sh + Trial)^2 + (wgdd.mean+Trial)^2,
                    family= gaussian(), data = A1)

# Summary for gamma.A1.top
summary(gamma.A1.top)

#Save model of best fit
saveRDS(gamma.A1.top,file = 'C:/Users/noren/OneDrive/Documents/R/Scallops/Bioec/Growth.rda')

#Visual assessment of residuals
plot(resid(gamma.A1.top))
qqnorm(resid(gamma.A1.top))


# Analysis 2 meat weights as a function of shell height (variable name A3)

A3 <- sw
A3$Month <- as.factor(A3$Month)
A3$Sh_Height <- as.numeric(A3$Sh_Height)

# Assess on various distributions
qqp(A3$Ww_Add, "norm")

# Attempted a gamma fit but visual inspection shows gaussian is more appropriate

gamma <- fitdistr(A3$Ww_Add, "gamma")
qqp(A3$Ww_Add, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

gamma.A3 <- glmmTMB(Ww_Add ~  (1|Site) + Sh_Height + Month + wgdd.mean + Trial,
                    family= gaussian(link=log), data = A3)

summary(gamma.A3)

MXPARAMS<-7 #limit max parameters to about 1:10 per vars:total N in dataset
options(na.action = "na.fail")
gamma.A3_dredgemm <- dredge(gamma.A3, rank="AICc",   
                            extra=alist(AICc))      #use beta="sd" if cont. vars are scaled    
write.csv(gamma.A3_dredgemm, file= "gammaA3_dredgemm.csv") #complete model selection output
gamma.A3_subset<- subset(gamma.A3_dredgemm, delta <2) #confidence set model output
write.csv(gamma.A3_subset, file="gammaA3_subset.csv")
RI_gamma.A3 <- sw(gamma.A3_subset) #outputs variable importance if >1 model in conf.set
print(RI_gamma.A3)

gamma.A3.top <- glmmTMB(Ww_Add ~  (1|Site) + Sh_Height + Month,
                    family= gaussian (link = 'log'), data = A3)

summary(gamma.A3.top)
plot(resid(gamma.A3.top))
qqnorm(resid(gamma.A3.top))



# Analysis 3 two step reconstructed growth model

# Note, this is a somewhat repetitive bit of code just due to the way I began 
# the process and can be simplified if desired.

# Generalized sine/cosine function for temperature to standardize 
# time over multiple years

temp.ma$day <- temp.ma$DT-as.Date('2021-08-04')
temp.ma$day <- as.numeric(temp.ma$day)
temp.maagg <- aggregate(data = temp.ma, Tempc~day, FUN = 'mean')

fit <- lm(Tempc ~ sin(2*pi/365*day)+cos(2*pi/365*day),data=temp.maagg)

plot(data = temp.maagg, Tempc~day)

# Predict data from 1-760 using day
d <- seq(1,760, by=1)

pr.temp <- predict(fit, newdata = data.frame(day = d),
                   type = "response")
plot(pr.temp~d)

# Apply weighted growing degree days to function using t.weighted
pr.temp <- sapply(pr.temp,t.weighted)

# Assign predicted weighted growth degree days to y for ear hanging trials
y<- pr.temp

populate_value.eh<- function(x){
  for(i in 2:length(x)){
    x[i] <- ((predict (gamma.A1.top, newdata = data.frame(mean_sh = x[i-1], 
                                                          wgdd.mean = y[i], 
                                                          Trial = 'Ear Hanging', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),
                       type = "response", allow.new.levels = TRUE)))+x[i-1]
  }
  x
}



## Notice I have changed it to data.frame
ret = data.frame(matrix(nrow = 760, ncol = 1))
colnames(ret) = 'x'

## TIterate starting at a projected size of 50 mm
pri = ret
pri[1,] = 50

d <- seq(0,759, by=1)                                  
predicted.eh <- data.frame(day = d)

predicted.eh$ShellHeight <- sapply(pri,populate_value.eh)
colnames(predicted.eh) <- c("day","Sh_Height")
predicted.eh$Trial <- 'Ear Hanging'

# Create a sequence of dates D starting at 08/04

D <- seq(as.Date('0002-08-04'), length = 760, by = "days")

predicted.eh$Date <- D
predicted.eh$Year <- as.numeric(format(predicted.eh$Date, '%y'))
predicted.eh$Month <- as.numeric(format(predicted.eh$Date, '%m'))
predicted.eh$yrDate <- as.yearmon(paste(predicted.eh$Year, 
                                        predicted.eh$Month), "%y %m")

predicted.eh$yrDatefac <- as.factor(predicted.eh$yrDate)
predicted.eh.sh <- predicted.eh

# Subset to sample months
predicted.eh <- subset(predicted.eh, Month == '5' | Month == '8' | Month == '11' | Month == '2')


######## Lantern net trials #####
# Note that code is replicated from ear hanging code and then merged at bottom,
# sloppy but it works

populate_value.ln<- function(x){
  for(i in 2:length(x)){
    x[i] <- ((predict (gamma.A1.top, newdata = data.frame(mean_sh = x[i-1], 
                                                          wgdd.mean = y[i], 
                                                          Trial = 'Lantern Net', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),
                       type = "response")))+x[i-1]
  }
  x
}


## Notice I have changed it to data.frame
ret = data.frame(matrix(nrow = 760, ncol = 1))
colnames(ret) = 'x'

## This makes the empty 30x10 dataframe where prices will go
pri = ret
pri[1,] = 50

predicted.ln <- data.frame(day = d)

predicted.ln$ShellHeight <- sapply(pri,populate_value.ln)

colnames(predicted.ln) <- c("day","Sh_Height")
predicted.ln$Trial <- 'Lantern Net'



predicted.ln$Date <- D

predicted.ln$Year <- as.numeric(format(predicted.ln$Date, '%y'))
predicted.ln$Month <- as.numeric(format(predicted.ln$Date, '%m'))
predicted.ln$yrDate <- as.yearmon(paste(predicted.ln$Year, 
                                        predicted.ln$Month), "%y %m")
predicted.ln$yrDatefac <- as.factor(predicted.ln$yrDate)

predicted.ln.sh <- predicted.ln
predicted.ln <- subset(predicted.ln, Month == '5' | Month == '8' | Month == '11' | Month == '2')

# Combine data sets

predicted <- rbind(predicted.eh,predicted.ln)
predicted.sh <- rbind(predicted.eh.sh,predicted.ln.sh)
predicted$Site <- 'NA'


# Predict adductor weights based on simulated shell heights
predicted$pred <- predict(gamma.A3.top,predicted,allow.new.levels=TRUE,
                          type = 'response')

p.sh <- aggregate(data = predicted, Sh_Height~Trial+yrDate, FUN = 'mean')
p.add <- aggregate(data = predicted, pred~Trial+yrDate, FUN = 'mean')

# Merge shell height and adductor
p.tot <- merge(p.sh,p.add)

p.tot <- mutate(p.tot, countlbs = 1.0/(0.00220462*pred))

# Confidence intervals

#Lower confidence adductor
ad_confint.low <- predicted[,-2] 
ad_confint.low$Sh_Height <- ad_confint.low$Lower.cum
ad_confint.low$Lower.cum.ad <- predict(gamma.A3.top,ad_confint.low,allow.new.levels=TRUE,
                          type = 'response')

pconfint.low.ad <- aggregate(data = ad_confint.low, Lower.cum.ad~Trial+yrDate, FUN = 'mean')

pconfint.low.ad <- mutate(pconfint.low.ad, countlbs.low = 1.0/(0.00220462*Lower.cum.ad))

# Upper confidence interval bound
ad_confint.up <- predicted[,-2] 
ad_confint.up$Sh_Height <- ad_confint.low$Upper.cum

ad_confint.up$Upper.cum.ad <- predict(gamma.A3.top,ad_confint.up,allow.new.levels=TRUE,
                          type = 'response')
pconfint.up.ad <- aggregate(data = ad_confint.up, Upper.cum.ad~Trial+yrDate, FUN = 'mean')

pconfint.up.ad <- mutate(pconfint.up.ad, countlbs.low = 1.0/(0.00220462*Upper.cum.ad))

# Join confidence intervals and shell heights

p.tot <- merge(p.tot, pconfint.low.ad, by = c("Trial","yrDate"))
p.tot <- merge(p.tot, pconfint.up.ad, by = c("Trial","yrDate"))


p.tot <- arrange(p.tot,yrDate)

perc.sh <- p.tot %>%
  mutate(perc.diff.sh = (abs((x-lag(x, default =first(x))))/((x+lag(x, default =first(x)))/2)*100))  

perc.add <- p.tot %>%
  mutate(perc.diff.add = (abs((pred-lag(pred, default =first(pred))))/((pred+lag(pred, default =first(pred)))/2)*100))  
  
perc.sh <- subset(perc.sh, Trial=='Lantern Net')
perc.add <- subset(perc.add, Trial=='Lantern Net')

perc <- merge(perc.sh,perc.add)

o.sh.change <- subset(STE.n.tble.dat, Trial != 'Seed')
o.sh.change$yrDate <- as.yearmon(paste(o.sh.change$Year, 
                                   o.sh.change$Month), "%y %m")

O.sh.change <- aggregate(o.sh.change, ShellHeight~yrDate+Cohort+Trial, FUN='mean')
O.sh.change <- O.sh.change %>%
  group_by (yrDate)

O.sh.change.eh <- subset(O.sh.change, Trial  == 'Ear Hanging')
O.sh.change.ln <- subset(O.sh.change, Trial == 'Lantern Net')

O.sh.change.eh$percent_change <- ifelse(O.sh.change.eh$Cohort == 'y1', 
                                        (O.sh.change.eh$ShellHeight - 59.387326)/59.387326, (O.sh.change.eh$ShellHeight - 64.32816)/64.32816)
O.sh.change.ln$percent_change <- ifelse(O.sh.change.ln$Cohort == 'y1', 
                                        (O.sh.change.ln$ShellHeight - 61.41667)/61.41667, (O.sh.change.eh$ShellHeight - 68.69917)/68.69917)

O.sh.change <- rbind(O.sh.change.eh,O.sh.change.ln)
O.sh.change$percent_change <- round(O.sh.change$percent_change,3)


library(scales)

# Example list of dates (replace with your actual list of dates)
date_list <- c("2024-06-20", "2023-10-15", "2025-01-01")

# Define the start date
start_date <- ymd("2018-10-01")  # Change this to your desired start date

# Calculate the year difference
year_difference <- difftime(ymd(date_list), start_date, units = "days")

year_rel <- year_difference + ymd("0000-10-01")

# Print the year difference
print(year_rel)

formatted_date <- format(year_rel, "%B %d, Year %_Y")

print(formatted_date[order(formatted_date)]) #should still work as a valid date format

########
min <- as.Date("0002-07-01")
max <- NA
mid <- as.Date("0004-02-01")
min2 <- as.Date("0003-03-01")
min3 <- as.Date("0003-04-01")
max2 <- as.Date("0004-09-01")

st <- ymd("0000-10-01")

P1 <- ggplot(predicted.sh, aes(y = Sh_Height))+ 
  geom_line(aes(x = Date, color = Trial), linewidth = .1) +
  geom_ribbon(aes(ymin=Lower.cum,ymax=Upper.cum, x=Date, group = Trial), alpha = .3) +
  annotate("rect", xmin = min3, xmax = max2, ymin = 90, ymax = 120,
           alpha = .1,fill = "blue")+
  annotate("text", x = mid, y=93,
           label="Adductor Harvest period")+
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab('Shell Height (mm)')+
  labs(color = "Trial")+
  scale_x_date(breaks = selected_dates, labels = selected_labels) +
  # scale_x_date(NULL,
  #              breaks = scales::breaks_width("3 months"),
  #              labels = date_format("%b\nyear %_Y"),
  #              limits = c(min,max))+
  scale_color_manual(values=c("#192841", "#F79256"))+
  theme(plot.title = element_blank (),
    strip.text.x = element_text(size=12, face="bold"), legend.position = 'none',
        strip.text.y = element_text(size=12, face="bold"),axis.title.x = element_blank(),
        legend.title = element_text(size=10, face="bold"), legend.text = element_text(size = 10),
        axis.title.y = element_text(face="bold", size=12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12,angle=35,vjust= .5),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

print(P1)


p.tot$yrDate <- p.tot$yrDate - 2000
p.tot <- transform(p.tot, Date = as.Date(yrDate, frac = .5))
p.tot$Trial <- as.factor(p.tot$Trial)
p.tot.p2 <- subset(p.tot, x>82)
p.tot.p2$yrdate.2 <- as.Date(p.tot.p2$yrDate)
selected_dates.P2 <- ymd("0003-05-15", "0003-08-15", "0003-11-16", 
                         "0004-02-15", "0004-05-15", "0004-08-15")
selected_labels.P2 <- c("May Year 2", "Aug Year 3", "Nov Year 3", 
                        "Feb Year 3","May Year 3", "Aug Year 4")


P2 <- ggplot(p.tot.p2, aes(y = pred))+ 
  geom_point(aes(x = Date, color = Trial), size = 2.5) +
  geom_pointrange(aes(ymin=lower.add, ymax=upper.add, x=Date, color = Trial)) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  scale_y_continuous(
    "Adductor Weight (grams)", 
    sec.axis = sec_axis(~(1/.) * 453.5929, name = "Meats/lbs (Count)",breaks=seq(5,30,5))
  )+
  labs(color = "Trial")+
  xlab("Time (Month)")+
  scale_x_date(breaks = selected_dates.P2, labels = selected_labels.P2) +
  scale_color_manual(values=c("#192841", "#F79256"))+
  theme(strip.text.x = element_text(size=12, face="bold"), legend.position = 'bottom',
        strip.text.y = element_text(size=12, face="bold"),axis.title.x = element_text(face="bold", size=12),
        legend.title = element_text(size=10, face="bold"), legend.text = element_text(size = 10),
        axis.title.y = element_text(face="bold", size=12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12,angle=35,vjust= .5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(fill='#E5E3FD'))

print(P2)


plot_grid(P2, P1,ncol = 1,
          labels = "AUTO")


###### Assumption 10,000, 50,000, 100,000, 1,000,000 scallop farm
##### Mortality set to standard 2.5%,5%,7.5%,10%


p.mort <- subset(predicted, day>269)
p.mort <- aggregate(data = p.mort, pred~Trial+yrDate, FUN = 'mean')

p.mort.eh <- subset(p.mort,Trial=='Ear Hanging')

p.mort.eh$m2.5 <- c(0,.025,.05,.075,.1,.125)
p.mort.eh$m5 <- c(0,.05,.1,.15,.2,.25)
p.mort.eh$m7.5 <- c(0,.075,.15,.225,.3,.375)
p.mort.eh$m10 <- c(0,.1,.2,.3,.4,.5)

p.mort.eh <- p.mort.eh %>%
  mutate(p.2.5 = pred-(pred*m2.5)) %>%
  mutate(p.5 = pred-(pred*m5)) %>%
  mutate(p.7.5 = pred-(pred*m7.5)) %>%
  mutate(p.10 = pred-(pred*m10))

p.mort.eh <- p.mort.eh %>%
  mutate('0%' = (pred-lag(pred,default=first(pred)))/pred*100) %>%
  mutate('2.5%' = (p.2.5 - lag(p.2.5,default = first(p.2.5)))/p.2.5*100) %>%
  mutate('5%' = (p.5 - lag(p.5,default = first(p.5)))/p.5*100) %>%
  mutate('7.5%' = (p.7.5 - lag(p.7.5,default = first(p.7.5)))/p.7.5*100) %>%
  mutate('10%' = (p.10 - lag(p.10,default = first(p.10)))/p.10*100)

p.mort.ln <- subset(p.mort,Trial=='Lantern Net')

p.mort.ln$m2.5 <- c(0,.025,.05,.075,.1,.125)
p.mort.ln$m5 <- c(0,.05,.1,.15,.2,.25)
p.mort.ln$m7.5 <- c(0,.075,.15,.225,.3,.375)
p.mort.ln$m10 <- c(0,.1,.2,.3,.4,.5)

p.mort.ln <- p.mort.ln %>%
  mutate(p.2.5 = pred-(pred*m2.5)) %>%
  mutate(p.5 = pred-(pred*m5)) %>%
  mutate(p.7.5 = pred-(pred*m7.5)) %>%
  mutate(p.10 = pred-(pred*m10))

p.mort.ln <- p.mort.ln %>%
  mutate('0%' = (pred-lag(pred,default=first(pred)))/pred*100) %>%
  mutate('2.5%' = (p.2.5 - lag(p.2.5,default = first(p.2.5)))/p.2.5*100) %>%
  mutate('5%' = (p.5 - lag(p.5,default = first(p.5)))/p.5*100) %>%
  mutate('7.5%' = (p.7.5 - lag(p.7.5,default = first(p.7.5)))/p.7.5*100) %>%
  mutate('10%' = (p.10 - lag(p.10,default = first(p.10)))/p.10*100)

p.mort <- rbind(p.mort.eh,p.mort.ln)
p.mort.table <- rbind(p.mort.eh,p.mort.ln)
write.csv(p.mort.table, 'p_mort_table.csv')

p.mort$yrDate<- p.mort$yrDate - 2000

p.mort$Date <- as.Date(p.mort$yrDate)
p.mort$Date.character <- as.character(p.mort$yrDate)
p.mort$Date.character <- c("May Year 2", "Aug Year 3", "Nov Year 3", 
                           "Feb Year 3", "May Year 3", "Aug Year 4", 
                           "May Year 2", "Aug Year 3", "Nov Year 3",
                           "Feb Year 3", "May Year 3", "Aug Year 4")

p.mort <- gather(p.mort, mortality,diff,'0%':'10%')

min3 <- as.Date("2002-06-01")
max3 <- NA

p.mort$mortality <- as.factor(p.mort$mortality)
p.mort$mortality <- factor(p.mort$mortality, levels = c('0%','2.5%','5%','7.5%','10%'))


P5 <- ggplot(p.mort, aes (x= Date, y = diff)) +
  geom_line(aes(color = mortality),linewidth=1) +
  geom_hline(yintercept=0, linetype='dashed',linewidth=1.2)+
  #scale_x_reverse() +
  #Theme presets
  theme_classic() +
  facet_grid(rows = vars(Trial)) +
  scale_x_date(breaks = p.mort$Date, labels = p.mort$Date.character) +
  #remove gridlines
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + 
  # Plot Labels
  #ggtitle("Quarterly Percent Change in Adductor Meat Weight over Time") +
  xlab("Time (Month)")+
  ylab("Weight Change (%)")+
  labs(colour = "Quarterly Mortality (%)")+  #Legend Title Label
  # Aesthetic themes
  theme(plot.title = element_blank (),      #Plot title themese
        axis.text.x  = element_text(size=16),     #X axis themes
        axis.title.x = element_text(face="bold", size=16),
        strip.text.x = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=16),      #y axis themes
        axis.title.y = element_text(face="bold", size=16),
        strip.text.y = element_text(size=16, face="bold"),
        legend.position = "bottom",               # Legend themes
        legend.title = element_text(size=16, face="bold"), 
        legend.text = element_text(size = 16))

p.mort<- gather(p.mort, add.mort, weight, c(pred,p.2.5:p.10))

p.mort$add.mort <- as.factor(p.mort$add.mort)
p.mort$add.mort <- factor(p.mort$add.mort, levels = c('pred','p.2.5','p.5','p.7.5','p.10'))


P6 <- ggplot(p.mort, aes (x= Date, y = weight)) +
  geom_line(aes(color = add.mort),linewidth=1) +
  #scale_x_reverse() +
  #Theme presets
  theme_classic() +
  facet_grid(rows = vars(Trial)) +
  scale_x_date(breaks = p.mort$Date, labels = p.mort$Date.character) +
  #remove gridlines
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + 
  # Plot Labels
  #ggtitle("Adductor Production (kg) in a 100,000 scallop Farm over Year 3 Grow-Out") +
  xlab("Time (Month/Year Class)")+
  ylab("Adductor Weight (grams)")+
   #Legend Title Label
  # Aesthetic themes
  theme(plot.title = element_blank(),      #Plot title themese
        axis.text.x  = element_blank(),     #X axis themes
        axis.title.x = element_blank(),
        strip.text.x = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=16),      #y axis themes
        axis.title.y = element_text(face="bold", size=16),
        strip.text.y = element_text(size=16, face="bold"),
        legend.position = "none",               # Legend themes
        legend.title = element_text(size=16, face="bold"), 
        legend.text = element_text(size = 16))

plot_grid(P6, P5,ncol = 1,labels='AUTO')


sig.ste <- subset(STE.dat, Cohort == 'y2' & Month == 8 & Year == 21 & Trial != 'Seed')

sig <- aov(data = sig.ste, ShellHeight~ Site * Trial, na.action=na.exclude)
summary(sig)

ggplot(Predicted.ci, aes(y = mean_sh))+ 
  #geom_line(aes(x = day, color = Trial), linewidth = .1) +
  geom_ribbon(aes(ymin=Low.day,ymax=High.day, x=day, group = Trial), alpha = .8) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab('Shell Height (mm)')+
  labs(color = "Trial")+
  # scale_x_date(NULL,
  #              breaks = scales::breaks_width("3 months"),
  #              labels = date_format("%b\nyear %_Y"),
  #              limits = c(min,max))+
  scale_color_manual(values=c("#192841", "#F79256"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
P1 <- ggplot(predicted.sh, aes(y = Sh_Height))+ 
  geom_line(aes(x = Date, color = Trial), linewidth = .1) +
  geom_ribbon(aes(ymin=Lower.cum,ymax=Upper.cum, x=Date, group = Trial), alpha = .3) +
  annotate("rect", xmin = min3, xmax = max2, ymin = 90, ymax = 120,
           alpha = .1,fill = "blue")+
  annotate("text", x = mid, y=93,
           label="Adductor Harvest period")+
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab('Shell Height (mm)')+
  labs(color = "Trial")+
  scale_x_date(breaks = selected_dates, labels = selected_labels) +
  # scale_x_date(NULL,
  #              breaks = scales::breaks_width("3 months"),
  #              labels = date_format("%b\nyear %_Y"),
  #              limits = c(min,max))+
  scale_color_manual(values=c("#192841", "#F79256"))+
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=12, face="bold"), legend.position = 'none',
        strip.text.y = element_text(size=12, face="bold"),axis.title.x = element_blank(),
        legend.title = element_text(size=10, face="bold"), legend.text = element_text(size = 10),
        axis.title.y = element_text(face="bold", size=12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12,angle=35,vjust= .5),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

print(P1)




###########Confidence Intervals

# Confidence intervals

# Lower bound of confidence interval.  Iterated daily 'through time' as opposed to 'at a time' for prediction purposes.
# For mixed effects models, a bootstrapped interval is required and the package merTools was used
# and the lmer function was also needed as opposed to glmmtmb
gamma.A1.lmer <- lmer(LGR ~ (1|Site) + (1|Cohort) + (mean_sh + Trial)^2 + (wgdd.mean+Trial)^2, data = A1)

lowerci <- function(z,y){
  a <- predictInterval(gamma.A1.lmer,newdata = data.frame(mean_sh = z, 
                                                          wgdd.mean = y, 
                                                          Trial = 'Ear Hanging', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),n.sims = 999,level =.80)
  a[,3]
}

populate_cilow.eh<- function(x){
  for(i in 2:length(x)){
    x[i] <- lowerci(x[i-1],y[i])+x[i-1]
  }
  x  
}

## TIterate starting at a projected size of 50 mm
pri = ret
pri[1,] = 50

d <- seq(0,759, by=1)                                  
predicted.eh.cilow <- data.frame(day = d)

predicted.eh.cilow$Lower <- sapply(pri,populate_cilow.eh)
colnames(predicted.eh.cilow) <- c("day","Lower.cum")
predicted.eh.cilow$Trial <- 'Ear Hanging'

# Upper bounds

upperci <- function(z,y){
  a <- predictInterval(gamma.A1.lmer,newdata = data.frame(mean_sh = z, 
                                                          wgdd.mean = y, 
                                                          Trial = 'Ear Hanging', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),n.sims = 999,level =.80)
  a[,2]
}

populate_ciup.eh<- function(x){
  for(i in 2:length(x)){
    x[i] <- upperci(x[i-1],y[i])+x[i-1]
  }
  x  
}

## TIterate starting at a projected size of 50 mm
pri = ret
pri[1,] = 50

d <- seq(0,759, by=1)                                  
predicted.eh.ciup <- data.frame(day = d)

predicted.eh.ciup$Upper <- sapply(pri,populate_ciup.eh)
colnames(predicted.eh.ciup) <- c("day","Upper.cum")
predicted.eh.ciup$Trial <- 'Ear Hanging'

predicted.eh$Upper.cum <- predicted.eh.ciup$Upper.cum
predicted.eh$Lower.cum <- predicted.eh.cilow$Lower.cum


# Confidence intervals

lowerci.ln <- function(z,y){
  a <- predictInterval(gamma.A1.lmer,newdata = data.frame(mean_sh = z, 
                                                          wgdd.mean = y, 
                                                          Trial = 'Lantern Net', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),n.sims = 999,level =.80)
  a[,3]
}

populate_cilow.ln<- function(x){
  for(i in 2:length(x)){
    x[i] <- lowerci.ln(x[i-1],y[i])+x[i-1]
  }
  x  
}

## TIterate starting at a projected size of 50 mm
pri = ret
pri[1,] = 50

d <- seq(0,759, by=1)                                  
predicted.ln.cilow <- data.frame(day = d)

predicted.ln.cilow$Lower <- sapply(pri,populate_cilow.ln)
colnames(predicted.ln.cilow) <- c("day","Lower.cum")
predicted.ln.cilow$Trial <- 'Lantern Net'

# Upper bounds

upperci.ln <- function(z,y){
  a <- predictInterval(gamma.A1.lmer,newdata = data.frame(mean_sh = z, 
                                                          wgdd.mean = y, 
                                                          Trial = 'Lantern Net', 
                                                          Site = 'NA',
                                                          Cohort = 'NA'),n.sims = 999,level =.80)
  a[,2]
}

populate_ciup.ln<- function(x){
  for(i in 2:length(x)){
    x[i] <- upperci.ln(x[i-1],y[i])+x[i-1]
  }
  x  
}

## Iterate starting at a projected size of 50 mm
pri = ret
pri[1,] = 50

d <- seq(0,759, by=1)                                  
predicted.ln.ciup <- data.frame(day = d)

predicted.ln.ciup$Upper <- sapply(pri,populate_ciup.ln)
colnames(predicted.ln.ciup) <- c("day","Upper.cum")
predicted.ln.ciup$Trial <- 'Lantern Net'

predicted.ln$Upper.cum <- predicted.ln.ciup$Upper.cum
predicted.ln$Lower.cum <- predicted.ln.cilow$Lower.cum

# Estimate bootstrapped confidence intervals at each day

predicted.ciln <- data.frame(day = d, mean_sh = predicted.ln.sh$Sh_Height, Trial = predicted.ln.sh$Trial, Site = 'NA',
                             Cohort = 'NA', wgdd.mean = y)
predicted.cieh <- data.frame(day = d, mean_sh = predicted.eh.sh$Sh_Height, Trial = predicted.eh.sh$Trial, Site = 'NA',
                             Cohort = 'NA', wgdd.mean = y)
Predicted.ci <- rbind(predicted.ciln,predicted.cieh)

colnames(Predicted.ci) <- c('day','mean_sh','Trial','Site','Cohort','wgdd.mean')
ci <- predictInterval(gamma.A1.lmer,newdata = Predicted.ci,n.sims = 999,level =.99)
co <- rbind(data.frame(fit = NA,upr=NA,lwr=NA), ci)
co <- co[-1521,]

Predicted.ci <- cbind(Predicted.ci,co)
Predicted.ci$Low.day <- Predicted.ci$mean_sh - Predicted.ci$lwr
Predicted.ci$High.day <- Predicted.ci$mean_sh + Predicted.ci$upr

predict(gamma.A1.top,newdata=Predicted.ci,se.fit = TRUE)