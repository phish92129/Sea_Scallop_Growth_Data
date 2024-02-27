# This code encompasses all analysis aspects and includes some graphics to support
# the model diagnostics and for showcasing decision process.  Load STE.R and run
# prior to this code (ie run this code second).

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

gamma.A3 <- glmmTMB(Ww_Add ~  (1|Site) + Sh_Height + Month + Trial,
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

p.tot <- arrange(p.tot,yrDate)


###### Analysis 4 percent change in shell and adductor against assumed mortality
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
