#### Load libraries####
library(ggplot2)
library(dplyr)
library(ggpattern)
library(forcats)
library(vctrs)
library(tidyr)
library(tibble)
library(lubridate)
library(anytime)
library(stringr)
library(egg)
library(cowplot)
library(zoo)
#unloadNamespace("reshape2")
 #unloadNamespace("plyr")

# ----- Tier 1 (Full): Imported Data Set-----
# Load data frame located in folder documents/R
# .dat indicates a data frame
# STE is base code for 'Scallop TechnoEconomic Analysis'

setwd("~/R/Scallops")
STE = read.csv ("earcomb_R.dat.csv")

# Assess dataset
summary(STE)

# Create global data set .dat

STE.dat <- STE

rm(STE)

###### Begin data formatting #####

# Site - Change names and to factors

  STE.dat$Site[STE.dat$Site == "Peters"] <- "Vertical Bay Farms"
  STE.dat$Site[STE.dat$Site == "deKoning"] <- "Acadia Aqua Farms"
  STE.dat$Site <- as.factor(STE.dat$Site)

# Date - Change date to Date format

  STE.dat$Date <- as.Date(STE.dat$Date,format='%m/%d/%y')
  
# Create a variable for month and year
  
  STE.dat$Month <- as.numeric(format(STE.dat$Date, '%m'))
  STE.dat$Year <- as.numeric(format(STE.dat$Date, '%y'))

# method -  Convert to factor
  
  STE.dat$method <- as.factor(STE.dat$method)
  
# Net - change to factor  

  STE.dat$Net <- as.factor(STE.dat$Net)
  
# Trial - Change to factor
  
  STE.dat$Trial <- as.factor(STE.dat$Trial)

# Add column 'Cohort' to differentiate cohorts

STE.dat$Cohort <- as.factor(ifelse(STE.dat$Trial == 'seed' | 
                                     STE.dat$Trial == 'snet' | 
                                     STE.dat$Trial == 'shung', 'y1', 'y2'))

# Replace placeholder data for actual names for Trial
STE.dat$Trial <- as.character(STE.dat$Trial)
STE.dat$Trial[STE.dat$Trial == "earhung" | STE.dat$Trial == "shung" ] <- "Ear Hanging"
STE.dat$Trial[STE.dat$Trial == "net" | STE.dat$Trial == "snet" ] <- "Lantern Net"
STE.dat$Trial[STE.dat$Trial == "seed"] <- "Seed"
STE.dat$Trial <- as.factor(STE.dat$Trial)

# Create date timeline

d0.vbf <- "08/03/21"
d0.vbf <- as.Date(d0.vbf, "%m/%d/%y")

d0.aaf <- "08/26/21"
d0.aaf <- as.Date(d0.aaf, "%m/%d/%y")

STE.dat$day <- ifelse(STE.dat$Site == 'Acadia Aqua Farms', STE.dat$Date - d0.aaf, STE.dat$Date - d0.vbf)

STE.dat$day <- ifelse(STE.dat$Cohort == 'y1', STE.dat$day, STE.dat$day + 365)

# Create an adjusted date as well for comparison between the farms 
# related to estimated settlement date y1: 10/01/2020 and y2: 10/01/2019

d0.y1 <- "10/01/20"
d0.y1 <- as.Date(d0.y1, "%m/%d/%y")

d0.y2 <- "10/01/19"
d0.y2 <- as.Date(d0.y2, "%m/%d/%y")

STE.dat$day.adjusted <- ifelse(STE.dat$Cohort == 'y2', STE.dat$Date - d0.y2, STE.dat$Date - d0.y1)

# Reorganize

STE.dat <- STE.dat[,c(1,2,9,10,11,12,13,4,8,7,3,5,6)]

# ----- Tier 2 (Full): Replicate (Net) level analysis ----- 

# Create smaller data frame showing average shell height by net number

STE.n.dat <- STE.dat %>% 
  dplyr::group_by(Site, Date, Month, Year, Cohort, day, day.adjusted, Net, Trial, method, IndTier) %>% 
  dplyr::summarise(ShellHeight = mean(ShellHeight,na.rm = TRUE))
  

# Save means by net trial for population modelling

STE.n.tble.dat <- STE.dat %>%
  dplyr::group_by(Site, Date, Month, Year, Cohort, day, day.adjusted, Trial, IndTier) %>% 
  dplyr::summarise(ShellHeight = mean(ShellHeight,na.rm = TRUE))

STE.var <- STE.dat %>% 
  dplyr::group_by(Site, Date, Month, Year, Cohort, day, day.adjusted, Trial, IndTier) %>% 
  dplyr::summarise(variance = var(ShellHeight,na.rm = TRUE))

STE.std <- STE.dat %>% 
  dplyr::group_by(Site, Date, Month, Year, Cohort, day, day.adjusted, Trial, IndTier) %>% 
  dplyr::summarise(std = sd(ShellHeight,na.rm = TRUE))

STE.n.mean.dat <- STE.n.dat
STE.n.tble.dat <- merge(STE.n.tble.dat,STE.var)
STE.n.tble.dat <- merge(STE.n.tble.dat,STE.std)

write.csv(STE.n.tble.dat, "table2.csv")

# Calculate percent growth rate
STE.n.dat <- STE.n.dat %>%
  group_by(Net) %>%
  mutate(Diff_Time = Date - lag(Date)) %>%
  mutate(Diff_growth = ShellHeight - lag(ShellHeight)) %>%
  mutate(mean_sh = (ShellHeight+lag(ShellHeight))/2)

STE.n.dat <- STE.n.dat %>%
  group_by(Net) %>%
  mutate(start_Date = Date - Diff_Time) %>%
  mutate(end_Date = Date-1)
         
STE.n.dat$Diff_Time <- as.numeric(STE.n.dat$Diff_Time)

# Remove NA values
STE.n.dat$Diff_Time[is.na(STE.n.dat$Diff_Time)] <- 0
STE.n.dat$Diff_growth[is.na(STE.n.dat$Diff_growth)] <- 0
# STE.n.dat$g.st[is.na(STE.n.dat$g.st)] <- 0

STE.n.dat = STE.n.dat %>%
  dplyr::group_by (Net) %>%
  mutate(Rate_percent = (Diff_growth / Diff_Time)/lag(ShellHeight) * 100)
 

STE.n.dat = STE.n.dat %>%
  dplyr::group_by (Net) %>%
  mutate (LGR = Diff_growth/Diff_Time)

STE.n.dat <- STE.n.dat %>%
                tidyr::drop_na(Rate_percent)

# STE.n.dat = STE.n.dat %>%
#   dplyr::group_by (Net) %>%
#   mutate(SGR = 100*(exp(g.st/Diff_Time)-1))

###### Individual Hobo Logger Data sets processed for merging by Average Daily Temperature #####

# Ear Hanging for VBF start: 08/02/2021 end: 02/23/2022

# Load data set (commented only for the first block)
peters.t <- read.csv("P_OD_1_EH.csv")
# Convert DT to character to extract just m/d/y
peters.t$DT <- as.character(peters.t$DT)

# Select first 8 characters mm/dd/yy
peters.t$DT <- substring(peters.t$DT,first = 0,last = 8)

# Convert to factor and then date
peters.t$DT <- as.factor(peters.t$DT)
peters.t$DT <- as.Date(peters.t$DT, "%m/%d/%y")

# Slice out data taken outside the water
peters.t <- peters.t %>% slice(-c(4592:4916))
peters.t <- peters.t %>% slice(-c(1:559))

# Aggregate data to calculate mean daily temperature
peters.tm <- aggregate(data = peters.t, Temp~DT, FUN = 'mean')

# Distinguish Site
peters.tm$Site <- "Vertical Bay Farm"


# Lantern Net for AAF start: 08/26/2021 end: 02/23/2022
alex.t <- read.csv("D_OD_1_LN.csv")
alex.t$DT <- as.character(alex.t$DT)
alex.t$DT <- substring(alex.t$DT,first = 0,last = 8)
alex.t$DT <- as.factor(alex.t$DT)
alex.t$DT <- as.Date(alex.t$DT, "%m/%d/%y")
alex.t <- alex.t %>% slice(-c(4016:4340))
alex.tm <- aggregate(data = alex.t, Temp~DT, FUN = 'mean')
alex.tm$Site <- "Acadia Aqua Farm"

# Lantern Net for VBF start: 08/02/2021 end: 05/11/2022
peters.t2 <- read.csv("P_OD_1_LN.csv")
#peters.t2$DT <- as.character(peters.t2$DT)
#peters.t2$DT <- substring(peters.t2$DT,first = 0,last = 8)
peters.t2$DT <- as.factor(peters.t2$DT)
peters.t2$DT <- as.Date(peters.t2$DT, "%m/%d/%Y")
peters.t2 <- peters.t2 %>% slice(-c(6761:6808))
peters.t2 <- peters.t2 %>% slice(-c(1:31))
peters.tm2 <- aggregate(data = peters.t2, Temp~DT, FUN = 'mean')
peters.tm2$Site <- "Vertical Bay Farm"

# Lantern Net for AAF start: 08/26/2021 end: 05/13/2022
alex.t2 <- read.csv("D_OD_1_EH.csv")
alex.t2$DT <- as.character(alex.t2$DT)
alex.t2$DT <- substring(alex.t2$DT,first = 0,last = 8)
alex.t2$DT <- as.factor(alex.t2$DT)
alex.t2$DT <- as.Date(alex.t2$DT, "%m/%d/%y")
alex.t2 <- alex.t2 %>% slice(-c(6209:6236))
alex.tm2 <- aggregate(data = alex.t2, Temp~DT, FUN = 'mean')
alex.tm2$Site <- "Acadia Aqua Farm"


# Ear Hanging for VBF start: 02/11/2022 end: 08/09/2022
peters.t3 <- read.csv("P_OD_2_EH.csv")
peters.t3$DT <- as.character(peters.t3$DT)
peters.t3$DT <- substring(peters.t3$DT,first = 0,last = 8) 
peters.t3$DT <- as.factor(peters.t3$DT)
peters.t3$DT <- as.Date(peters.t3$DT, "%m/%d/%y")
peters.t3 <- peters.t3 %>% slice(-c(4263:4298))
peters.tm3 <- aggregate(data = peters.t3, Temp~DT, FUN = 'mean')
peters.tm3$Site <- "Vertical Bay Farm"

# Lantern Net for VBF start: 05/11/2022 end: 11/02/2022
peters.t4 <- read.csv("P_OD_2_LN.csv")
peters.t4$DT <- as.character(peters.t4$DT)
peters.t4$DT <- substring(peters.t4$DT,first = 0,last = 8)
peters.t4$DT <- as.factor(peters.t4$DT)
peters.t4$DT <- as.Date(peters.t4$DT, "%m/%d/%y")
peters.t4 <- peters.t4 %>% slice(-c(4006:4205))
peters.tm4 <- aggregate(data = peters.t4, Temp~DT, FUN = 'mean')
peters.tm4$Site <- "Vertical Bay Farm"

# Ear Hanging for VBF start: 08/08/2022 end: 02/15/2023
peters.t5 <- read.csv("P_OD_3_EH.csv")
peters.t5$DT <- as.character(peters.t5$DT)
peters.t5$DT <- substring(peters.t5$DT,first = 0,last = 8)
peters.t5$DT <- as.factor(peters.t5$DT)
peters.t5$DT <- as.Date(peters.t5$DT, "%m/%d/%y")
peters.t5 <- peters.t5 %>% slice(-c(4547:4590))
peters.tm5 <- aggregate(data = peters.t5, Temp~DT, FUN = 'mean')
peters.tm5$Site <- "Vertical Bay Farm"

# Lantern Net for VBFF start: 11/03/2022 end: 05/10/2023
peters.t6 <- read.csv("P_OD_4_LN.csv")
peters.t6$DT <- as.character(peters.t6$DT)
peters.t6$DT <- substring(peters.t6$DT,first = 0,last = 8)
peters.t6$DT <- as.factor(peters.t6$DT)
peters.t6$DT <- as.Date(peters.t6$DT, "%m/%d/%y")
peters.t6 <- peters.t6 %>% slice(-c(4507:4513))
peters.tm6 <- aggregate(data = peters.t6, Temp~DT, FUN = 'mean')
peters.tm6$Site <- "Vertical Bay Farm"

# Ear Hanging for VBF start: 02/14/2023 end: 05/11/2023
peters.t7 <- read.csv("P_OD_4_EH.csv")
peters.t7$DT <- as.character(peters.t7$DT)
peters.t7$DT <- substring(peters.t7$DT,first = 0,last = 8)
peters.t7$DT <- as.factor(peters.t7$DT)
peters.t7$DT <- as.Date(peters.t7$DT, "%m/%d/%y")
peters.t7 <- peters.t7 %>% slice(-c(8150:8259))
peters.tm7 <- aggregate(data = peters.t7, Temp~DT, FUN = 'mean')
peters.tm7$Site <- "Vertical Bay Farm"

# Ear Hanging for VBF start: 02/14/2023 end: 05/11/2023
peters.t8 <- read.csv("P_OD_5_LN.csv")
peters.t8$DT <- as.character(peters.t8$DT)
peters.t8$DT <- substring(peters.t8$DT,first = 0,last = 10)
peters.t8$DT <- as.factor(peters.t8$DT)
peters.t8$DT <- as.Date(peters.t8$DT, "%m/%d/%Y")
peters.tm8 <- aggregate(data = peters.t8, Temp~DT, FUN = 'mean')
peters.tm8$Temp <- (peters.tm8$Temp  *(9/5)) +32
peters.tm8$Site <- "Vertical Bay Farm"

# Merge into a single continuous data set

temp.m <- rbind(peters.tm,
                alex.tm,
                peters.tm2,
                alex.tm2,
                peters.tm3,
                peters.tm4,
                peters.tm5,
                peters.tm6,
                peters.tm7,
                peters.tm8)

# Aggregate into mean daily temperatures within site for all loggers
temp.ma<- aggregate(data = temp.m, Temp~DT + Site, FUN = 'mean')

# Convert Fahrenheit to Celsius
temp.ma <- mutate(temp.ma, Tempc = (Temp - 32) *(5/9))


# Create a dummy month variable at yy/mm/15 date midpoint for each month
temp.ma$Month_Yr <- format(as.Date(temp.ma$DT), "%Y-%m-15")
temp.ma$Month_Yr <- as.Date(temp.ma$Month_Yr)

# Convert site to a factor
temp.ma$Site <- as.factor(temp.ma$Site)

# Create a data set with average monthly temperatures
temp.mamo <- aggregate(data = temp.ma, Tempc~Month_Yr + Site, FUN = 'mean')

# Remove excess data sets from above from global environment
rm(peters.tm,
   alex.tm,
   peters.tm2,
   alex.tm2,
   peters.tm3,
   peters.tm4,
   peters.tm5,
   peters.tm6,
   peters.tm7,
   peters.tm8)

rm(peters.t,
   alex.t,
   peters.t2,
   alex.t2,
   peters.t3,
   peters.t4,
   peters.t5,
   peters.t6,
   peters.t7,
   peters.t8)

# Plot data separated by site and daily average with dots indicating monthly means

ggplot(data = temp.ma, aes (y= Tempc, x=DT)) +
  geom_line(aes(color = Site), size = 1.1, alpha = .4) +
  geom_point(data = temp.mamo, aes(x = Month_Yr, y = Tempc, color = Site), size = 1.6) +
  #Theme presets
  theme_classic() + scale_colour_brewer(palette="Set1") +
  #remove gridlines
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) +
  # Plot Labels
  xlab("Time")+
  ylab(expression("Temperature ("*~degree*C*")"))+
  labs(color = "Sample Site")+  #Legend Title Label
  scale_x_date(breaks = seq(min(temp.mamo$Month_Yr),max(temp.mamo$Month_Yr), by = "3 month"), date_labels="%Y-%b") +
  scale_y_continuous(limits = c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  geom_hline(aes(yintercept = 10), color = "black", linetype = 'dashed') +
  geom_hline(aes(yintercept = 15), color = "black", linetype = 'dashed') +
  annotate("text", x = as.Date("2022-09-15"), y = 12.5, label = "Optimal Growth Range", size =6) +
  # Aesthetic themes
  theme(plot.title = element_blank(),      #Plot title themese
        axis.text.x  = element_text(size=16, angle = 35, hjust = .85),     #X axis themes
        axis.title.x = element_text(face="bold", size=16),
        strip.text.x = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=16),      #y axis themes
        axis.title.y = element_text(face="bold", size=16),
        strip.text.y = element_text(size=16, face="bold"),             # Legend themes
        legend.title = element_text(size=12, face="bold"), 
        legend.text = element_text(size = 12),
        legend.position = c(0.90, 0.90),
        legend.background = element_rect(fill = "white", color = "black", size = .8),)


# Create degree growth days category

# Create data frame with Site, Stat Date, End Date, and Date for sample periods
df <- STE.n.dat[,c(1,8,16,17,2)]

# Make factor Site numeric
df$Site <- as.numeric(df$Site)

# Create a new data frame for temp 
Temp.wdgd <- temp.ma

# Work out function for weighted degree growth days Tmin, Tmax, T opt
Tmax <- 21
Tmin <- -2
Topt <- 12.5

# Create Russo applied function (for pH similar to other papers)
t.weighted <- function(x) {
  ((x-Tmin)*(x-Tmax))/((x-Tmin)*(x-Tmax) - (x-Topt)^2)
}

# apply function to temperature range
Temp.wdgd$wdgd <- sapply(Temp.wdgd$Tempc,t.weighted)

# Create a binning function to sum cumulative degree growth days 

# New data frame
Temp.wdgd.F <- Temp.wdgd


# Re-organize data, switch site to numeric and ensure that values can not decrease below 0
Temp.wdgd.F<- Temp.wdgd.F[,c(2,6,1)]
Temp.wdgd.F$Site <- as.numeric(Temp.wdgd.F$Site)
Temp.wdgd.F$wdgd<- pmax(Temp.wdgd.F$wdgd, 0)

#left join the data frames
Temp.df <- left_join(df, Temp.wdgd.F,relationship = "many-to-many") %>%
  #determine if DT is outside of DT_START and DT_END. If so, or NA, assign 0, else assign wdgd 
  mutate(in_range = ifelse(is.na(DT) | (DT >= start_Date & DT <= end_Date), wdgd, 0)) %>%
  #group by and summarise
  dplyr::group_by(Site,Net, start_Date, end_Date) %>%
  dplyr::summarise(COUNT = sum(in_range)) %>%
  ungroup %>%
  as.data.frame

#Remove Net category
Temp.df <- Temp.df[,-c(1,2)]

# Rename column to abb of weighted growth degree days
colnames(Temp.df)[3]="wgdd"

# Merge data sets with temperature data

# Merge with temperature wgdd days data EH_Temperature.R use duplicate function
# create a new data set as well to ensure rows not deleted in error
STE.n.dat.t <- merge(STE.n.dat,Temp.df)
STE.n.dat.t <- STE.n.dat.t[!duplicated(STE.n.dat.t$LGR),]

# Take average wgdd time
STE.n.dat.t <- mutate(STE.n.dat.t, wgdd.mean = wgdd/Diff_Time)



###############Get seasonal temps

# Re-organize data, switch site to numeric and ensure that values can not decrease below 0
Temp.wdgd.t<- Temp.wdgd[,c(2,4,1)]
Temp.wdgd.t$Site <- as.numeric(Temp.wdgd.t$Site)

#left join the data frames
Temp.df.t <- left_join(df, Temp.wdgd.t,relationship = "many-to-many") %>%
  #determine if DT is outside of DT_START and DT_END. If so, or NA, assign 0, else assign wdgd 
  mutate(in_range = ifelse(DT >= start_Date & DT <= end_Date, Tempc, NA)) %>%
  #group by and summarise
  dplyr::group_by(Site,Net, start_Date, end_Date) %>%
  dplyr::summarise(mean = mean(in_range, na.rm = TRUE)) %>%
  ungroup %>%
  as.data.frame



# Merge data sets with temperature data

# Merge with temperature wgdd days data EH_Temperature.R use duplicate function
# create a new data set as well to ensure rows not deleted in error
STE.n.dat.t <- merge(STE.n.dat,Temp.df)
STE.n.dat.t <- STE.n.dat.t[!duplicated(STE.n.dat.t$LGR),]

# Take average wgdd time
STE.n.dat.t <- mutate(STE.n.dat.t, wgdd.mean = wgdd/Diff_Time)


#####Meat Weight Analysis#####

#### Load Data set
sw <- read.csv("Scallop_Weights.csv")

# Create indexes between different meat weight categories

# Year Month column
sw$DateFull <- as.Date(paste(sw$Year,sw$Month,sw$Date),"%Y %m %d")
sw$Date <- as.yearmon(paste(sw$Year, sw$Month), "%Y %m")

# Integrate WGDD

sw.temp <- Temp.df
sw.temp$DateFull <- ymd(sw.temp$end_Date %m+% days(1))
sw.temp <- sw.temp[,c(3,4)]

sw <- merge(sw,sw.temp)
sw <- sw[!duplicated(sw$Spec_Num),]

sw.diff <- STE.n.dat.t[,c(4,15)]
colnames(sw.diff)[1] = "DateFull"

sw <- merge(sw,sw.diff)
sw <- sw[!duplicated(sw$Spec_Num),]

sw <- mutate(sw, wgdd.mean = wgdd/Diff_Time)

#Total Meat weight
sw <- mutate(sw, tww = Ww_Add + Ww_Gon + Ww_Vis)

# Gonadosomatic Index
sw <- mutate(sw, gsi = Ww_Gon/tww)

# Adductor Index
sw <- mutate(sw, ai = Ww_Add/tww)

#Viscera Index
sw <- mutate(sw, vi = Ww_Vis/tww)

sw$facMonthyr <- as.factor(sw$Date)
sw <- mutate(sw, ashi = Ww_Add/Sh_Height)

#### Imperial Units

sw <- mutate(sw, Sh.imp = 0.0393701 * Sh_Height)
sw <- mutate(sw, Ww_Add.imp = 0.00220462 * Ww_Add)
sw <- mutate(sw, meat_count = 1.0/Ww_Add.imp)

sw <- sw[-c(21), ] 
