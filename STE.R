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

# ----- Tier 1 (Full): Imported Data Set-----
# Load data frame located in folder documents/R
# .dat indicates a data frame
# STE is base code for 'Scallop TechnoEconomic Analysis'

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

rm(d0.aaf,d0.vbf,d0.y1,d0.y2)
# Reorganize

STE.dat <- STE.dat[,c(1,2,9,10,11,12,13,4,8,7,3,5,6)]

# ----- Tier 2 (Full): Replicate (Net) level analysis ----- 

# Create smaller data frame showing average shell height by net number replicate

STE.n.dat <- STE.dat %>% 
  dplyr::group_by(Site, Date, Month, Year, Cohort, day, day.adjusted, Net, Trial, method, IndTier) %>% 
  dplyr::summarise(ShellHeight = mean(ShellHeight,na.rm = TRUE))
  

# Save means by net trial for tabulated output and remove method

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
rm(STE.std,STE.var)

write.csv(STE.n.tble.dat, "table2.csv")

# Calculate growth rate
STE.n.dat <- STE.n.dat %>%
  dplyr::group_by(Net) %>%
  dplyr::mutate(Diff_Time = Date - dplyr::lag(Date)) %>%
  dplyr::mutate(Diff_growth = ShellHeight - dplyr::lag(ShellHeight)) %>%
  dplyr::mutate(mean_sh = (ShellHeight+dplyr::lag(ShellHeight))/2)

STE.n.dat <- STE.n.dat %>%
  dplyr::group_by(Net) %>%
  dplyr::mutate(start_Date = Date - Diff_Time) %>%
  dplyr::mutate(end_Date = Date-1)
         
STE.n.dat$Diff_Time <- as.numeric(STE.n.dat$Diff_Time)

# Remove NA values
STE.n.dat$Diff_Time[is.na(STE.n.dat$Diff_Time)] <- 0
STE.n.dat$Diff_growth[is.na(STE.n.dat$Diff_growth)] <- 0
# STE.n.dat$g.st[is.na(STE.n.dat$g.st)] <- 0

STE.n.dat = STE.n.dat %>%
  dplyr::group_by (Net) %>%
  dplyr::mutate(Rate_percent = (Diff_growth / Diff_Time)/dplyr::lag(ShellHeight) * 100)
 

STE.n.dat = STE.n.dat %>%
  dplyr::group_by (Net) %>%
  dplyr::mutate (LGR = Diff_growth/Diff_Time)

STE.n.dat <- STE.n.dat %>%
                tidyr::drop_na(Rate_percent)

###### Individual Hobo Logger Data sets processed for merging by Average Daily Temperature (process not shown) #####

temp.m <- read.csv('temp_combined.csv')
temp.m <- temp.m[,-1]
temp.m$DT <- as.Date(temp.m$DT)
temp.m$Site <- as.factor(temp.m$Site)

# Aggregate into mean daily temperatures within site for all loggers
temp.ma<- aggregate(data = temp.m, Temp~DT + Site, FUN = 'mean')
rm(temp.m)

# Convert Fahrenheit to Celsius
temp.ma <- mutate(temp.ma, Tempc = (Temp - 32) *(5/9))


# Create a dummy month variable at yy/mm/15 date midpoint for each month
temp.ma$Month_Yr <- format(as.Date(temp.ma$DT), "%Y-%m-15")
temp.ma$Month_Yr <- as.Date(temp.ma$Month_Yr)

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
Temp.wdgd


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
rm(sw.temp)

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
