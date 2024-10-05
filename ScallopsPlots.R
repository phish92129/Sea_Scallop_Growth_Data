###### Figure 1######
# (A) Annual Commercial sea scallop harvest and (B) annual price per pound

land <- read.csv("Scallop_Landings.csv")
cpi <- read.csv('CPI.csv')

land$kg <- land$Pounds*0.453592

land$Pricekgs <- land$Dollars/land$kg

cpi.m <- data.frame(Year=cpi[,1], Means=rowMeans(cpi[,-1]))

land <- merge(land,cpi.m,'Year')

land <- land %>%
  mutate(cpi.adjust = (Pricekgs/Means) *100)
land <- land %>%
  mutate(Sales = (Dollars/1000000))

colors <- c("Landings" = "#006994", "Sales" = "#FFD700")

pl1 <- ggplot(land, aes(x=Year))  + 
  geom_line(aes(y = Metric.Tons, color = 'Landings'), size = 1.1)+
  geom_point (aes(y = Metric.Tons), color = '#006994', size = 1.2)+
  geom_line(aes(y=Sales*45, color = 'Sales'), size = 1.1)+
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab('Commercially Landed Scallops (Metric Tons)')+
  scale_y_continuous(sec.axis = sec_axis(~./45, name='Total Sales (in Millions USD)'),breaks=seq(0,30000,5000)) +
  scale_x_continuous(breaks = seq(1950,2021,5)) +
  scale_color_manual(values = colors) +
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        axis.title.y = element_text(size=12,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        legend.position = c(0.08, 0.85),
        legend.background = element_rect(fill = "white", color = "black", size = .8),
        axis.text.y = element_text(size=12),
        axis.text.x=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

colors2 <- c("USD/kgs" = "#301934", "Adjusted USD/kgs" = "#85BB65")

PL2 <- ggplot(land, aes(x=Year))  + 
  geom_line(aes(y = (cpi.adjust*3), color = 'Adjusted USD/kgs'), size = 1.1)+
  geom_line(aes(y=Pricekgs, color = 'USD/kgs'), alpha = .5, size = 1.1, linetype = 'dashed')+
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab(bquote('Price '(USD~kg^-1)))+
  labs(color = "Price")+
  xlab("Year")+
  scale_y_continuous(sec.axis = sec_axis(~./ 3, name=bquote('Inflation Adjusted Price  ' (USD~kg^-1)))) +
  scale_x_continuous(breaks = seq(1950,2021,5)) +
  scale_color_manual(values = colors2, labels = c(bquote('Adjusted Price '(USD~kg^-1)),bquote('Price '(USD~kg^-1)))) +
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_text( size=15),
        legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        axis.title.y = element_text(size=12, margin = margin(t = 0, r = 22, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", color = "black", size = .8),
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size = 12,  angle = 35, hjust = .85),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

plot_grid(pl1, PL2,ncol = 1,
          labels = "AUTO")

# Save figure as 1100x1100

##### Figure 3 ######
# Full growth timeline

p <- STE.dat
# p1 <- subset(p1, Trial != "Seed")

p1<- p
p1$Trial <- as.character(p1$Trial)
p1$Trial[p1$Trial == "Ear Hanging"] <- "Ear-Hanging"
p1$Trial <- as.factor(p1$Trial)
p1$M <- month.abb[p1$Month]
P1.y1 <- subset(p1,Cohort == "y1")
P1.y2 <- subset(p1,Cohort == "y2")

P1.y1$Y <- ifelse(P1.y1$day >= 0 & P1.y1$day <= 350, 'Year 1','Year 2')
P1.y2$Y <- ifelse(P1.y2$day >= 365 & P1.y2$day <= 700, 'Year 2',ifelse(P1.y2$day==1099, 'Year 4','Year 3'))

p1 <- rbind(P1.y1,P1.y2)

p1$S <- paste(p1$M,p1$Y, sep = "-")
p1$S <- as.factor(p1$S)

p1$S <- factor(p1$S, levels = c("Aug-Year 1", "Nov-Year 1", "Feb-Year 1","May-Year 1","Aug-Year 2", "Nov-Year 2", "Feb-Year 2","May-Year 2","Aug-Year 3", "Nov-Year 3", "Feb-Year 3", "May-Year 3", "Aug-Year 4"))

#  Plot 1 - tiled violin plot of all measurement periods

ggplot(p1, aes(y = ShellHeight))  + 
  geom_violin(aes(fill = Trial, x = S))+
  facet_grid(vars(Site)) +
  geom_hline (yintercept = 45, linetype="dashed", color = "red") +
  geom_hline (yintercept = 90, linetype="dashed", color = "red") +
  geom_hline (yintercept = 100, linetype="dashed", color = "red") +
  annotate ("text", x = .8, y = 51, label = "Ear-Hanging Size", size = 3.7)+
  annotate ("text", x = 1, y = 96, label = "Harvest - Whole Scallop", size = 3.7)+
  annotate ("text", x = 1, y = 106, label = "Harvest - Adductor Muscle", size = 3.7)+
  annotate ("rect", xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf, color = 'grey', alpha = .1)+
  annotate ("rect", xmin =4, xmax = 6, ymin = -Inf, ymax = Inf, color = 'grey', alpha = .1)+
  annotate ("text", x = .8, y = 0, label = "Restock", size = 3.7)+
  annotate ("text", x = 5, y = 0, label = "Restock or Ear-Hanging", size = 3.7)+
  theme_minimal() +
  scale_fill_manual(values=c("#192841", "#F79256","#0B6623"))+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab("Shell Height (mm)")+
  labs(fill = "Gear Type")+
  xlab("Sample Measurement Period")+
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=14, face="bold"), legend.position = "bottom",
        strip.text.y = element_text(size=14, face="bold"),axis.title.x = element_text(face="bold", size=14),
        legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14),
        axis.title.y = element_text(face="bold", size=14,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 35, hjust = .85),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

# Save as 1350x600

############### Figure 4 ##############
# Temperature plot

# Create a data set with average monthly temperatures
temp.mamo <- aggregate(data = temp.ma, Tempc~Month_Yr + Site, FUN = 'mean')

# Plot data separated by site and daily average with dots indicating monthly means

ggplot(data = temp.ma, aes (y= Tempc, x=DT)) +
  geom_line(aes(color = Site), size = 1.5, alpha = .4) +
  geom_point(data = temp.mamo, aes(x = Month_Yr, y = Tempc, color = Site), size = 2.0) +
  #Theme presets
  theme_classic() + scale_colour_brewer(palette="Set1") +
  #remove gridlines
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) +
  # Plot Labels
  xlab("Date")+
  ylab(expression("Temperature ("*~degree*C*")"))+
  labs(color = "Sample Site")+  #Legend Title Label
  scale_x_date(breaks = seq(min(temp.mamo$Month_Yr),max(temp.mamo$Month_Yr), by = "3 month"), date_labels="%Y-%b") +
  scale_y_continuous(limits = c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  geom_hline(aes(yintercept = 10), color = "black", linetype = 'dashed') +
  geom_hline(aes(yintercept = 15), color = "black", linetype = 'dashed') +
  annotate("text", x = as.Date("2022-02-15"), y = 12.5, label = "Optimal Growth Range", size =6) +
  # Aesthetic themes
  theme(plot.title = element_blank(),      #Plot title themese
        axis.text.x  = element_text(size=20, angle = 35, hjust = .85),     #X axis themes
        axis.title.x = element_text(size=20),
        strip.text.x = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=20),      #y axis themes
        axis.title.y = element_text(size=20),
        strip.text.y = element_text(size=20, face="bold"),             # Legend themes
        legend.title = element_text(size=16, face="bold"), 
        legend.text = element_text(size = 16),
        legend.position = c(0.90, 0.15),
        legend.background = element_rect(fill = "white", color = "black", size = .8),)

# Save as 1250 x 650

##### Figure 5 #######
p.wgdd <- Temp.wdgd
p.wgdd$Month <- as.numeric(format(p.wgdd$DT, '%m'))
p.wgdd$Month <- month.abb[p.wgdd$Month]
p.wgdd$Month <- as.factor(p.wgdd$Month)

p.wgdd$Month <- factor(p.wgdd$Month, levels = c("Jan", "Feb", "Mar","Apr","May", 
                                                "Jun", "Jul","Aug","Sep", "Oct", "Nov","Dec"))

p.w <- aggregate(data = p.wgdd, wdgd~Month, FUN = 'mean')

color <- colorRamp(c("Blue","gold"))
p.w$st <- with(p.w, (wdgd - min(wdgd)) / diff(range(wdgd)))
p.w$color <- rgb(color(p.w$st)/255)

ggplot(p.wgdd, aes(x=Tempc))  + 
  geom_point(aes(y = wdgd, color = Month)) +
  theme_minimal() +
  scale_color_manual(values=p.w$color)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab('Weighted Growth Degree Day')+
  guides(color=guide_legend(keyheight=0.15,default.unit="inch"))+
  #labs(fill = "Gear Type")+
  xlab(expression("Average Daily Temperature ("*~degree*C*")"))+
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=10, face="bold"), 
        legend.position = c(.93,.27),
        strip.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        legend.title = element_text(size=10, face="bold"), 
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12),
        legend.box.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, size=1))

# Save as 700x600

####### Figure 6 ########
# Predicted LGR as a function of Shell height

# Prediction of best fitting model for graphical output
x <- seq(min(A1$mean_sh),max(A1$mean_sh), by = .01)
y <- seq(.4,1,by=.01)

newdata.eh <- expand.grid(
  mean_sh = x,
  wgdd.mean = y,
  Trial = 'Ear Hanging',
  Site = 'NA',
  Cohort = 'NA'
)


newdata.eh$pred <- predict(gamma.A1.top,newdata.eh,allow.new.levels=TRUE)

newdata.ln <- expand.grid(
  mean_sh = x,
  wgdd.mean = y,
  Trial = 'Lantern Net',
  Site = 'NA',
  Cohort = 'NA'
)

newdata.ln$pred <- predict(gamma.A1.top,newdata.ln,allow.new.levels=TRUE)

pred.plot <- rbind(newdata.ln,newdata.eh)

pred.plot$Trial <- as.character(pred.plot$Trial)
pred.plot$Trial[pred.plot$Trial == "Ear Hanging"] <- "Ear-Hanging"
pred.plot$Trial <- as.factor(pred.plot$Trial)

ggplot(pred.plot, aes(y = pred))+ 
  geom_point(aes(x = mean_sh, color = wgdd.mean), size = 1.0) +
  theme_minimal() +
  scale_color_viridis() +
  facet_grid(rows = vars(Trial)) +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ylab(bquote('Linear Growth Rate  '(mm~day^-1)))+
  labs(color = "WGDD")+
  xlab("Shell Height (mm)")+
  theme(plot.title = element_blank (),
        strip.text.x = element_text(size=12, face="bold"), legend.position = c(.95,.85),
        strip.text.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12),
        legend.title = element_text(size=10, face="bold"), legend.text = element_text(size = 10),
        axis.title.y = element_text(face="bold", size=12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.ticks = element_line(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

# Save as 750x750

###### Figure 7 ######
# Adductor weight predicted as a function of shell height

# Create a plot with predicted trend and raw data

x.d <- seq(min(A3$Sh_Height),max(A3$Sh_Height), by = 1)

newdata.mw <- expand.grid(
  Site='NA',
  Sh_Height = x.d,
  Month = unique(A3$Month)
)

newdata.mw$pred <- predict(gamma.A3.top,newdata.mw,allow.new.levels=TRUE,
                           type = 'response')
newdata.mw$Month <- as.character(newdata.mw$Month)
newdata.mw$Month <- as.numeric(newdata.mw$Month)

newdata.mw$Month<- month.abb[newdata.mw$Month]
newdata.mw$Month <- as.factor(newdata.mw$Month)
newdata.mw$Month <- factor(newdata.mw$Month, levels = c('Feb','May','Aug','Nov'))

A3$Month <- as.character(A3$Month)
A3$Month <- as.numeric(A3$Month)

A3$Month<- month.abb[A3$Month]
A3$Month <- as.factor(A3$Month)
A3$Month <- factor(A3$Month, levels = c('Feb','May','Aug','Nov'))

ggplot(A3, aes (x= Sh_Height, y = Ww_Add)) +
  geom_point(aes(color = Month)) +
  geom_line(data = newdata.mw, aes(x=Sh_Height, y = pred, color = Month), size = 1) +
  geom_hline (yintercept = 45.36, linetype="dashed", color = "red") +
  annotate ("text", x = 65, y = 47, label = "10 Count", size = 4)+
  geom_hline (yintercept = 30.24, linetype="dashed", color = "red") +
  annotate ("text", x = 65, y = 32, label = "15 Count", size = 4)+
  geom_hline (yintercept = 22.68, linetype="dashed", color = "red") +
  annotate ("text", x = 65, y = 24.2, label = "20 Count", size = 4)+
  #scale_x_reverse() +
  #Theme presets
  theme_classic() +
  #remove gridlines
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + 
  # Plot Labels
  ggtitle("Shell Height as a Function of Adductor Weight") +
  xlab("Shell Height (mm)")+
  ylab("Weight (g)")+
  labs(colour = "Month")+  #Legend Title Label
  scale_color_manual(values=c("#8F00FF","#0D98BA","#FFFF00","#ff5349")) +
  # Aesthetic themes
  theme(plot.title = element_blank (),      #Plot title themese
        axis.text.x  = element_text(size=16),     #X axis themes
        axis.title.x = element_text(face="bold", size=16),
        strip.text.x = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=16),      #y axis themes
        axis.title.y = element_text(face="bold", size=16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.text.y = element_text(size=16, face="bold"),
        legend.position = "bottom",               # Legend themes
        legend.title = element_text(size=16, face="bold"), 
        legend.text = element_text(size = 16))

# set to size 1050x525

###### Figure 8 #######
# Simulated growth of shell height and adductor

library(scales)

custom_date_labels <- function(date_row, #point at OG dates
                               start_date, #specify what year number
                               start_month, #specify what month number aug = 8
                               mode = "labs", #for labs or breaks?
                               num_breaks = 9) { #set the number of breaks you want
  
  dates <- seq(min(date_row), max(date_row), by = ((as.numeric(difftime(max(date_row), min(date_row), units = "days")))/ (num_breaks - 1)))
  if (mode == "breaks"){ #Returns the dates of the breaks to be used in placing them
    return(dates)
  }else{#labs
    
    labels <- sapply(dates, function(date) {
      parsed_date <<- as.Date(date)
      year_diff <- year(parsed_date) - start_date
      if (month(parsed_date) >= start_month) {
        year_label <- year_diff
      } else {
        year_label <- year_diff - 1
      }
      paste(month(parsed_date, label = TRUE, abbr = TRUE), "Year", year_label)
    })
    return(labels) #Returns the specific labs
  }
}

selected_labels <- custom_date_labels(predicted.sh$Date, 0000,8, mode = "labs",num_breaks = 9)
selected_labels[selected_labels=="Sep Year 4"]<-"Aug Year 4" 
selected_dates <- custom_date_labels(predicted.sh$Date, 0000,8, mode = "breaks", num_breaks=9)
selected_dates[selected_dates=="0004-09-01"]<-"0004-08-15" 


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

#######

min <- as.Date("0002-07-01")
max <- NA
mid <- as.Date("0004-02-01")
min2 <- as.Date("0003-03-01")
min3 <- as.Date("0003-04-01")
max2 <- as.Date("0004-09-01")

st <- ymd("0000-10-01")

predicted.sh$Trial <- as.character(predicted.sh$Trial)
predicted.sh$Trial[predicted.sh$Trial == "Ear Hanging"] <- "Ear-Hanging"
predicted.sh$Trial <- as.factor(predicted.sh$Trial)


P1 <- ggplot(predicted.sh, aes(y = Sh_Height))+ 
  geom_line(aes(x = Date, color = Trial), linewidth = 2) +
  annotate("rect", xmin = min3, xmax = max2, ymin = 90, ymax = 120,
           alpha = .1,fill = "blue", color = "black")+
  annotate("text", x = mid, y=93,
           label="Adductor Harvest period", size=7)+
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
        strip.text.x = element_text(size=16, face="bold"), legend.position = 'none',
        strip.text.y = element_text(size=16, face="bold"),axis.title.x = element_blank(),
        legend.title = element_text(size=16, face="bold"), legend.text = element_text(size = 16),
        axis.title.y = element_text(face="bold", size=16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=16),
        axis.text.x=element_text(size=16,angle=35,vjust= .5),
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
selected_labels.P2 <- custom_date_labels(p.tot.p2$yrdate.2, 0000, 5, mode = "labs",num_breaks = 5)
selected_dates.P2 <- custom_date_labels(p.tot.p2$yrdate.2, 0000, 5, mode = "breaks", num_breaks=5)
selected_labels.P2[selected_labels.P2=="Dec Year 3"]<-"Nov Year 3"
selected_labels.P2[selected_labels.P2=="Apr Year 3"]<-"May Year 3"

p.tot.p2$Trial <- as.character(p.tot.p2$Trial)
p.tot.p2$Trial[p.tot.p2$Trial == "Ear Hanging"] <- "Ear-Hanging"
p.tot.p2$Trial <- as.factor(p.tot.p2$Trial)


P2 <- ggplot(p.tot.p2, aes(y = pred))+ 
  geom_point(aes(x = Date, color = Trial), size = 4) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  labs(color = "Trial")+
  xlab("Time (Month)")+
  ylab("Adductor Weight (grams)")+
  scale_x_date(breaks = selected_dates.P2, labels = selected_labels.P2) +
  scale_color_manual(values=c("#192841", "#F79256"))+
  theme(strip.text.x = element_text(size=16, face="bold"), legend.position = 'bottom',
        strip.text.y = element_text(size=16, face="bold"),axis.title.x = element_text(face="bold", size=16),
        legend.title = element_text(size=16, face="bold"), legend.text = element_text(size = 16),
        axis.title.y = element_text(face="bold", size=16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.y = element_text(size=16),
        axis.text.x=element_text(size=16,angle=35,vjust= .5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(fill='#E5E3FD'))

print(P2)


plot_grid(P2, P1,ncol = 1,
          labels = "AUTO")

# Saved as 1200x1400

###### Figure 9 #######
# Change in adductor weight offset by predicted mortality


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

p.mort$Trial <- as.character(p.mort$Trial)
p.mort$Trial[p.mort$Trial == "Ear Hanging"] <- "Ear-Hanging"
p.mort$Trial <- as.factor(p.mort$Trial)

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

# Size 1000x850
