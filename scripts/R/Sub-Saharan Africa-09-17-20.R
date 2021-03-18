library(openxlsx)
library(timeDate)
library(plm)
######################################################################################################################
# Read Primary Data                                                                                                  #
                                                                                                                     #
dta <- read.xlsx(xlsxFile="C:/Users/janin/Downloads/Sub-Saharan Africa.xlsx",na.string="NA",sheet=1)                                #
                                                                                                                     #
cntlst <- c("Angola","Benin","Botswana","Burkina Faso","Burundi","Cote dIvoire","Cameroon","Cape Verde",             # List of Country names in dataset
            "Central African Republic","Chad","Comoros","Congo - Brazzaville","Congo - Kinshasa",                    #
            "Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea",                   #
            "Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius",       #
            "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",        #
            "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Suriname","Tanzania","Togo",              #
            "Uganda","Zambia","Zimbabwe")                                                                            #
                                                                                                                     #
######################################################################################################################
# Compute basic data transformations                                                                                 #
                                                                                                                     #
dta$Pos.Rate   <- dta$Pos_daily/dta$Population*100000                                                                # Pos.Rate - new infections per 100,000
dta$Test.Rate  <- dta$Test_daily/dta$Population*100000                                                               # Test.Rate - new tests per 100,000
dta$Cum.Tests  <- dta$Pos/dta$Population*100000                                                                      # Cum.Tests - cumulative tests per 100,000
dta$Death.Rate <- dta$Death.Increase/dta$Population*100000                                                           #
dta$R.Date     <- convertToDate(dta$Date,origin = "1900-01-01")                                                      # R.Data - date converted into R format
dta$Weekend    <- 0                                                                                                  # Setup whether the day is a weekend
dta$Trend      <- 0                                                                                                  # Setup a basic trend data set
for(i in 1:nrow(dta)) {                                                                                              # Loop through complete dataset
  if(isWeekend(dta$R.Date[i]) == TRUE) dta$Weekend[i] <- 1                                                           # Set Weekend = 1 if the day is a weekend
  dta$Trend[i] <- dta$Date[i] - 43855                                                                                # Compute the trend based on Excel data variable
}                                                                                                                    #
                                                                                                                     #
######################################################################################################################
# Compute sample statistics for a period in time                                                                     #
                                                                                                                     #
strt.date <- 44023                                                                                                   # Start date excel.date(44023) = 7/11/20 (basically two month lag)
                                                                                                                     #
# -- Sample statistics for Pos.Rate - new infections                                                                 #
                                                                                                                     #
s1 <- lapply(cntlst,function(i) list(mean(dta[(dta$Country == i & dta$Date >= strt.date),"Pos.Rate"],na.rm=TRUE),    # Mean of Pos.Rate by Country
                                     sd(dta[(dta$Country == i & dta$Date >= strt.date),"Pos.Rate"],na.rm=TRUE),      # Standard deviation of Pos.Rate by Country
                                     quantile(dta[(dta$Country == i & dta$Date >= strt.date),"Pos.Rate"],            # Quantiles of Pos.Rate by Country
                                     prbs=c(0,0.10,0.25,0.50,0.75,0.90,1.0),na.rm=TRUE),                             #
                                     sum(is.na(dta[(dta$Country == i & dta$Date >= strt.date),"Pos.Rate"]))))        # Number of Pos.Rate by Country Missing
                                                                                                                     # 
# -- Sample statistics for Test.Rate                                                                                 #
                                                                                                                     #
s2 <- lapply(cntlst,function(i) list(mean(dta[(dta$Country == i & dta$Date >= strt.date),"Cum.Tests"],na.rm=TRUE),   # Mean of Test.Rate by Country
                                     sd(dta[(dta$Country == i & dta$Date >= strt.date),"Cum.Tests"],na.rm=TRUE),     # Standard deviation of Test.Rate by Country
                                     quantile(dta[(dta$Country == i & dta$Date >= strt.date),"Cum.Tests"],           # Quantiles of Test.Rate by Country
                                     prbs=c(0,0.10,0.25,0.50,0.75,0.90,1.0),na.rm=TRUE),                             #
                                     sum(is.na(dta[(dta$Country == i & dta$Date >= strt.date),"Cum.Tests"]))))       # Number of Test.Rate by Country Missing
                                                                                                                     #
# -- Aggregate sample Statistics                                                                                     #

dta.summary <- matrix(0,16,length(cntlst))
for(i in 1:length(cntlst)){
  dta.summary[1,i] <- s1[[i]][[1]]
  dta.summary[2,i] <- s1[[i]][[2]]
  for(j in 1:5) dta.summary[j+2,i] <- s1[[i]][[3]][[j]]
  dta.summary[8,i] <- s1[[i]][[4]]
  dta.summary[9,i] <- s2[[i]][[1]]
  dta.summary[10,i] <- s2[[i]][[2]]
  for(j in 1:5) dta.summary[j+10,i] <- s2[[i]][[3]][[j]]
  dta.summary[16,i] <- s2[[i]][[4]]
}

######################################################################################################################
# Econometrics of COVID model                                                                                        #
                                                                                                                     #
dta <- dta[(dta$Country != "Tanzania" & dta$Country != "Suriname" & dta$Date >= strt.date),]                         # Drop problem countries
p.dta <- pdata.frame(dta,index=c("Country","Date"))                                                                  # Create a panel dataset from the original data
                                                                                                                     #
# -- DPD estimation                                                                                                  #
gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + Cum.Tests + Weekend | Trend + I(Trend^2) + lag(Pos.Rate,8:15) |       # Estimate DPD model
               lag(Pos.Rate,2:5),data=p.dta,effect="individual",model="onestep",transformation="ld",fsm="I",         #
               na.action=omit,digits=10,robust=TRUE)                                                                 #
summary(gmm0)                                                                                                        # Print results of DPD estimation
                                                                                                                     #
gmm.cov <- vcov(gmm0)                                                                                                #
gmm.rest <- rbind(cbind(1,0,0,0),                                                                                    #
                  cbind(0,1,0,0))                                                                                    #
gmm.wald <- t(gmm.rest%*%gmm0$coefficients)%*%solve(gmm.rest%*%(gmm.cov/67)%*%t(gmm.rest))%*%                        #
            (gmm.rest%*%gmm0$coefficients)                                                                           #
                                                                                                                     #
######################################################################################################################
# Post Estimation Data Manipulation                                                                                  #
# -- Trimmed list of countries                                                                                       #
                                                                                                                     #
cntlst.2 <-  c("Angola","Benin","Botswana","Burkina Faso","Burundi","Cote dIvoire","Cameroon","Cape Verde",          #
               "Central African Republic","Chad","Comoros","Congo - Brazzaville","Congo - Kinshasa",                 #
               "Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea",                #
               "Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius",    #
              "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal",                   #
              "Seychelles","Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Togo","Uganda","Zambia",   #
              "Zimbabwe")                                                                                            #
                                                                                                                     #
######################################################################################################################
# Out.dta is my primary 'output' structure (3 dimensional array)                                                     #
# out.dta[i,j,k] - i is a date, j is a measure (i.e., number of new cases), k is a country                           #
#                                                                                                                    #
#  out.dta[j,1,i]  - Date of observation (Excel format)                                                              #
#  out.dta[j,2,i]  - Number of daily observed COVID cases (basically replicates the input data - new_cases_orig)     #
#  out.dta[j,3,i]  - Cumulative number of observed COVID cases (basically replicates the input data - cap_cum_cases) #
#  out.dta[j,4,i]  - 7 Day moving average of new COVID cases (computed based on data)                                #
#  out.dta[j,5,i]  - Rate of observed COVID cases per 100,000                                                        #
#  out.dta[j,6,i]  - Number of new COVID deaths (basically replicates the input data - new_deaths_orig)              #
#  out.dta[j,7,i]  - Cumulative number of COVID deaths (basically replicates - cap_cum_deaths)                       #
#  out.dta[j,8,i]  - 7 Day moving average of COVID deaths (computed based on data)                                   #
#  out.dta[j,9,i]  - COVID death rate per 100,000 (computed from original data)                                      #
#  out.dta[j,10,i] - Speed Number of new reported COVID cases (is this the same as [2] - new_cases_orig?)            #
#  out.dta[j,11,i] - Change in new cases, not reported but used to report acceleration                               #
#  out.dta[j,12,i] - Average acceleration over the past seven days                                                   #
#  out.dta[j,13,i] - Change in change (to compute the jerk), not reported                                            #
#  out.dta[j,14,i] - Jerk - change in acceleration                                                                   #'
#  out.dta[j,15,i] - 1 Day Persistance Effect                                                                        #
#  out.dta[j,16,i] - 7 Day Persistance Effect                                                                        #

ibeg <- 44037
iend <- 44089
l.end <- iend-ibeg+1
out.dates    <- seq(ibeg,iend,1)
out.dta      <- matrix(0,length(out.dates),(length(cntlst.2)*16))
dim(out.dta) <- c(length(out.dates),16,length(cntlst.2))
a0           <- matrix(0,2,length(cntlst.2))
for(i in 1:length(cntlst.2)) {
  for(j in 1:l.end) out.dta[j,1,i]  <- out.dates[j]                                                                    # Sample Date
  for(j in 1:l.end) out.dta[j,2,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos_daily"]      # New Cases
  for(j in 1:l.end) out.dta[j,3,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos"]            # Cumulateive Cases
  for(j in 8:l.end) out.dta[j,4,i]  <- (out.dta[j,2,i] + out.dta[j-1,2,i] + out.dta[j-2,2,i] + out.dta[j-3,2,i] +      # 7 Day Moving Average of New Cases
                                     out.dta[j-4,2,i] + out.dta[j-5,2,i] + out.dta[j-6,2,i])/7.0                       #
  for(j in 1:l.end) out.dta[j,5,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos.Rate"]       # Rate of new infections per 100,000
  for(j in 1:l.end) out.dta[j,6,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death.Increase"] # New COVID deaths
  for(j in 1:l.end) out.dta[j,7,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death"]          # Cumulative COVID deaths
  for(j in 8:l.end) out.dta[j,8,i]  <- (out.dta[j,6,i] + out.dta[j-1,6,i] + out.dta[j-2,6,i] + out.dta[j-3,6,i] +      # 7 Day Moving Average of COVID Deaths
                                     out.dta[j-4,6,i] + out.dta[j-5,6,i] + out.dta[j-6,6,i])/7.0                       #
  for(j in 1:l.end) out.dta[j,9,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death.Rate"]     # COVID death rate per 100,000
  for(j in 1:l.end) out.dta[j,10,i] <- out.dta[j,5,i]                                                                  # Speed - Rate per 100,000
  for(j in 2:l.end) out.dta[j,11,i] <- out.dta[j,5,i] - out.dta[j-1,5,i]                                               # Change in speed
  for(j in 8:l.end) out.dta[j,12,i] <- (out.dta[j,11,i] + out.dta[j-1,11,i] + out.dta[j-2,11,i] + out.dta[j-3,11,i] +  # Average Acceleration over the past 7 days
                                     out.dta[j-4,11,i] + out.dta[j-5,11,i] + out.dta[j-6,11,i])/7.0                    #
  for(j in 3:l.end) out.dta[j,13,i] <- out.dta[j,11,i] - out.dta[j-1,11,i]                                             # Change in chang for Jerk
  for(j in 9:l.end) out.dta[j,14,i] <- (out.dta[j,13,i] + out.dta[j-1,13,i] + out.dta[j-2,13,i] + out.dta[j-3,13,i] +  # Computing the Jerk
                                     out.dta[j-4,13,i] + out.dta[j-5,13,i] + out.dta[j-6,13,i])/7.0                    #
  for(j in 8:l.end) out.dta[j,15,i] <- gmm0$coefficients[1]*out.dta[j-1,2,i]                                           # 1 day persistance effect
  for(j in 8:l.end) out.dta[j,16,i] <- gmm0$coefficients[2]*out.dta[j-7,2,i]                                           # 7 day persistence effect
}

out.tble <- matrix(0,2*length(cntlst.2),15)
n.date <- c(46,53)
for(i in 1:length(cntlst.2)) {
  n.rws <- c(2*(i-1)+1,2*i)
  for(j in 1:2) {
    out.tble[n.rws[j],1]  <- out.dta[n.date[j],1,i]                                                                    # Date
    out.tble[n.rws[j],2]  <- cntlst.2[i]                                                                               # Country
    out.tble[n.rws[j],3]  <- out.dta[n.date[j],2,i]                                                                    # New Cases
    out.tble[n.rws[j],4]  <- out.dta[n.date[j],3,i]                                                                    # Cumulative Cases
    out.tble[n.rws[j],5]  <- out.dta[n.date[j],4,i]                                                                    # 7 Day Moving Average Infections
    out.tble[n.rws[j],6]  <- out.dta[n.date[j],5,i]                                                                    # Rate of New Infections
    out.tble[n.rws[j],7]  <- out.dta[n.date[j],6,i]                                                                    # Deaths
    out.tble[n.rws[j],8]  <- out.dta[n.date[j],7,i]                                                                    # Cumulative Deaths
    out.tble[n.rws[j],9]  <- out.dta[n.date[j],8,i]                                                                    # 7 Day Moving Average of Deaths
    out.tble[n.rws[j],10] <- out.dta[n.date[j],9,i]                                                                    # Death Rate per 100,000
    out.tble[n.rws[j],11] <- (out.dta[n.date[j],10,i] + out.dta[n.date[j]-1,10,i] + out.dta[n.date[j]-2,10,i] +        # Speed (Average)
                              out.dta[n.date[j]-3,10,i] + out.dta[n.date[j]-4,10,i] + out.dta[n.date[j]-5,10,i] +      #
                              out.dta[n.date[j]-6,10,i])/7.0                                                           #
    out.tble[n.rws[j],12] <- out.dta[n.date[j],12,i]                                                                   # Acceleration
    out.tble[n.rws[j],13] <- out.dta[n.date[j],14,i]                                                                   # Jerk
    out.tble[n.rws[j],14] <- (out.dta[n.date[j],15,i] + out.dta[n.date[j]-1,15,i] + out.dta[n.date[j]-2,15,i] +        # 1 Day Persistence
                              out.dta[n.date[j]-3,15,i] + out.dta[n.date[j]-4,15,i] + out.dta[n.date[j]-5,15,i] +      #
                              out.dta[n.date[j]-6,15,i])/7.0                                                           #
    out.tble[n.rws[j],15] <- (out.dta[n.date[j],16,i] + out.dta[n.date[j]-1,16,i] + out.dta[n.date[j]-2,16,i] +        # 7 Day Persistence
                              out.dta[n.date[j]-3,16,i] + out.dta[n.date[j]-4,16,i] + out.dta[n.date[j]-5,16,i] +      #
                              out.dta[n.date[j]-6,16,i])/7.0                                                           #
  }                                                                                                                    #
}                                                                                                                      #

write.csv(file="C:/Users/janin/Downloads/SSA-Temp.csv",out.tble)
