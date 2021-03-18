######################################################################################################################
# Charles B. Moss -  Code to estimate the Dynamic Panel Data models for analysis of the Dynamics of COVID-19         #
# cbmoss@ufl.edu - written 11/15/2020                                                                                #
######################################################################################################################
library(openxlsx)                                                                                                    #
library(timeDate)                                                                                                    #
library(plm)                                                                                                         #
######################################################################################################################
# Read Primary Data                                                                                                  #
                                                                                                                     #
full.dta <- read.csv(file="pgmm_input_recalc_totalsWCanada.csv")                                                     # Read data from pgmm_input_recalc_totals.csv file
regions <- as.character(unique(full.dta$Region))                                                                     # Create list of regions
                                                                                                                     #
######################################################################################################################
# Regions loop                                                                                                       #
                                                                                                                     #
n.region <- 1                                                                                                        #
while(n.region <= length(regions)) {                                                                                 # Loop Over Regions
  if(regions[n.region] != "North America") {                                                                         # Different setup for North America - Canada and the United States
    dta <- full.dta[full.dta$Region == regions[n.region],]                                                           # Setup Data for the loop by getting data from full.dta
    file.Label <- paste(regions[n.region],"-Results")                                                                # Create label for written files
    cntlst <- as.character(unique(dta$Country))                                                                      # Create a list of country names
    cntlst <- cntlst[-length(cntlst)]                                                                                # Drop the null at the end of the list
    dta <- dta[dta$Country != "",]                                                                                   # Drop data for the null county name
                                                                                                                     #
# Create test values to see that the country can be added                                                            #
    test.Stats <- lapply(cntlst,function(i) list(mean(dta[dta$Country == i,"Population"]),                           # S[1] - mean of Population
                                                 mean(dta[dta$Country == i,"Positive.Daily"]),                       # s[2] - mean of Positive Daily
                                                 mean(dta[dta$Country == i,"Tests.Daily"]),                          # s[3] - mean of Tests Daily
                                                 mean(dta[dta$Country == i,"Total.Positive"]),                       # s[4] - mean of Cumulative Positives
                                                 mean(dta[dta$Country == i,"Total.Deaths"]),                         # s[5] - mean of Cumulative Deaths
                                                 mean(dta[dta$Country == i,"Total.Tests"])))                         # s[6] - mean of Cumulative Tests
                                                                                                                     #
# Loop to drop suscpect countries                                                                                    #
    if(exists("drp.lst"))rm(drp.lst)                                                                                 # drp.lst - list of countries to be dropped
    icatch <- 0                                                                                                      # Drop if Population is zero or missing ("NA")
    for(i in 1:length(cntlst)) {                                                                                     # Drop if Positive Daily is zero or missing ("NA")
      if(is.na(test.Stats[[i]][[1]]) == TRUE | test.Stats[[i]][1] == 0 | is.na(test.Stats[[i]][[2]]) == TRUE |       # Drop if Tests Daily is zero or missing ("NA")
          test.Stats[[i]][[2]] == 0 | is.na(test.Stats[[i]][[3]]) == TRUE | test.Stats[[i]][[3]] == 0) {             #
        dta <- dta[dta$Country != cntlst[i],]                                                                        #
        if(icatch==0) drp.lst <- i else drp.lst <- rbind(drp.lst,i)                                                  #
        icatch <- icatch +1                                                                                          #
      }                                                                                                              #
    }                                                                                                                #
    if(exists("drp.lst")) cntlst <- as.character(cntlst[-drp.lst])                                                   # Drop suspect countries
    cntlst.2 <- append(cntlst,"Region")                                                                              #
                                                                                                                     #
# Set First and last date for estimation purposes                                                                    #
    dates <- sort(unique(dta$Date))                                                                                  # List of dates in the dataset
    last.date <- dates[length(dates)]                                                                                # End of period for analysis
    first.date <- dates[length(dates)-50]                                                                            # Beginning of period of analysis (first point for estimation)
                                                                                                                     #
######################################################################################################################
# Fill in missing cumulative dta                                                                                     #
                                                                                                                     #
    for(i in 1:length(cntlst)) {                                                                                     # Fill in cumulative positive cases
      if(is.na(test.Stats[[i]][[4]])) {                                                                              #
        dates.Temp <- sort(unique(dta[dta$Country == cntlst[i],"Date"]))                                             #
        dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Total.Positive"] <-                              #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Positive.Daily"]                               #
        for(j in 2:length(dates.Temp)) {                                                                             #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Total.Positive"] <-                            #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j-1]),"Total.Positive"]+                          #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Positive.Daily"]                             #
        }                                                                                                            #
      }                                                                                                              #
    }                                                                                                                #
    for(i in 1:length(cntlst)) {                                                                                     # Fill in cumulative deaths
      if(is.na(test.Stats[[i]][[5]])) {                                                                              #
        dates.Temp <- sort(unique(dta[dta$Country == cntlst[i],"Date"]))                                             #
        dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Total.Deaths"] <-                                #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Deaths.Daily"]                                 #
        for(j in 2:length(dates.Temp)) {                                                                             #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Total.Deaths"] <-                              #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j-1]),"Total.Deaths"]+                            #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Deaths.Daily"]                               #
        }                                                                                                            #
      }                                                                                                              #
    }                                                                                                                #
    for(i in 1:length(cntlst)) {                                                                                     # Fill in cumulative tests
      if(is.na(test.Stats[[i]][[6]])) {                                                                              #
        dates.Temp <- sort(unique(dta[dta$Country == cntlst[i],"Date"]))                                             #
        dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Total.Tests"] <-                                 #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[1]),"Tests.Daily"]                                  #
        for(j in 2:length(dates.Temp)) {                                                                             #
          dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Total.Tests"] <-                               #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j-1]),"Total.Tests"]+                             #
            dta[(dta$Country == cntlst[i] & dta$Date == dates.Temp[j]),"Tests.Daily"]                                #
        }                                                                                                            #
      }                                                                                                              #
    }                                                                                                                #
                                                                                                                     #
######################################################################################################################
# Compute basic data transformations                                                                                 #
                                                                                                                     #
    dta$Pos.Rate   <- dta$Positive.Daily/dta$Population.100k                                                         # Pos.Rate - new infections per 100,000
    dta$Test.Rate  <- dta$Tests.Daily/dta$Population.100k                                                            # Test.Rate - new tests per 100,000
    dta$Cum.Tests  <- dta$Total.Tests/dta$Population.100k                                                            # Cum.Tests - cumulative tests per 100,000
    dta$Cum.Cases  <- dta$Total.Positive/dta$Population.100k                                                         # Cumulative cases
    dta$Death.Rate <- dta$Deaths.Daily/dta$Population.100k                                                           # Death Rate
    dta$Cum.Deaths <- dta$Total.Deaths/dta$Population.100k                                                           # Cumulative Death
    dta$R.Date     <- convertToDate(dta$Date,origin = "1900-01-01")                                                  # R.Data - date converted into R format
    dta$Weekend    <- 0                                                                                              # Setup whether the day is a weekend
    dta$Trend      <- 0                                                                                              # Setup a basic trend data set
    dta$RecentWeek <- 0                                                                                              #
    for(i in 1:nrow(dta)) {                                                                                          # Loop through complete dataset
      if(isWeekend(dta$R.Date[i]) == TRUE) dta$Weekend[i] <- 1                                                       # Set Weekend = 1 if the day is a weekend
      dta$Trend[i] <- dta$Date[i] - dates[1]                                                                         # Compute the trend based on Excel data variable
      if(dta$Date[i] >= (last.date - 15)) dta$RecentWeek[i] <- 1                                                     # Set change two weeks before last date
    }                                                                                                                #
    dta$Recent.PosRate  <- dta$Pos.Rate*dta$RecentWeek                                                               #
    dta$TestRate.Sq     <- dta$Test.Rate^2                                                                           #
                                                                                                                     #
######################################################################################################################
# Econometrics of COVID model                                                                                        #
                                                                                                                     #
    p.dta <- pdata.frame(dta[dta$Date >= first.date,],index=c("Country","Date"))                                     # Create a panel dataset from the original data
                                                                                                                     #
# --------------------------------------- DPD estimation ------------------------------------------------------------#
                                                                                                                     #
    if(n.region != 7 & n.region != 8) {                                                                              # Different specification for n.region = 7 (South Asia) and n.region = 8 (Sub-Saharan Africa)
                                                                                                                     #
      gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + RecentWeek + lag(Recent.PosRate,c(1,7)) + Test.Rate +           #
                              Cum.Tests + Weekend | Trend + I(Trend^2) + lag(Pos.Rate,8:11) | lag(Pos.Rate,2:3),     #
                              data=p.dta,effect="individual",model="onestep", transformation="ld",fsm="I",           #
                              na.action=omit,digits=10,robust=TRUE)                                                  #
      sink(paste(regions[n.region],"- GMM.txt"))
      print(summary(gmm0))                                                                                           # Print results of DPD estimation
      sink()
                                                                                                                     #
# ----------------------------------- Wald Test for Lag Structure ---------------------------------------------------#
      gmm.cov <- vcov(gmm0)                                                                                          #
      gmm.rest <- rbind(cbind(1,0,0,0,0,0,0,0),                                                                      #
                        cbind(0,1,0,0,0,0,0,0))                                                                      #
      gmm.wald <- t(gmm.rest%*%gmm0$coefficients)%*%solve(gmm.rest%*%(gmm.cov/67)%*%t(gmm.rest))%*%                  #
                  (gmm.rest%*%gmm0$coefficients)                                                                     #
    } else if(n.region == 7) {                                                                                       #
                                                                                                                     #
      gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + RecentWeek + lag(Recent.PosRate,c(1,7)) +  Weekend |            # 
                              Trend + I(Trend^2) + lag(Pos.Rate,8:11) | lag(Pos.Rate,2:3),data=p.dta,                #
                              effect="individual",model="onestep", transformation="ld",fsm="I",na.action=omit,       #
                              digits=10,robust=TRUE)                                                                 #
      sink(paste(regions[n.region],"- GMM.txt"))
      print(summary(gmm0))                                                                                           #
      sink()
                                                                                                                     #
    } else if(n.region == 8) {                                                                                       #
      gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + RecentWeek + lag(Recent.PosRate,c(1,7)) + Weekend |             # Estimate DPD model
                              Trend + I(Trend^2) + lag(Pos.Rate,8:15) | lag(Pos.Rate,2:3),data=p.dta,                #
                              effect="individual",model="onestep",transformation="ld",fsm="I",na.action=omit,        #
                              digits=10,robust=TRUE)                                                                 #
      sink(paste(regions[n.region],"- GMM.txt"))
      print(summary(gmm0))                                                                                           #
      sink()
    } else break                                                                                                     #
#                                                                                                                    #
######################################################################################################################
# Post Estimation Data Manipulation                                                                                  #
# -- Add Regional Estimates                                                                                          #
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
                                                                                                                     #
    ibeg <- first.date - 15                                                                                          # First date for constructing of the surveillance table
    iend <- last.date                                                                                                # Last date for construction of the surveillance table
    l.end <- iend-ibeg+1                                                                                             # End point of loops
    out.dates    <- seq(ibeg,iend,1)                                                                                 # Sequence of output dates
    out.dta      <- matrix(0,length(out.dates),(length(cntlst.2)*17))                                                # Output data
    dim(out.dta) <- c(length(out.dates),17,length(cntlst.2))                                                         # 3 - dimensional array
    for(i in 1:(length(cntlst.2)-1)) {                                                                               ###########
      for(j in 1:l.end) out.dta[j,1,i]  <- out.dates[j]                                                                        # Sample Date
      for(j in 1:l.end) out.dta[j,2,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Positive.Daily"]     # New Cases
      for(j in 1:l.end) out.dta[j,3,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Positive"]     # Cumulative Cases
      for(j in 8:l.end) out.dta[j,4,i]  <- (out.dta[j,2,i] + out.dta[j-1,2,i] + out.dta[j-2,2,i] + out.dta[j-3,2,i] +          # 7 Day Moving Average of New Cases
                                            out.dta[j-4,2,i] + out.dta[j-5,2,i] + out.dta[j-6,2,i])/7.0                        #
      for(j in 1:l.end) out.dta[j,5,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos.Rate"]           # Rate of new infections per 100,000
      for(j in 1:l.end) out.dta[j,6,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Deaths.Daily"]       # New COVID deaths
      for(j in 1:l.end) out.dta[j,7,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Deaths"]       # Cumulative COVID deaths
      for(j in 8:l.end) out.dta[j,8,i]  <- (out.dta[j,6,i] + out.dta[j-1,6,i] + out.dta[j-2,6,i] + out.dta[j-3,6,i] +          # 7 Day Moving Average of COVID Deaths
                                            out.dta[j-4,6,i] + out.dta[j-5,6,i] + out.dta[j-6,6,i])/7.0                        #
      for(j in 1:l.end) out.dta[j,9,i]  <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death.Rate"]         # COVID death rate per 100,000
      for(j in 1:l.end) out.dta[j,10,i] <- out.dta[j,5,i]                                                                      # Speed - Rate per 100,000
      for(j in 2:l.end) out.dta[j,11,i] <- out.dta[j,5,i] - out.dta[j-1,5,i]                                                   # Change in speed
      for(j in 8:l.end) out.dta[j,12,i] <- (out.dta[j,11,i] + out.dta[j-1,11,i] + out.dta[j-2,11,i] + out.dta[j-3,11,i] +      # Average Acceleration over the past 7 days
                                           out.dta[j-4,11,i] + out.dta[j-5,11,i] + out.dta[j-6,11,i])/7.0                      #
      for(j in 3:l.end) out.dta[j,13,i] <- out.dta[j,11,i] - out.dta[j-1,11,i]                                                 # Change in chang for Jerk
      for(j in 9:l.end) out.dta[j,14,i] <- (out.dta[j,13,i] + out.dta[j-1,13,i] + out.dta[j-2,13,i] + out.dta[j-3,13,i] +      # Computing the Jerk
                                           out.dta[j-4,13,i] + out.dta[j-5,13,i] + out.dta[j-6,13,i])/7.0                      #
      for(j in 1:l.end) out.dta[j,17,i] <- dta[(dta$Country == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"RecentWeek"]         # Dummy variable for last two weeks
      for(j in 8:l.end) out.dta[j,15,i] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,i])*out.dta[j-1,5,i]        # 1 day persistance effect
      for(j in 8:l.end) out.dta[j,16,i] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,i])*out.dta[j-7,5,i]        # 7 day persistence effect
    }                                                                                                                          #
                                                                                                                               #
#                                                                                                                              #
# -------------------------------------------------- Regional Aggregates ------------------------------------------------------#
#                                                                                                                              #
    reg.pop <- 0                                                                                                               #
    for(i in 1:length(cntlst)) reg.pop <- reg.pop + dta[(dta$Country == cntlst[i] & dta$Date == first.date),"Population.100k"] #
    reg.totals <- length(cntlst.2)                                                                                             # Region Total Column
    for(j in 1:l.end) out.dta[j,1,reg.totals] <- out.dates[j]                                                                  # Region Total Date
    for(j in 1:l.end) out.dta[j,2,reg.totals] <- sum(out.dta[j,2,])                                                            # Region Total New Cases
    for(j in 1:l.end) out.dta[j,3,reg.totals] <- sum(out.dta[j,3,])                                                            # Region Total Cumulative Cases
    for(j in 8:l.end) out.dta[j,4,reg.totals] <- (out.dta[j,2,reg.totals] + out.dta[j-1,2,reg.totals] +                        # Region Total 7 Day Moving Average
                                                  out.dta[j-2,2,reg.totals] + out.dta[j-3,2,reg.totals] +                      #
                                                  out.dta[j-4,2,reg.totals] + out.dta[j-5,2,reg.totals] +                      #
                                                  out.dta[j-6,2,i])/7.0                                                        # 7 Day Moving Average of New Cases
    for(j in 1:l.end) out.dta[j,5,reg.totals] <- out.dta[j,2,reg.totals]/reg.pop                                               # Rate of New Infections per 100,000
    for(j in 1:l.end) out.dta[j,6,reg.totals] <- sum(out.dta[j,6,])                                                            # Region New COVID Deaths
    for(j in 1:l.end) out.dta[j,7,reg.totals] <- sum(out.dta[j,7,])                                                            # Region Cumulative COVID Deaths
    for(j in 8:l.end) out.dta[j,8,reg.totals] <- (out.dta[j,6,reg.totals] + out.dta[j-1,6,reg.totals] +                        # 7 Day Moving Average of COVID Deaths
                                                  out.dta[j-2,6,reg.totals] + out.dta[j-3,6,reg.totals] +                      #
                                                  out.dta[j-4,6,reg.totals] + out.dta[j-5,6,reg.totals] +                      #
                                                  out.dta[j-6,6,reg.totals])/7.0                                               #
    for(j in 1:l.end) out.dta[j,9,reg.totals] <- out.dta[j,6,reg.totals]/reg.pop                                               # Region COVID Deaths per 100,000
    for(j in 1:l.end) out.dta[j,10,reg.totals] <- out.dta[j,5,reg.totals]                                                      # Speed - Rate per 100,000
    for(j in 3:l.end) out.dta[j,11,reg.totals] <- out.dta[j,5,reg.totals]-out.dta[j-1,5,reg.totals]                            # Change in Speed
    for(j in 8:l.end) out.dta[j,12,reg.totals] <- (out.dta[j,11,reg.totals] + out.dta[j-1,11,reg.totals] +                     # Average Acceleration over the past 7 days
                                                   out.dta[j-2,11,reg.totals] + out.dta[j-3,11,reg.totals] +                   #
                                                   out.dta[j-4,11,reg.totals] + out.dta[j-5,11,reg.totals] +                   #
                                                   out.dta[j-6,11,reg.totals])/7.0                                             #
    for(j in 3:l.end) out.dta[j,13,reg.totals] <- out.dta[j,11,reg.totals] - out.dta[j-1,11,reg.totals]                        # Change in chang for Jerk
    for(j in 9:l.end) out.dta[j,14,reg.totals] <- (out.dta[j,13,reg.totals] + out.dta[j-1,13,reg.totals] +                     # Region Jerk
                                                   out.dta[j-2,13,reg.totals] + out.dta[j-3,13,reg.totals] +                   # Computing the Jerk
                                                   out.dta[j-4,13,reg.totals] + out.dta[j-5,13,reg.totals] +                   #
                                                   out.dta[j-6,13,reg.totals])/7.0                                             #
    for(j in 1:l.end) out.dta[j,17,reg.totals] <- dta[(dta$Country == cntlst.2[1] & dta$Date == out.dta[j,1,i]),"RecentWeek"]  # Recent Week dummy for shift in parameters
    for(j in 8:l.end) out.dta[j,15,reg.totals] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,reg.totals])*        # 1 day persistance effect
                                                   out.dta[j-1,5,reg.totals]                                                   # 
    for(j in 8:l.end) out.dta[j,16,reg.totals] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,reg.totals])*        # 7 day persistence effect
                                                   out.dta[j-7,5,reg.totals]                                                   #
#                                                                                                                              #
#                                                                                                                    ###########
# ----------------------------------- Write Output Table on Weekly Basis --------------------------------------------#
#                                                                                                                    #
    out.tble <- matrix(0,7*length(cntlst.2),15)                                                                      #
    n.date <- c(24,31,38,45,52,59,66)                                                                                #
    for(i in 1:length(cntlst.2)) {                                                                                   #
      n.rws <- (seq(7*(i-1)+1,7*i,1))                                                                                #
      for(j in 1:7) {                                                                                                #
        out.tble[n.rws[j],1]  <- out.dta[n.date[j],1,i]                                                              # Date
        out.tble[n.rws[j],2]  <- cntlst.2[i]                                                                         # Country
        out.tble[n.rws[j],3]  <- out.dta[n.date[j],2,i]                                                              # New Cases
        out.tble[n.rws[j],4]  <- out.dta[n.date[j],3,i]                                                              # Cumulative Cases
        out.tble[n.rws[j],5]  <- out.dta[n.date[j],4,i]                                                              # 7 Day Moving Average Infections
        out.tble[n.rws[j],6]  <- out.dta[n.date[j],5,i]                                                              # Rate of New Infections
        out.tble[n.rws[j],7]  <- out.dta[n.date[j],6,i]                                                              # Deaths
        out.tble[n.rws[j],8]  <- out.dta[n.date[j],7,i]                                                              # Cumulative Deaths
        out.tble[n.rws[j],9]  <- out.dta[n.date[j],8,i]                                                              # 7 Day Moving Average of Deaths
        out.tble[n.rws[j],10] <- out.dta[n.date[j],9,i]                                                              # Death Rate per 100,000
        out.tble[n.rws[j],11] <- (out.dta[n.date[j],10,i] + out.dta[n.date[j]-1,10,i] + out.dta[n.date[j]-2,10,i] +  # Speed (Average)
                                  out.dta[n.date[j]-3,10,i] + out.dta[n.date[j]-4,10,i] + out.dta[n.date[j]-5,10,i] + #
                                  out.dta[n.date[j]-6,10,i])/7.0                                                     #
        out.tble[n.rws[j],12] <- out.dta[n.date[j],12,i]                                                             # Acceleration
        out.tble[n.rws[j],13] <- out.dta[n.date[j],14,i]                                                             # Jerk
        out.tble[n.rws[j],14] <- (out.dta[n.date[j],15,i] + out.dta[n.date[j]-1,15,i] + out.dta[n.date[j]-2,15,i] +  # 1 Day Persistence
                                  out.dta[n.date[j]-3,15,i] + out.dta[n.date[j]-4,15,i] + out.dta[n.date[j]-5,15,i] + #
                                  out.dta[n.date[j]-6,15,i])/7.0                                                     #
        out.tble[n.rws[j],15] <- (out.dta[n.date[j],16,i] + out.dta[n.date[j]-1,16,i] + out.dta[n.date[j]-2,16,i] +  # 7 Day Persistence
                                  out.dta[n.date[j]-3,16,i] + out.dta[n.date[j]-4,16,i] + out.dta[n.date[j]-5,16,i] + #
                                  out.dta[n.date[j]-6,16,i])/7.0                                                     #
      }                                                                                                              #
    }                                                                                                                #
                                                                                                                     #
    write.csv(file=paste(file.Label,".csv"),out.tble)                                                                #
                                                                                                                     #
# End of country                                                                                                     #
######################################################################################################################
                                                                                                                     #
  } else if(regions[n.region] == "North America") {                                                                  #
                                                                                                                     #
######################################################################################################################
# North America                                                                                                      #
# -- United States                                                                                                   #
    dta <- full.dta[(full.dta$Region == regions[n.region] & full.dta$Country == "United States"),]                   #
    dta <- dta[dta$State.Province != "",]                                                                            #
    states <- as.character(unique(dta$State.Province))                                                               #
# Create test values to see that the country can be added                                                            #
    test.Stats <- lapply(states,function(i) list(mean(dta[dta$State.Province == i,"Population"]),                    # S[1] - mean of Population
                                                 mean(dta[dta$State.Province == i,"Positive.Daily"]),                # s[2] - mean of Positive Daily
                                                 mean(dta[dta$State.Province == i,"Tests.Daily"])))                  # s[3] - mean of Tests Daily
# Loop to drop suspect states/provinces                                                                              #
    if(exists("drp.lst"))rm(drp.lst)                                                                                 # drp.lst - list of countries to be dropped
    icatch <- 0                                                                                                      # Drop if Population is zero or missing ("NA")
    for(i in 1:length(states)) {                                                                                     # Drop if Positive Daily is zero or missing ("NA")
      if(is.na(test.Stats[[i]][[1]]) == TRUE | test.Stats[[i]][1] == 0 | is.na(test.Stats[[i]][[2]]) == TRUE |       # Drop if Tests Daily is zero or missing ("NA")
         test.Stats[[i]][[2]] == 0 | is.na(test.Stats[[i]][[3]]) == TRUE | test.Stats[[i]][[3]] == 0) {              #
        dta <- dta[dta$State.Province != states[i],]                                                                 #
        if(icatch==0) drp.lst <- i else drp.lst <- rbind(drp.lst,i)                                                  #
        icatch <- icatch +1                                                                                          #
      }                                                                                                              #
    }                                                                                                                #
    if(exists("drp.lst")) states <- as.character(states[-drp.lst])                                                   # Drop suspect states/provinces
    cntlst.2 <- append(states,"Region")                                                                              #
                                                                                                                     #
######################################################################################################################
# Compute basic data transformations                                                                                 #
                                                                                                                     #
    dta$Pos.Rate   <- dta$Positive.Daily/dta$Population.100k                                                         # Pos.Rate - new infections per 100,000
    dta$Test.Rate  <- dta$Tests.Daily/dta$Population.100k                                                            # Test.Rate - new tests per 100,000
    dta$Cum.Tests  <- dta$Total.Tests/dta$Population.100k                                                            # Cum.Tests - cumulative tests per 100,000
    dta$Cum.Cases  <- dta$Total.Positive/dta$Population.100k                                                         # Cumulative cases
    dta$Death.Rate <- dta$Deaths.Daily/dta$Population.100k                                                           # Death Rate
    dta$Cum.Deaths <- dta$Total.Deaths/dta$Population.100k                                                           # Cumulative Death
    dta$R.Date     <- convertToDate(dta$Date,origin = "1900-01-01")                                                  # R.Data - date converted into R format
    dta$Weekend    <- 0                                                                                              # Setup whether the day is a weekend
    dta$Trend      <- 0                                                                                              # Setup a basic trend data set
    dta$RecentWeek <- 0                                                                                              #
    for(i in 1:nrow(dta)) {                                                                                          # Loop through complete dataset
      if(isWeekend(dta$R.Date[i]) == TRUE) dta$Weekend[i] <- 1                                                       # Set Weekend = 1 if the day is a weekend
      dta$Trend[i] <- dta$Date[i] - dates[1]                                                                         # Compute the trend based on Excel data variable
      if(dta$Date[i] >= (last.date - 15)) dta$RecentWeek[i] <- 1                                                     # Set change two weeks before last date
    }                                                                                                                #
    dta$Recent.PosRate  <- dta$Pos.Rate*dta$RecentWeek                                                               #
    dta$TestRate.Sq     <- dta$Test.Rate^2                                                                           #
                                                                                                                     #
######################################################################################################################
# Econometrics of COVID model                                                                                        #
                                                                                                                     #
    p.dta <- pdata.frame(dta[dta$Date >= first.date,],index=c("State.Province","Date"))                              # Create a panel dataset from the original data
                                                                                                                     #
#                                                                                                                    #
# --------------------------------------- DPD estimation ------------------------------------------------------------#
#                                                                                                                    #
                                                                                                                     #
      gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + RecentWeek + lag(Recent.PosRate,c(1,7)) + Test.Rate +           #
                              Cum.Tests + Weekend | Trend + I(Trend^2) + lag(Pos.Rate,8:11) | lag(Pos.Rate,2:3),     #
                              data=p.dta,effect="individual",model="onestep", transformation="ld",fsm="I",           #
                              na.action=omit,digits=10,robust=TRUE)                                                  #
      sink("United States - GMM.txt")
      print(summary(gmm0))                                                                                           # Print results of DPD estimation
      sink()
#                                                                                                                    #
######################################################################################################################
      ibeg <- first.date - 15                                                                                        # First date for constructing of the surveillance table
      iend <- last.date                                                                                              # Last date for construction of the surveillance table
      l.end <- iend-ibeg+1                                                                                           # End point of loops
      out.dates    <- seq(ibeg,iend,1)                                                                               # Sequence of output dates
      out.dta      <- matrix(0,length(out.dates),(length(cntlst.2)*17))                                              # Output data
      dim(out.dta) <- c(length(out.dates),17,length(cntlst.2))                                                       # 3 - dimensional array
      for(i in 1:(length(cntlst.2)-1)) {                                                                             #
        for(j in 1:l.end) out.dta[j,1,i]  <- out.dates[j]                                                            # Sample Date
        for(j in 1:l.end) {                                                                                          # New Cases
          out.dta[j,2,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Positive.Daily"]} #
        for(j in 1:l.end) {                                                                                          # Cumulative Cases
          out.dta[j,3,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Positive"]} #
        for(j in 8:l.end) {                                                                                          # 7 Day Moving Average of New Cases
          out.dta[j,4,i]  <- (out.dta[j,2,i] + out.dta[j-1,2,i] + out.dta[j-2,2,i] + out.dta[j-3,2,i] +              #
                              out.dta[j-4,2,i] + out.dta[j-5,2,i] + out.dta[j-6,2,i])/7.0}                           #
        for(j in 1:l.end) {                                                                                          # Rate of new infections per 100,000
          out.dta[j,5,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos.Rate"]}       #
        for(j in 1:l.end) {                                                                                          # New COVID deaths
          out.dta[j,6,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Deaths.Daily"]}   #
        for(j in 1:l.end) {                                                                                          # Cumulative COVID deaths
          out.dta[j,7,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Deaths"]}   #  
        for(j in 8:l.end) out.dta[j,8,i]  <- (out.dta[j,6,i] + out.dta[j-1,6,i] + out.dta[j-2,6,i] +                 # 7 Day Moving Average of Deaths
                                              out.dta[j-3,6,i] + out.dta[j-4,6,i] + out.dta[j-5,6,i] +               #
                                              out.dta[j-6,6,i])/7.0                                                  #
        for(j in 1:l.end) {                                                                                          # COVID death rate per 100,000
          out.dta[j,9,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death.Rate"] }    #   
        for(j in 1:l.end) out.dta[j,10,i] <- out.dta[j,5,i]                                                          # Speed - Rate per 100,000
        for(j in 2:l.end) out.dta[j,11,i] <- out.dta[j,5,i] - out.dta[j-1,5,i]                                       # Change in speed
        for(j in 8:l.end) out.dta[j,12,i] <- (out.dta[j,11,i] + out.dta[j-1,11,i] + out.dta[j-2,11,i] +              # Average Acceleration over the past 7 days
                                              out.dta[j-3,11,i] + out.dta[j-4,11,i] + out.dta[j-5,11,i] +            #
                                              out.dta[j-6,11,i])/7.0                                                 #
        for(j in 3:l.end) out.dta[j,13,i] <- out.dta[j,11,i] - out.dta[j-1,11,i]                                     # Change in chang for Jerk
        for(j in 9:l.end) out.dta[j,14,i] <- (out.dta[j,13,i] + out.dta[j-1,13,i] + out.dta[j-2,13,i] +              # Computing the Jerk
                                              out.dta[j-3,13,i] + out.dta[j-4,13,i] + out.dta[j-5,13,i] +            #
                                              out.dta[j-6,13,i])/7.0                                                 #
        for(j in 1:l.end) {                                                                                          # Dummy variable for last two weeks
          out.dta[j,17,i] <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"RecentWeek"] }    # Recent Week dummy
        for(j in 8:l.end) out.dta[j,15,i] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,i])*            # 1 day persistance effect
                                              out.dta[j-1,5,i]                                                       #
        for(j in 8:l.end) out.dta[j,16,i] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,i])*            # 7 day persistence effect
                                              out.dta[j-7,5,i]                                                       #
      }                                                                                                              #
                                                                                                                     #
#                                                                                                                    #
# --------------------------------------------- Regional Aggregates -------------------------------------------------#
#                                                                                                                    #
      reg.pop <- 0                                                                                                   #
      for(i in 1:length(states)) {                                                                                   #
          reg.pop <- reg.pop + dta[(dta$State.Province == states[i] & dta$Date == first.date),"Population.100k"] }   #
      reg.totals <- length(cntlst.2)                                                                                 # Region Total Column
      for(j in 1:l.end) out.dta[j,1,reg.totals] <- out.dates[j]                                                      # Region Total Date
      for(j in 1:l.end) out.dta[j,2,reg.totals] <- sum(out.dta[j,2,])                                                # Region Total New Cases
      for(j in 1:l.end) out.dta[j,3,reg.totals] <- sum(out.dta[j,3,])                                                # Region Total Cumulative Cases
      for(j in 8:l.end) out.dta[j,4,reg.totals] <- (out.dta[j,2,reg.totals] + out.dta[j-1,2,reg.totals] +            # Region Total 7 Day Moving Average
                                                    out.dta[j-2,2,reg.totals] + out.dta[j-3,2,reg.totals] +          #
                                                    out.dta[j-4,2,reg.totals] + out.dta[j-5,2,reg.totals] +          #
                                                    out.dta[j-6,2,i])/7.0                                            # 7 Day Moving Average of New Cases
      for(j in 1:l.end) out.dta[j,5,reg.totals] <- out.dta[j,2,reg.totals]/reg.pop                                   # Rate of New Infections per 100,000
      for(j in 1:l.end) out.dta[j,6,reg.totals] <- sum(out.dta[j,6,])                                                # Region New COVID Deaths
      for(j in 1:l.end) out.dta[j,7,reg.totals] <- sum(out.dta[j,7,])                                                # Region Cumulative COVID Deaths
      for(j in 8:l.end) out.dta[j,8,reg.totals] <- (out.dta[j,6,reg.totals] + out.dta[j-1,6,reg.totals] +            # 7 Day Moving Average of COVID Deaths
                                                    out.dta[j-2,6,reg.totals] + out.dta[j-3,6,reg.totals] +          #
                                                    out.dta[j-4,6,reg.totals] + out.dta[j-5,6,reg.totals] +          #
                                                    out.dta[j-6,6,reg.totals])/7.0                                   #
      for(j in 1:l.end) out.dta[j,9,reg.totals] <- out.dta[j,6,reg.totals]/reg.pop                                   # Region COVID Deaths per 100,000
      for(j in 1:l.end) out.dta[j,10,reg.totals] <- out.dta[j,5,reg.totals]                                          # Speed - Rate per 100,000
      for(j in 3:l.end) out.dta[j,11,reg.totals] <- out.dta[j,5,reg.totals]-out.dta[j-1,5,reg.totals]                # Change in Speed
      for(j in 8:l.end) out.dta[j,12,reg.totals] <- (out.dta[j,11,reg.totals] + out.dta[j-1,11,reg.totals] +         # Average Acceleration over the past 7 days
                                                     out.dta[j-2,11,reg.totals] + out.dta[j-3,11,reg.totals] +       #
                                                     out.dta[j-4,11,reg.totals] + out.dta[j-5,11,reg.totals] +       #
                                                     out.dta[j-6,11,reg.totals])/7.0                                 #
      for(j in 3:l.end) out.dta[j,13,reg.totals] <- out.dta[j,11,reg.totals] - out.dta[j-1,11,reg.totals]            # Change in chang for Jerk
      for(j in 9:l.end) out.dta[j,14,reg.totals] <- (out.dta[j,13,reg.totals] + out.dta[j-1,13,reg.totals] +         # Region Jerk
                                                     out.dta[j-2,13,reg.totals] + out.dta[j-3,13,reg.totals] +       # Computing the Jerk
                                                     out.dta[j-4,13,reg.totals] + out.dta[j-5,13,reg.totals] +       #
                                                     out.dta[j-6,13,reg.totals])/7.0                                 #
      for(j in 1:l.end) {                                                                                            # Recent Week dummy for shift in parameters
        out.dta[j,17,reg.totals] <- dta[(dta$State.Province == cntlst.2[1] & dta$Date == out.dta[j,1,i]),"RecentWeek"] }    #
      for(j in 8:l.end) {                                                                                            # 1 day persistance effect
        out.dta[j,15,reg.totals] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,reg.totals])*            #
                                     out.dta[j-1,5,reg.totals]}                                                      # 
      for(j in 8:l.end) {                                                                                            # 7 day persistence effect
        out.dta[j,16,reg.totals] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,reg.totals])*            #
                                     out.dta[j-7,5,reg.totals]}                                                      #
#                                                                                                                    #      
# ----------------------------------- Write Output Table on Weekly Basis --------------------------------------------#
#                                                                                                                    #
    out.tble <- matrix(0,7*length(cntlst.2),15)                                                                      #
    n.date <- c(24,31,38,45,52,59,66)                                                                                #
    for(i in 1:length(cntlst.2)) {                                                                                   #
      n.rws <- (seq(7*(i-1)+1,7*i,1))                                                                                #
      for(j in 1:7) {                                                                                                #
        out.tble[n.rws[j],1]  <- out.dta[n.date[j],1,i]                                                              # Date
        out.tble[n.rws[j],2]  <- cntlst.2[i]                                                                         # Country
        out.tble[n.rws[j],3]  <- out.dta[n.date[j],2,i]                                                              # New Cases
        out.tble[n.rws[j],4]  <- out.dta[n.date[j],3,i]                                                              # Cumulative Cases
        out.tble[n.rws[j],5]  <- out.dta[n.date[j],4,i]                                                              # 7 Day Moving Average Infections
        out.tble[n.rws[j],6]  <- out.dta[n.date[j],5,i]                                                              # Rate of New Infections
        out.tble[n.rws[j],7]  <- out.dta[n.date[j],6,i]                                                              # Deaths
        out.tble[n.rws[j],8]  <- out.dta[n.date[j],7,i]                                                              # Cumulative Deaths
        out.tble[n.rws[j],9]  <- out.dta[n.date[j],8,i]                                                              # 7 Day Moving Average of Deaths
        out.tble[n.rws[j],10] <- out.dta[n.date[j],9,i]                                                              # Death Rate per 100,000
        out.tble[n.rws[j],11] <- (out.dta[n.date[j],10,i] + out.dta[n.date[j]-1,10,i] + out.dta[n.date[j]-2,10,i] +  # Speed (Average)
                                  out.dta[n.date[j]-3,10,i] + out.dta[n.date[j]-4,10,i] + out.dta[n.date[j]-5,10,i] + #
                                  out.dta[n.date[j]-6,10,i])/7.0                                                     #
        out.tble[n.rws[j],12] <- out.dta[n.date[j],12,i]                                                             # Acceleration
        out.tble[n.rws[j],13] <- out.dta[n.date[j],14,i]                                                             # Jerk
        out.tble[n.rws[j],14] <- (out.dta[n.date[j],15,i] + out.dta[n.date[j]-1,15,i] + out.dta[n.date[j]-2,15,i] +  # 1 Day Persistence
                                  out.dta[n.date[j]-3,15,i] + out.dta[n.date[j]-4,15,i] + out.dta[n.date[j]-5,15,i] + #
                                  out.dta[n.date[j]-6,15,i])/7.0                                                     #
        out.tble[n.rws[j],15] <- (out.dta[n.date[j],16,i] + out.dta[n.date[j]-1,16,i] + out.dta[n.date[j]-2,16,i] +  # 7 Day Persistence
                                  out.dta[n.date[j]-3,16,i] + out.dta[n.date[j]-4,16,i] + out.dta[n.date[j]-5,16,i] + #
                                  out.dta[n.date[j]-6,16,i])/7.0                                                     #
      }                                                                                                              #
    }                                                                                                                #
                                                                                                                     #
#                                                                                                                    #
    write.csv(file="United States - Results.csv",out.tble)                                                           #
#                                                                                                                    #
# End of United States                                                                                               #
######################################################################################################################
# -- Canada                                                                                                          #
    dta <- full.dta[full.dta$Country == "Canada",]                                                                   #
    dta <- dta[dta$State.Province != "",]                                                                            #
    states <- as.character(unique(dta$State.Province))                                                               #
# Create test values to see that the country can be added                                                            #
    test.Stats <- lapply(states,function(i) list(mean(dta[dta$State.Province == i,"Population"]),                    # S[1] - mean of Population
                                                 mean(dta[dta$State.Province == i,"Positive.Daily"]),                # s[2] - mean of Positive Daily
                                                 mean(dta[dta$State.Province == i,"Tests.Daily"])))                  # s[3] - mean of Tests Daily
# Loop to drop suspect states/provinces                                                                              #
    if(exists("drp.lst"))rm(drp.lst)                                                                                 # drp.lst - list of countries to be dropped
    icatch <- 0                                                                                                      # Drop if Population is zero or missing ("NA")
    for(i in 1:length(states)) {                                                                                     # Drop if Positive Daily is zero or missing ("NA")
      if(is.na(test.Stats[[i]][[1]]) == TRUE | test.Stats[[i]][1] == 0 | is.na(test.Stats[[i]][[2]]) == TRUE |       # Drop if Tests Daily is zero or missing ("NA")
         test.Stats[[i]][[2]] == 0 | is.na(test.Stats[[i]][[3]]) == TRUE | test.Stats[[i]][[3]] == 0) {              #
        dta <- dta[dta$State.Province != states[i],]                                                                 #
        if(icatch==0) drp.lst <- i else drp.lst <- rbind(drp.lst,i)                                                  #
        icatch <- icatch +1                                                                                          #
      }                                                                                                              #
    }                                                                                                                #
    if(exists("drp.lst")) states <- as.character(states[-drp.lst])                                                   # Drop suspect states/provinces
    cntlst.2 <- append(states,"Region")                                                                              #
######################################################################################################################
# Compute basic data transformations                                                                                 #
                                                                                                                     #
    dta$Pos.Rate   <- dta$Positive.Daily/dta$Population.100k                                                         # Pos.Rate - new infections per 100,000
    dta$Test.Rate  <- dta$Tests.Daily/dta$Population.100k                                                            # Test.Rate - new tests per 100,000
    dta$Cum.Tests  <- dta$Total.Tests/dta$Population.100k                                                            # Cum.Tests - cumulative tests per 100,000
    dta$Cum.Cases  <- dta$Total.Positive/dta$Population.100k                                                         # Cumulative cases
    dta$Death.Rate <- dta$Deaths.Daily/dta$Population.100k                                                           # Death Rate
    dta$Cum.Deaths <- dta$Total.Deaths/dta$Population.100k                                                           # Cumulative Death
    dta$R.Date     <- convertToDate(dta$Date,origin = "1900-01-01")                                                  # R.Data - date converted into R format
    dta$Weekend    <- 0                                                                                              # Setup whether the day is a weekend
    dta$Trend      <- 0                                                                                              # Setup a basic trend data set
    dta$RecentWeek <- 0                                                                                              #
    for(i in 1:nrow(dta)) {                                                                                          # Loop through complete dataset
      if(isWeekend(dta$R.Date[i]) == TRUE) dta$Weekend[i] <- 1                                                       # Set Weekend = 1 if the day is a weekend
      dta$Trend[i] <- dta$Date[i] - dates[1]                                                                         # Compute the trend based on Excel data variable
      if(dta$Date[i] >= (last.date - 15)) dta$RecentWeek[i] <- 1                                                     # Set change two weeks before last date
    }                                                                                                                #
    dta$Recent.PosRate  <- dta$Pos.Rate*dta$RecentWeek                                                               #
    dta$TestRate.Sq     <- dta$Test.Rate^2                                                                           #
                                                                                                                     #
######################################################################################################################
# Econometrics of COVID model                                                                                        #
                                                                                                                     #
    p.dta <- pdata.frame(dta[dta$Date >= first.date,],index=c("State.Province","Date"))                              # Create a panel dataset from the original data
#
#                                                                                                                    #
# --------------------------------------- DPD estimation ------------------------------------------------------------#
#                                                                                                                    #
    gmm0 <- pgmm(Pos.Rate ~ lag(Pos.Rate,c(1,7)) + RecentWeek + lag(Recent.PosRate,c(1,7)) + Test.Rate +             #
                              Cum.Tests + Weekend | Trend + I(Trend^2) + lag(Pos.Rate,8:11) | lag(Pos.Rate,2:3),     #
                              data=p.dta,effect="individual",model="onestep", transformation="ld",fsm="I",           #
                              na.action=omit,digits=10,robust=TRUE)                                                  #
    sink("Canada - GMM.txt")
    print(summary(gmm0))                                                                                             # Print results of DPD estimation
    sink()
#                                                                                                                    #
######################################################################################################################
    ibeg <- first.date - 15                                                                                          # First date for constructing of the surveillance table
    iend <- last.date                                                                                                # Last date for construction of the surveillance table
    l.end <- iend-ibeg+1                                                                                             # End point of loops
    out.dates    <- seq(ibeg,iend,1)                                                                                 # Sequence of output dates
    out.dta      <- matrix(0,length(out.dates),(length(cntlst.2)*17))                                                # Output data
    dim(out.dta) <- c(length(out.dates),17,length(cntlst.2))                                                         # 3 - dimensional array
    for(i in 1:(length(cntlst.2)-1)) {                                                                               #
      for(j in 1:l.end) out.dta[j,1,i]  <- out.dates[j]                                                              # Sample Date
      for(j in 1:l.end) {                                                                                            # New Cases
        out.dta[j,2,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Positive.Daily"]}   #
      for(j in 1:l.end) {                                                                                            # Cumulative Cases
        out.dta[j,3,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Positive"]}   #
      for(j in 8:l.end) {                                                                                            # 7 Day Moving Average of New Cases
        out.dta[j,4,i]  <- (out.dta[j,2,i] + out.dta[j-1,2,i] + out.dta[j-2,2,i] + out.dta[j-3,2,i] +                #
                              out.dta[j-4,2,i] + out.dta[j-5,2,i] + out.dta[j-6,2,i])/7.0}                           #
      for(j in 1:l.end) {                                                                                            # Rate of new infections per 100,000
        out.dta[j,5,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Pos.Rate"]}         #
      for(j in 1:l.end) {                                                                                            # New COVID deaths
        out.dta[j,6,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Deaths.Daily"]}     #
      for(j in 1:l.end) {                                                                                            # Cumulative COVID deaths
        out.dta[j,7,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Total.Deaths"]}     #  
      for(j in 8:l.end) out.dta[j,8,i]  <- (out.dta[j,6,i] + out.dta[j-1,6,i] + out.dta[j-2,6,i] +                   # 7 Day Moving Average of Deaths
                                              out.dta[j-3,6,i] + out.dta[j-4,6,i] + out.dta[j-5,6,i] +               #
                                              out.dta[j-6,6,i])/7.0                                                  #
      for(j in 1:l.end) {                                                                                            # COVID death rate per 100,000
        out.dta[j,9,i]  <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"Death.Rate"] }      #   
      for(j in 1:l.end) out.dta[j,10,i] <- out.dta[j,5,i]                                                            # Speed - Rate per 100,000
      for(j in 2:l.end) out.dta[j,11,i] <- out.dta[j,5,i] - out.dta[j-1,5,i]                                         # Change in speed
      for(j in 8:l.end) out.dta[j,12,i] <- (out.dta[j,11,i] + out.dta[j-1,11,i] + out.dta[j-2,11,i] +                # Average Acceleration over the past 7 days
                                              out.dta[j-3,11,i] + out.dta[j-4,11,i] + out.dta[j-5,11,i] +            #
                                              out.dta[j-6,11,i])/7.0                                                 #
      for(j in 3:l.end) out.dta[j,13,i] <- out.dta[j,11,i] - out.dta[j-1,11,i]                                       # Change in chang for Jerk
      for(j in 9:l.end) out.dta[j,14,i] <- (out.dta[j,13,i] + out.dta[j-1,13,i] + out.dta[j-2,13,i] +                # Computing the Jerk
                                              out.dta[j-3,13,i] + out.dta[j-4,13,i] + out.dta[j-5,13,i] +            #
                                              out.dta[j-6,13,i])/7.0                                                 #
      for(j in 1:l.end) {                                                                                            # Dummy variable for last two weeks
        out.dta[j,17,i] <- dta[(dta$State.Province == cntlst.2[i] & dta$Date == out.dta[j,1,i]),"RecentWeek"] }      # Recent Week dummy
      for(j in 8:l.end) out.dta[j,15,i] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,i])*              # 1 day persistance effect
                                            out.dta[j-1,5,i]                                                         #
      for(j in 8:l.end) out.dta[j,16,i] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,i])*              # 7 day persistence effect
                                            out.dta[j-7,5,i]                                                         #
    }                                                                                                                #
                                                                                                                     #
#                                                                                                                    #
# --------------------------------------------- Regional Aggregates -------------------------------------------------#
#                                                                                                                    #
    reg.pop <- 0                                                                                                     #
    for(i in 1:length(states)) {                                                                                     #
      reg.pop <- reg.pop + dta[(dta$State.Province == states[i] & dta$Date == first.date),"Population.100k"] }       #
    reg.totals <- length(cntlst.2)                                                                                   # Region Total Column
    for(j in 1:l.end) out.dta[j,1,reg.totals] <- out.dates[j]                                                        # Region Total Date
    for(j in 1:l.end) out.dta[j,2,reg.totals] <- sum(out.dta[j,2,])                                                  # Region Total New Cases
    for(j in 1:l.end) out.dta[j,3,reg.totals] <- sum(out.dta[j,3,])                                                  # Region Total Cumulative Cases
    for(j in 8:l.end) out.dta[j,4,reg.totals] <- (out.dta[j,2,reg.totals] + out.dta[j-1,2,reg.totals] +              # Region Total 7 Day Moving Average
                                                    out.dta[j-2,2,reg.totals] + out.dta[j-3,2,reg.totals] +          #
                                                    out.dta[j-4,2,reg.totals] + out.dta[j-5,2,reg.totals] +          #
                                                    out.dta[j-6,2,i])/7.0                                            # 7 Day Moving Average of New Cases
    for(j in 1:l.end) out.dta[j,5,reg.totals] <- out.dta[j,2,reg.totals]/reg.pop                                     # Rate of New Infections per 100,000
    for(j in 1:l.end) out.dta[j,6,reg.totals] <- sum(out.dta[j,6,])                                                  # Region New COVID Deaths
    for(j in 1:l.end) out.dta[j,7,reg.totals] <- sum(out.dta[j,7,])                                                  # Region Cumulative COVID Deaths
    for(j in 8:l.end) out.dta[j,8,reg.totals] <- (out.dta[j,6,reg.totals] + out.dta[j-1,6,reg.totals] +              # 7 Day Moving Average of COVID Deaths
                                                    out.dta[j-2,6,reg.totals] + out.dta[j-3,6,reg.totals] +          #
                                                    out.dta[j-4,6,reg.totals] + out.dta[j-5,6,reg.totals] +          #
                                                    out.dta[j-6,6,reg.totals])/7.0                                   #
    for(j in 1:l.end) out.dta[j,9,reg.totals] <- out.dta[j,6,reg.totals]/reg.pop                                     # Region COVID Deaths per 100,000
    for(j in 1:l.end) out.dta[j,10,reg.totals] <- out.dta[j,5,reg.totals]                                            # Speed - Rate per 100,000
    for(j in 3:l.end) out.dta[j,11,reg.totals] <- out.dta[j,5,reg.totals]-out.dta[j-1,5,reg.totals]                  # Change in Speed
    for(j in 8:l.end) out.dta[j,12,reg.totals] <- (out.dta[j,11,reg.totals] + out.dta[j-1,11,reg.totals] +           # Average Acceleration over the past 7 days
                                                     out.dta[j-2,11,reg.totals] + out.dta[j-3,11,reg.totals] +       #
                                                     out.dta[j-4,11,reg.totals] + out.dta[j-5,11,reg.totals] +       #
                                                     out.dta[j-6,11,reg.totals])/7.0                                 #
    for(j in 3:l.end) out.dta[j,13,reg.totals] <- out.dta[j,11,reg.totals] - out.dta[j-1,11,reg.totals]              # Change in chang for Jerk
    for(j in 9:l.end) out.dta[j,14,reg.totals] <- (out.dta[j,13,reg.totals] + out.dta[j-1,13,reg.totals] +           # Region Jerk
                                                     out.dta[j-2,13,reg.totals] + out.dta[j-3,13,reg.totals] +       # Computing the Jerk
                                                     out.dta[j-4,13,reg.totals] + out.dta[j-5,13,reg.totals] +       #
                                                     out.dta[j-6,13,reg.totals])/7.0                                 #
    for(j in 1:l.end) {                                                                                              # Recent Week dummy for shift in parameters
      out.dta[j,17,reg.totals] <- dta[(dta$State.Province == cntlst.2[1] & dta$Date == out.dta[j,1,i]),"RecentWeek"] } #
    for(j in 8:l.end) {                                                                                              # 1 day persistance effect
      out.dta[j,15,reg.totals] <- (gmm0$coefficients[1]+gmm0$coefficients[4]*out.dta[j,17,reg.totals])*              #
        out.dta[j-1,5,reg.totals]}                                                                                   # 
    for(j in 8:l.end) {                                                                                              # 7 day persistence effect
      out.dta[j,16,reg.totals] <- (gmm0$coefficients[2]+gmm0$coefficients[5]*out.dta[j,17,reg.totals])*              #
        out.dta[j-7,5,reg.totals]}                                                                                   #
#                                                                                                                    #      
# ----------------------------------- Write Output Table on Weekly Basis --------------------------------------------#
#                                                                                                                    #
    out.tble <- matrix(0,7*length(cntlst.2),15)                                                                      #
    n.date <- c(24,31,38,45,52,59,66)                                                                                #
    for(i in 1:length(cntlst.2)) {                                                                                   #
      n.rws <- (seq(7*(i-1)+1,7*i,1))                                                                                #
      for(j in 1:7) {                                                                                                #
        out.tble[n.rws[j],1]  <- out.dta[n.date[j],1,i]                                                              # Date
        out.tble[n.rws[j],2]  <- cntlst.2[i]                                                                         # Country
        out.tble[n.rws[j],3]  <- out.dta[n.date[j],2,i]                                                              # New Cases
        out.tble[n.rws[j],4]  <- out.dta[n.date[j],3,i]                                                              # Cumulative Cases
        out.tble[n.rws[j],5]  <- out.dta[n.date[j],4,i]                                                              # 7 Day Moving Average Infections
        out.tble[n.rws[j],6]  <- out.dta[n.date[j],5,i]                                                              # Rate of New Infections
        out.tble[n.rws[j],7]  <- out.dta[n.date[j],6,i]                                                              # Deaths
        out.tble[n.rws[j],8]  <- out.dta[n.date[j],7,i]                                                              # Cumulative Deaths
        out.tble[n.rws[j],9]  <- out.dta[n.date[j],8,i]                                                              # 7 Day Moving Average of Deaths
        out.tble[n.rws[j],10] <- out.dta[n.date[j],9,i]                                                              # Death Rate per 100,000
        out.tble[n.rws[j],11] <- (out.dta[n.date[j],10,i] + out.dta[n.date[j]-1,10,i] + out.dta[n.date[j]-2,10,i] +  # Speed (Average)
                                    out.dta[n.date[j]-3,10,i] + out.dta[n.date[j]-4,10,i] + out.dta[n.date[j]-5,10,i] + #
                                    out.dta[n.date[j]-6,10,i])/7.0                                                   #
        out.tble[n.rws[j],12] <- out.dta[n.date[j],12,i]                                                             # Acceleration
        out.tble[n.rws[j],13] <- out.dta[n.date[j],14,i]                                                             # Jerk
        out.tble[n.rws[j],14] <- (out.dta[n.date[j],15,i] + out.dta[n.date[j]-1,15,i] + out.dta[n.date[j]-2,15,i] +  # 1 Day Persistence
                                    out.dta[n.date[j]-3,15,i] + out.dta[n.date[j]-4,15,i] + out.dta[n.date[j]-5,15,i] + #
                                    out.dta[n.date[j]-6,15,i])/7.0                                                   #
        out.tble[n.rws[j],15] <- (out.dta[n.date[j],16,i] + out.dta[n.date[j]-1,16,i] + out.dta[n.date[j]-2,16,i] +  # 7 Day Persistence
                                    out.dta[n.date[j]-3,16,i] + out.dta[n.date[j]-4,16,i] + out.dta[n.date[j]-5,16,i] + #
                                    out.dta[n.date[j]-6,16,i])/7.0                                                   #
      }                                                                                                              #
    }                                                                                                                #
write.csv(file="Canada - Results.csv",out.tble)                                                                      #
#                                                                                                                    #
# End of Canada                                                                                                      #
######################################################################################################################    
  }

n.region <- n.region + 1
}
