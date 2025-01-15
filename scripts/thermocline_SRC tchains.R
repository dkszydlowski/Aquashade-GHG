#### calculating thermocline from tchains ####

### CURRENTLY, PAUL 2018 IS EXPERIMENTAL TO TRY TO USE FULL TCHAIN



# Linear interpolation to find thermoclines
# code adapted from SRC

if (!require(npreg, quietly = TRUE)) install.packages('npreg')
suppressWarnings(library(npreg))

if (!require(stats, quietly = TRUE)) install.packages('stats')
suppressWarnings(library(stats))

if (!require(tidyverse, quietly = TRUE)) install.packages('tidyverse')
suppressWarnings(library(tidyverse))


  # read in the temp chain data formatted to be hourly
  hourly.temp = get(load(file = "./data/formatted data/hourly tchain/2018 and 2019 hourly tchain.RData"))

  # format the data so it matches Steve's code
  hourly.temp = hourly.temp %>% mutate(DoY = decimal_date(as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")), RoundDoY = yday(datetime))
  
  # break up into three lake_years
  hourly.temp.l18 = hourly.temp %>% filter(Lake == "Paul" & Year == 2018) %>% select(-Lake, -Year)
  hourly.temp.r18 = hourly.temp %>% filter(Lake == "Peter" & Year == 2018) %>% select(-Lake, -Year)
  hourly.temp.r19 = hourly.temp %>% filter(Lake == "Peter" & Year == 2019) %>% select(-Lake, -Year)
  
  ### check the example data for formatting
  # load(file = "./data/example data from Steve/Peter2015_Tchain.Rdata")
  
  #ex.data = read.csv("./data/example data from Steve/rLakeAnalyzer tchain hourly Paul 2024-08-26.csv", sep = "\t")
  
  # Function ================================================================================================================
  # Use linear interpolation to find depth where slope = -2 degrees / meter
  #
  # https://stackoverflow.com/questions/52655729/get-x-value-given-y-value-general-root-finding-for-linear-non-linear-interpol
  #
  ## given (x, y) data, find x where the linear interpolation crosses y = y0
  ## the default value y0 = 0 implies root finding
  ## since linear interpolation is just a linear spline interpolation
  ## the function is named RootSpline1
  #
  # The same website has Rootspline3 for cubic splines
  # =========================================================================================================================
  RootSpline1 <- function (x, y, y0, verbose = TRUE) {
    if (is.unsorted(x)) {
      ind <- order(x)
      x <- x[ind]; y <- y[ind]
    }
    z <- y - y0
    ## which piecewise linear segment crosses zero?
    k <- which(z[-1] * z[-length(z)] <= 0)
    ## analytical root finding
    xr <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
    ## make a plot?
    if (verbose) {
      windows()
      plot(x, y, "l"); abline(h = y0, lty = 2)
      points(xr, rep.int(y0, length(xr)))
    }
    ## return roots
    return(unname(xr))
  }
  # ================================================================================================
  
  # Read data
  
  # tchain.data is all rows of Tchain data for Peter 2015
  # Zchain is depths (m) for for tchain.data[,6:14]
  
  # exmple data
  #save(tchain.data,Zchain,file='Peter2015_Tchain.Rdata')
  #load(file='Peter2015_Tchain.Rdata')
  
  tchain.data = hourly.temp.l18
  
  Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5)
  
  tchain.data = tchain.data %>% mutate(hour = hour(datetime))
  
  # select day range
  doy0 = min(tchain.data$RoundDoY) # 1st day of season
  doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
  doyvec = c(doy0:doyN)
  ndoy = length(doyvec)
  
  # Select time window within each day
  # want from 12 to 4
  tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    
    # which doy is the current of the loop iteration
    today = doyvec[id]
    
    Tc = tchain.data %>% filter(RoundDoY == today)
    
    # use profiles only, not other columns
    Tprof = colMeans(Tc[ ,2:12],na.rm=T)
    
    # linear first derivative, use first point for surface
    dT = diff(Tprof)
    d1zchain = (Zchain[1:9] + Zchain[2:10])/2  # get the midpoints
    
    # Thermocline by -2 degrees/m rule
    twotest = RootSpline1(x=d1zchain,y=dT,y0=-2,verbose=F) 
    if(length(twotest) == 1) {
      ZT1a[id] = twotest
      ZT1b[id] = twotest
    }
    if(length(twotest) == 2) {
      ZT1a[id] = twotest[1]
      ZT1b[id] = twotest[2]
    }
    
    # linear second derivative, most negative at steepest slope
    d2T = diff(dT)
    d2zchain = (d1zchain[1:8] + d1zchain[2:9])/2
    # find index of minima
    imin = which.min(d2T)
    
    
    tryCatch({
      # This is the line that might produce an error
      ZT2[id] <- d2zchain[imin]
    }, error = function(e) {
      # Print a message indicating which id caused the error
      message(paste("Error with id", id, ": ", e$message))
      
      error.days <<- c(error.days, id)
      
    })
    
  }  # end loop over DOY *************************************************************

  
  # make a dataframe of the values from Steve
  thermos = data.frame(doy = doyvec, ZT1a = ZT1a, ZT1b = ZT1b, ZT2 = ZT2)
  
  thermos = thermos %>% pivot_longer(cols = c("ZT1a", "ZT1b", "ZT2"), names_to = "thermocline", values_to = "depth")
  
  # convert error days to doy
  error.days = error.days + doy0-1
  
  thermos = thermos %>% filter(!doy %in% error.days)
  
  # replace all values where the thermocline was not calculated with NA
  
  # zt1a = "shallow -2 line"
  # zt1b = "deep or only -2 line"
  # zt2 = "steepest slope"
  
  
  ggplot(thermos, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Paul 2018", " thermocline"), x = "Day of 2018", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  

  thermosl18 = thermos %>% mutate(lake = "Paul", year = 2018, source = "tchain")  
  

  
  
  
  
  
  
  
  
  
  
#==============================================================================#
  #### Peter 2018 ####
  
  
  tchain.data = hourly.temp.r18
  
  Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  
  tchain.data = tchain.data %>% mutate(hour = hour(datetime))
  
  # select day range
  doy0 = min(tchain.data$RoundDoY) # 1st day of season
  doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
  doyvec = c(doy0:doyN)
  ndoy = length(doyvec)
  
  # Select time window within each day
  # want from 12 to 4
  tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    
    # which doy is the current of the loop iteration
    today = doyvec[id]
    
    Tc = tchain.data %>% filter(RoundDoY == today)
    
    # use profiles only, not other columns
    Tprof = colMeans(Tc[ ,2:10],na.rm=T)
    
    # linear first derivative, use first point for surface
    dT = diff(Tprof)
    d1zchain = (Zchain[1:9] + Zchain[2:10])/2  # get the midpoints
    
    # Thermocline by -2 degrees/m rule
    twotest = RootSpline1(x=d1zchain,y=dT,y0=-2,verbose=F) 
    if(length(twotest) == 1) {
      ZT1a[id] = twotest
      ZT1b[id] = twotest
    }
    if(length(twotest) == 2) {
      ZT1a[id] = twotest[1]
      ZT1b[id] = twotest[2]
    }
    
    # linear second derivative, most negative at steepest slope
    d2T = diff(dT)
    d2zchain = (d1zchain[1:8] + d1zchain[2:9])/2
    # find index of minima
    imin = which.min(d2T)
    
    
    tryCatch({
      # This is the line that might produce an error
      ZT2[id] <- d2zchain[imin]
    }, error = function(e) {
      # Print a message indicating which id caused the error
      message(paste("Error with id", id, ": ", e$message))
      
      error.days <<- c(error.days, id)
      
    })
    
  }  # end loop over DOY *************************************************************
  
  
  # make a dataframe of the values from Steve
  thermos = data.frame(doy = doyvec, ZT1a = ZT1a, ZT1b = ZT1b, ZT2 = ZT2)
  
  thermos = thermos %>% pivot_longer(cols = c("ZT1a", "ZT1b", "ZT2"), names_to = "thermocline", values_to = "depth")
  
  # convert error days to doy
  error.days = error.days + doy0-1
  
  thermos = thermos %>% filter(!doy %in% error.days)
  
  # replace all values where the thermocline was not calculated with NA
  
  # zt1a = "shallow -2 line"
  # zt1b = "deep or only -2 line"
  # zt2 = "steepest slope"
  
  
  ggplot(thermos, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Peter 2018", " thermocline"), x = "Day of 2018", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")

  
  thermosr18 = thermos %>% mutate(lake = "Peter", year = 2018, source = "tchain")  
  

  
  
  
  
  #==============================================================================#
  #### Peter 2019 ####
  
  
  tchain.data = hourly.temp.r19
  
  Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6)
  
  tchain.data = tchain.data %>% mutate(hour = hour(datetime))
  
  # select day range
  doy0 = min(tchain.data$RoundDoY) # 1st day of season
  doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
  doyvec = c(doy0:doyN)
  ndoy = length(doyvec)
  
  # Select time window within each day
  # want from 12 to 4
  tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    
    # which doy is the current of the loop iteration
    today = doyvec[id]
    
    Tc = tchain.data %>% filter(RoundDoY == today)
    
    # use profiles only, not other columns
    Tprof = colMeans(Tc[ ,2:10],na.rm=T)
    
    # linear first derivative, use first point for surface
    dT = diff(Tprof)
    d1zchain = (Zchain[1:10] + Zchain[2:11])/2  # get the midpoints
    
    # Thermocline by -2 degrees/m rule
    twotest = RootSpline1(x=d1zchain,y=dT,y0=-2,verbose=F) 
    if(length(twotest) == 1) {
      ZT1a[id] = twotest
      ZT1b[id] = twotest
    }
    if(length(twotest) == 2) {
      ZT1a[id] = twotest[1]
      ZT1b[id] = twotest[2]
    }
    
    # linear second derivative, most negative at steepest slope
    d2T = diff(dT)
    d2zchain = (d1zchain[1:9] + d1zchain[2:10])/2
    # find index of minima
    imin = which.min(d2T)
    
    
    tryCatch({
      # This is the line that might produce an error
      ZT2[id] <- d2zchain[imin]
    }, error = function(e) {
      # Print a message indicating which id caused the error
      message(paste("Error with id", id, ": ", e$message))
      
      error.days <<- c(error.days, id)
      
    })
    
  }  # end loop over DOY *************************************************************
  
  
  # make a dataframe of the values from Steve
  thermos = data.frame(doy = doyvec, ZT1a = ZT1a, ZT1b = ZT1b, ZT2 = ZT2)
  
  thermos = thermos %>% pivot_longer(cols = c("ZT1a", "ZT1b", "ZT2"), names_to = "thermocline", values_to = "depth")
  
  # convert error days to doy
  error.days = error.days + doy0-1
  
  thermos = thermos %>% filter(!doy %in% error.days)
  
  # replace all values where the thermocline was not calculated with NA
  
  # zt1a = "shallow -2 line"
  # zt1b = "deep or only -2 line"
  # zt2 = "steepest slope"
  
  
  ggplot(thermos, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Peter 2018", " thermocline"), x = "Day of 2018", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
  
  thermosr19 = thermos %>% mutate(lake = "Peter", year = 2019, source = "tchain")  
  
  
#==========================================================================================# 
#### combine dataframes ####  

thermos.all = rbind(thermosl18, thermosr18, thermosr19)
  
#==========================================================================================# 
#### Plot all lakes ####
  ggplot(thermos.all, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    facet_wrap(lake~year)+
    labs(title = paste0("tchain", " thermocline"), x = "day of year", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
  
  # read in 2024 tchain data and combine #
  thermo.tchain.24 = read.csv("R:/Cascade/Data/2024 Dailies/Daily_data_converted/2024_thermocline.csv")
  
  thermo.tchain.24 = thermo.tchain.24 %>% rename(lake = Lake, year = Year) %>% 
    mutate(source = "tchain")

  ### combine with the routines thermoclines ###
  thermos.all = rbind(thermos.all, thermo.tchain.24, thermos.all.routines)
  
  write.csv(thermos.all, "./data/formatted data/thermoclines/thermoclines routines and tchain 2025-01-15.csv", row.names= FALSE)
  
  
  