#### function for calculating thermocline ####

# Linear interpolation to find thermoclines
# code adapted from SRC

if (!require(npreg, quietly = TRUE)) install.packages('npreg')
suppressWarnings(library(npreg))

if (!require(stats, quietly = TRUE)) install.packages('stats')
suppressWarnings(library(stats))

if (!require(tidyverse, quietly = TRUE)) install.packages('tidyverse')
suppressWarnings(library(tidyverse))


if(lake == "Peter" | lake == "Tuesday" | lake == "Paul"){
  # read in the temp chain data formatted to be hourly
  hourly.temp = read.csv(paste("./data/formatted data/2024/", lake,
                               "/tchain rLakeAnalyzer/", date, "/rLakeAnalyzer tchain hourly ",
                               lake , " ", date, ".csv", sep = ""  ), sep = "\t")
  
  # format the data so it matches Steve's code
  hourly.temp = hourly.temp %>% mutate(DoY = decimal_date(as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")), RoundDoY = yday(datetime))
  
  
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
  #save(tchain.data,Zchain,file='Peter2015_Tchain.Rdata')
  #load(file='Peter2015_Tchain.Rdata')
  
  tchain.data = hourly.temp
  
  Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  
  tchain.data = tchain.data %>% mutate(hour = hour(datetime))
  
  # select day range
  doy0 = 136 # 1st day of season
  doyN = yday(as.Date(date)) -1 # current date, subtract 1 because usually downloaded in morning
  doyvec = c(doy0:doyN)
  ndoy = length(doyvec)
  
  # Select time window within each day
  tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    today = doyvec[id]
    # extract the rows for today
    # Tc = subset(tchain.data,subset=(DoY <= (today + tstop) & DoY >= (today + tstart)) )
    # use profiles only
    Tc = tchain.data %>% filter(RoundDoY == today)
    
    Tprof = colMeans(Tc[ ,2:10],na.rm=T)
    
    # linear first derivative, use first point for surface
    dT = diff(Tprof)
    d1zchain = (Zchain[1:8] + Zchain[2:9])/2  # midpoints
    
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
    d2zchain = (d1zchain[1:7] + d1zchain[2:8])/2
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
  # 
  # plot thermocline indicators versus day of year
  # windows(width=10,height=6)
  # par(mfrow=c(1,1),mar=c(4, 4.2, 1, 2) + 0.1,cex.axis=1.6,cex.lab=1.8)
  # yrange = range(c(ZT1a,ZT1b,ZT2))
  # plot(doyvec,ZT1a,ylim=yrange,type='b',lwd=2,pch=16,col='deepskyblue',xlab='Day of 2024',ylab='Thermocline (m)',
  #      main = paste(lake, "Lake thermocline"))
  # points(doyvec,ZT1b,ylim=yrange,type='b',lwd=2,pch=16,col='blue')
  # points(doyvec,ZT2,type='b',lwd=2,pch=16,col='red')
  # legend('bottomright',cex=1.5,bty='n',
  #        legend=c('shallow -2 line','deep or only -2 line','steepest slope'),
  #        lwd=c(2,2,2),pch=c(16,16,16),col=c('deepskyblue','blue','red'))
  
  
  
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
    labs(title = paste0(lake, " thermocline"), x = "Day of 2024", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
}






######## PLOT THERMOCLINES FOR ALL LAKES #############
if(lake == "all"){
  
  lakes = c("Peter", "Paul", "Tuesday")
  
  for(i in 1:3){
    
    cur.lake = lakes[i]
    
    # read in the temp chain data formatted to be hourly
    hourly.temp = read.csv(paste("./data/formatted data/2024/", cur.lake,
                                 "/tchain rLakeAnalyzer/", date, "/rLakeAnalyzer tchain hourly ",
                                 cur.lake , " ", date, ".csv", sep = ""  ), sep = "\t")
    
    # format the data so it matches Steve's code
    hourly.temp = hourly.temp %>% mutate(DoY = decimal_date(as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")), RoundDoY = yday(datetime))
    
    
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
    #save(tchain.data,Zchain,file='Peter2015_Tchain.Rdata')
    #load(file='Peter2015_Tchain.Rdata')
    
    tchain.data = hourly.temp
    
    Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
    
    tchain.data = tchain.data %>% mutate(hour = hour(datetime))
    
    # select day range
    doy0 = 136 # 1st day of season
    doyN = yday(as.Date(date)) -1 # current date, subtract 1 because usually downloaded in morning
    doyvec = c(doy0:doyN)
    ndoy = length(doyvec)
    
    # Select time window within each day
    tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
    
    # vectors for results
    ZT1a = rep(0,ndoy)
    ZT1b = rep(0,ndoy)
    ZT2 = rep(0,ndoy)
    id = 1
    
    error.days = c()
    
    for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
      today = doyvec[id]
      # extract the rows for today
      # Tc = subset(tchain.data,subset=(DoY <= (today + tstop) & DoY >= (today + tstart)) )
      # use profiles only
      Tc = tchain.data %>% filter(RoundDoY == today)
      
      Tprof = colMeans(Tc[ ,2:10],na.rm=T)
      
      # linear first derivative, use first point for surface
      dT = diff(Tprof)
      d1zchain = (Zchain[1:8] + Zchain[2:9])/2  # midpoints
      
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
      d2zchain = (d1zchain[1:7] + d1zchain[2:8])/2
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
    # # 
    # # plot thermocline indicators versus day of year
    # windows(width=10,height=6)
    # par(mfrow=c(1,1),mar=c(4, 4.2, 1, 2) + 0.1,cex.axis=1.6,cex.lab=1.8)
    # yrange = range(c(ZT1a,ZT1b,ZT2))
    # plot(doyvec,ZT1a,ylim=yrange,type='b',lwd=2,pch=16,col='deepskyblue',xlab='Day of 2024',ylab='Thermocline (m)',
    #      main = paste(cur.lake, "Lake thermocline"))
    # points(doyvec,ZT1b,ylim=yrange,type='b',lwd=2,pch=16,col='blue')
    # points(doyvec,ZT2,type='b',lwd=2,pch=16,col='red')
    # legend('bottomright',cex=1.5,bty='n',
    #        legend=c('shallow -2 line','deep or only -2 line','steepest slope'),
    #        lwd=c(2,2,2),pch=c(16,16,16),col=c('deepskyblue','blue','red'))
    
    
    
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
    
    
    if(cur.lake == "Peter"){thermosR = thermos}
    if(cur.lake == "Paul"){thermosL = thermos}
    if(cur.lake == "Tuesday"){thermosT = thermos}
    
  }
  
  
  Rthermo = ggplot(thermosR, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Peter", " thermocline"), x = "Day of 2024", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
  Lthermo = ggplot(thermosL, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Paul", " thermocline"), x = "Day of 2024", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
  Tthermo = ggplot(thermosT, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0("Tuesday", " thermocline"), x = "Day of 2024", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  
  
  print(ggarrange(Lthermo, Rthermo, Tthermo, ncol = 1, nrow =3, common.legend = TRUE, legend = "right"))
  
  png("C:/Users/Danny/Box/2024 Cascade Outputs/thermoclines all lakes.png",
      height = 10, width = 6, units = "in", res = 300)
  print(ggarrange(Lthermo, Rthermo, Tthermo, ncol = 1, nrow =3, common.legend = TRUE, legend = "right"))
  dev.off()
  
}




