#### calculating thermocline from routines ####

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
routines.temp = get(load(file = "./data/formatted data/routines temp/2018 2019 2024 routines temp.RData"))

# format the data so it matches Steve's code
routines.temp = routines.temp %>% mutate(DoY = decimal_date(date), RoundDoY = yday(date))

# break up into four lake_years
routines.templ18 = routines.temp %>% filter(lake == "L" & year == 2018) %>% select(-lake, -year)
routines.tempr18 = routines.temp %>% filter(lake == "R" & year == 2018) %>% select(-lake, -year)
routines.tempr19 = routines.temp %>% filter(lake == "R" & year == 2019) %>% select(-lake, -year)
routines.templ19 = routines.temp %>% filter(lake == "L" & year == 2019) %>% select(-lake, -year)
routines.tempr24 = routines.temp %>% filter(lake == "R" & year == 2024) %>% select(-lake, -year)
routines.templ24 = routines.temp %>% filter(lake == "L" & year == 2024) %>% select(-lake, -year)





#==============================================================================#
#### Peter 2018 ####


tchain.data = routines.tempr18

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
  # Thermocline by -2 degrees/m rule
  twotest = RootSpline1(x=d1zchain,y=dT,y0=-2,verbose=FALSE) 
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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


thermosr18 = thermos %>% mutate(lake = "Peter", year = 2018, source = "routines")  






#==============================================================================#
#### Paul 2018 ####


tchain.data = routines.templ18

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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


thermosl18 = thermos %>% mutate(lake = "Paul", year = 2018, source = "routines")  









#==============================================================================#
#### Paul 2019 ####


tchain.data = routines.templ19

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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
  labs(title = paste0("Paul 2019", " thermocline"), x = "Day of 2019", y = "depth (m)")+
  scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                      values = c("steelblue1", "darkblue", "red")) +   
  theme(legend.position="right")


thermosl19 = thermos %>% mutate(lake = "Paul", year = 2019, source = "routines")  






#==============================================================================#
#### Peter 2019 ####


tchain.data = routines.tempr19

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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
  labs(title = paste0("Peter 2019", " thermocline"), x = "Day of 2019", y = "depth (m)")+
  scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                      values = c("steelblue1", "darkblue", "red")) +   
  theme(legend.position="right")


thermosr19 = thermos %>% mutate(lake = "Peter", year = 2019, source = "routines")  







#==============================================================================#
#### Peter 2024 ####


tchain.data = routines.tempr24

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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
  labs(title = paste0("Peter 2024", " thermocline"), x = "Day of 2024", y = "depth (m)")+
  scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                      values = c("steelblue1", "darkblue", "red")) +   
  theme(legend.position="right")


thermosr24 = thermos %>% mutate(lake = "Peter", year = 2024, source = "routines")  



#==============================================================================#
#### Paul 2024 ####


tchain.data = routines.templ24

Zchain = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)


# select day range
doy0 = min(tchain.data$RoundDoY) # 1st day of season
doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
doyvec = sort(unique(tchain.data$RoundDoY))
ndoy = length(doyvec)

# Select time window within each day
# want from 12 to 4
#tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)

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
  Tprof = colMeans(Tc[ ,5:18],na.rm=T)
  
  # linear first derivative, use first point for surface
  dT = diff(Tprof)
  d1zchain = (Zchain[1:12] + Zchain[2:13])/2  # get the midpoints
  
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
  d2zchain = (d1zchain[1:11] + d1zchain[2:12])/2
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
  labs(title = paste0("Paul 2024", " thermocline"), x = "Day of 2024", y = "depth (m)")+
  scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                      values = c("steelblue1", "darkblue", "red")) +   
  theme(legend.position="right")


thermosl24 = thermos %>% mutate(lake = "Paul", year = 2024, source = "routines")  



#==========================================================================================# 
#### combine dataframes ####  

thermos.all.routines = rbind(thermosl18, thermosl19, thermosl24, thermosr18, thermosr19, thermosr24)

#==========================================================================================# 
#### Plot all lakes ####
ggplot(thermos.all.routines, aes(x = doy, y = depth, color = thermocline))+
  geom_point()+
  geom_line()+
  theme_classic()+
  scale_y_reverse(limits = c(5, 0))+
  xlim(136, max(thermos$doy))+
  facet_wrap(lake~year)+
  labs(title = paste0("routines", " thermocline"), x = "day of year", y = "depth (m)")+
  scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                      values = c("steelblue1", "darkblue", "red")) +   
  theme(legend.position="right")



