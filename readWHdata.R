fn_script = "readWHdata.R"
# read water heater data from Decay_Plotting2.csv
# which came from Peter Grant, Davis Energy Group 

# Jim Lutz "Mon Jul 31 14:22:12 2017"

# make sure all packages loaded 
# at some point I should turn this into a common file 
source("setup.R")

# read the csv file 
DT_draws <- fread(file= "./Decay_Plotting.2.csv")

names(DT_draws)
str(DT_draws)

# calculate fraction cooled down to ambient
DT_draws[,f_cooldown:= (T_delay - T_Amb)/(T_Inital - T_Amb)] # watch the typos

# change t_Delay to minutes
DT_draws[,t_Delay:=t_Delay/60]

# remove Navien NP-240, it has a small tank
unique(DT_draws$model)
DT_draws <- DT_draws[model != "Navien NP-240",] # that leave 46 draws, 5 models

# scatter plot, for verification
ggplot(DT_draws, aes(t_Delay, f_cooldown, colour = model)) + geom_point()

# try finding a non-linear fit
fit <- nls(formula=f_cooldown ~ exp(-t_Delay/TC), 
    data = DT_draws
    )

summary(fit)
str(fit)
str(summary(fit))
TC.est <- summary(fit)$coefficients[[1]]

DT_est <- data.table(t_Delay=0:600)
DT_est[,f_cooldown:= exp(-t_Delay/TC.est)]


# with non-linear fit
ggplot() +
  geom_point(data=DT_draws, aes(t_Delay, f_cooldown, colour = model)) + 
  geom_line(data=DT_est,aes(t_Delay, f_cooldown))

# save data.table
save(DT_WHs , file=paste0(wd_data,"DT_WHs.RData"))



