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

# try Maximum Likelihood Estimation
# first the function
decay <- function(TC) { # return the error in the fit
  # trials
  trials <- exp(-DT_draws$t_Delay/TC)
  errs <- trials - DT_draws$f_cooldown
  
  RMSE <- sqrt(mean(errs)^2)
  return(RMSE)
}
  

decay(TC=60)

fit <- optim (60, decay, method="Brent", lower=1, upper=1000)

fit$par

TC.est <- fit$par
DT_est <- data.table(t_Delay=0:600)
DT_est[,f_cooldown:= exp(-t_Delay/TC.est)]

decay(TC.est)

# with non-linear fit
ggplot() +
  geom_point(data=DT_draws, aes(t_Delay, f_cooldown, colour = model)) + 
  geom_line(data=DT_est,aes(t_Delay, f_cooldown)) +
  labs(x="time since previous draw (minutes)", y="fraction remaining") +
  ggtitle("Tankless water heater cooldown") +
  scale_color_discrete(name = "water heater models") +
  scale_x_continuous(breaks = seq(from =0, to = 600, by = 60))
  

ggsave(filename = "decay.png")



