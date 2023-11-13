################################################################################################
## MLE method
################################################################################################
setwd("~/Isotope metabolism/Isotope project/Marcuc metab scripts")
source("legacy/metab.support.R")
library(pbapply) 

ggplot(filter(df, datetime < as.POSIXct("2022-09-02")))+
  geom_line(aes(x = datetime, y = DO_mgL))

# set parameters
error.type='OE' # observation error or process error specification for MLE (OE fits initial DO)
logged=T # whether or not to log /exponentiate parameter estimates for constraining positive /negative 
bootstrap=T # whether or not to bootstrap the fits to produce distribution of fitted parameters (uncertainty in parameter estimate)
n.boot=100 # how many iterations in bootstrapping if bootstrap = T 
ar1.resids=T # maintain autocorrelation in residuals when bootstrapping if True 
guesses=c(1E-5,1E-5) # MLE guesses for gppCoeff and rCoeff 
# guesses=c(1E-1,1E-4,1E-4) # MLE guess for gppMaxCoeff, gppCoeff, and rCoeff for light saturating function
nDaysSim=1 #number of days over which to estimate metab coefficients 
optim_method='Nelder-Mead'
sunrise=F # if True, fit model from sunrise to sunrise 
#dates <- dates[5]
y = df |>
  filter(minute(datetime) == 0 | minute(datetime) == 30)

fs.metab <- function(dates){
  x <- filter(y,as.Date(datetime) == as.Date(dates))
  
  #x$datetime <- seq(x$datetime[1],x$datetime[length(x$datetime)],length.out=nrow(x))
  #get sampling frequency for BD15
  
  # x$datetime <- as.POSIXct(x$datetime,tz="UTC")
  
  output <- my.metab.mle(do.obs=x$DO_mgL, do.sat=x$DO_perc, k.gas=x$k_gas, z.mix=x$z_mix,
                         irr=x$PAR, wtr=x$Temp_C, datetime=x$datetime,irr_day=mean(x$PAR),
                         wtr_day=mean(x$Temp_C), error.type=error.type,logged=logged, 
                         bootstrap=bootstrap,n.boot= n.boot,ar1.resids=ar1.resids, guesses=guesses,
                         daysSim=48,optim_method=optim_method,
                         sunrise=sunrise)
 return(output)
}

#id <- names(do)#track lake ID
#metlist <- list()#to save metabolism frame
#for(i in 1:length(do)){
 # print(id[i])
  
#  x1 <- do[[i]]#data frame with DO timeseries for the evaluated lake
  
 # colnames(x1)[colnames(x1)=="par.wm2"] <- "par.ue"
#  colnames(x1)[colnames(x1)=="rad.wm2"] <- "par.ue"

  ####ONLY FOR BD18####
 # x1$Zmix <- if_else(is.na(x1$Zmix)==TRUE,max(x1$Zmix,na.rm=T),x1$Zmix)
#  x1 <- select(x1,-c(Zseas,ZmetU,ZmetL,Thyp,do.mgl.hyp,
                    # temp.c.hyp,do.pct.sat.hyp,do.sat.hyp))
  
 # x1 <- rename(x1,do=do.mgl.epi,do.sat=do.sat.epi,zmix.m=Zmix,temp=temp.c.epi)
  #####
  #some fiexed
  #x1$date <- as.Date(x1$datetime)
  #x1$k.gas <- x1$ko2
  #y <- x1[complete.cases(x1),]
  dates <- as.POSIXct(levels(as.factor(y$datetime)),tz="UTC") 

  output <- pblapply(dates, fs.metab)
  
  parms <- list()
  metab <- list()
  for(k in 1:length(dates)){
    x <- output[[k]]
    parms[[k]] <- x[[1]]
    metab[[k]] <- x[[2]]
    
  }
  aja <- bind_rows(metab)
  bajs <- bind_rows(parms)
  metabol <- cbind(dates,aja,bajs)
  metabol <- data.frame(metabol)
  metabol$reliability <- if_else(metabol$GPP <0 | metabol$R > 0, "unreal","real")
  
}
ggplot(metabol)+
  geom_line(aes(x = dates, y = GPP))+
  geom_line(aes(x = dates, y = R, color = "R"))+
  geom_line(aes(x = dates, y = NEP, color = "NEP"))
  
setwd("~/Isotope metabolism/Isotope project/Abisko 2018/complete dataset/bootstrap meta")
for(i in 1:length(metlist)){
  x <- metlist[[i]]
  id <- x$lake[1]
  write_xlsx(x,paste0(id,".metab.marcus.xlsx"))
}
