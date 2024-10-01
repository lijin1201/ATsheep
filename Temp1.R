#with perspiration heat loss

library(deSolve)

START<-0; FINISH<-30; STEP<-0.5
simtime <- seq(START, FINISH, by=STEP)

stocks  <- c(sTemp=30)

auxs    <- c(aInputTemp=30, aACRate=20,
             aHeatCapacity=15000*1.2*1005,
             aNsheep=20000, aPowerSheep=68)

sweatingR <- function(T){
#  return (20)
  if (T <27.5) {
    return (11.95) 
  } else if (T < 35) {
    return (1.1665* T^2 - 64.166*T + 894.35 )
  } else {
    return (4.2976*T - 71.289 )
  }
}

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aRateVent = (aInputTemp - sTemp)*aACRate/60
    aHRateSheep = aNsheep*aPowerSheep*60
    
    aHRateEvaporation = sweatingR((aInputTemp+sTemp)/2)* 2260 * 2 *aNsheep/60

    d_sTemp_dt = aRateVent + (aHRateSheep- aHRateEvaporation)/aHeatCapacity
    
    return (list(c(d_sTemp_dt),aRateEvent=aRateVent, aHEvap = aHRateEvaporation))
  })
}

o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                  parms=auxs, method="euler"))


ggplot()+
  geom_line(data=o,aes(time,sTemp,color="Temperature"))+
  ylab("Temperature")+
  xlab("Min")+labs(color="")+
  theme(legend.position="bottom")

