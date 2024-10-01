
library(deSolve)

START<-0; FINISH<-30; STEP<-0.5
simtime <- seq(START, FINISH, by=STEP)

stocks  <- c(sTemp=20)

auxs    <- c(aInputTemp=20, aACRate=20,
             aHeatCapacity=15000*1.2*1005,
             aNsheep=20000, aPowerSheep=68)
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aRateVent = (aInputTemp - sTemp)*aACRate/60
    aRateSheep = aNsheep*aPowerSheep*60/aHeatCapacity
    d_sTemp_dt = aRateVent + aRateSheep
    
    return (list(c(d_sTemp_dt),aRateEvent=aRateVent))
  })
}

o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                  parms=auxs, method="euler"))


ggplot()+
  geom_line(data=o,aes(time,sTemp,color="Temperature"))+
  ylab("Temperature")+
  xlab("Min")+labs(color="")+
  theme(legend.position="bottom")