#with perspiration heat loss
#2:  Add humidity

library(deSolve)
library(ggplot2)
require(gridExtra)


sweatingR <- function(T){

  if (T <27.5) {
    return (11.95) 
  } else if (T < 35) {
    return (1.1665* T^2 - 64.166*T + 894.35 )
  } else {
    return (4.2976*T - 71.289 )
  }
}

sVaporPressure = function(T) {
  return (610.5* exp(17.27*T/(237.7+T)))
}

relativeHumidity = function(AH,T){
  return (AH/1000* 461.5*(T+273)/sVaporPressure(T))
}

evaporationRate = function(RH,T){
  if (RH>1) return (0)
  else return (329*exp(0.0063*(T-20))*(1-RH))
}

apparentTemperature = function(T,RH){
  return (T + 0.33*sVaporPressure(T)/100*RH - 4.00)
}

ACRtoAT <- function(ACR,iTemp,iRH) {
  START<-0; FINISH<-30; STEP<-0.2
  simtime <- seq(START, FINISH, by=STEP)
  
  auxs    <- c(aInputTemp=iTemp, aACRate=ACR, aVolume = 15000,
               aNsheep=20000, aPowerSheep=68,
               aInputRH = iRH)
  
  auxs["aHeatCapacity"] <- auxs["aVolume"] * 1.2 * 1005
  
  stocks  <- c(sTemp=as.numeric(auxs["aInputTemp"]),
               sRH=as.numeric(auxs["aInputRH"]))
  
  model <- function(time, stocks, auxs){
    with(as.list(c(stocks, auxs)),{
      aRateVent = (aInputTemp - sTemp)*aACRate/60
      aHRateSheep = aNsheep*aPowerSheep*60
      
      
      aWaterMass = min(sweatingR((aInputTemp+sTemp)/2),
                       evaporationRate(sRH,(aInputTemp+sTemp)/2))* 1.3*aNsheep/60
      aHRateEvaporation = aWaterMass*2260
      
      
      aAH_Evaporation = aWaterMass / aVolume
      aRH_Evaporation = relativeHumidity(aAH_Evaporation,sTemp)
      aRH_Vent = (aInputRH/sVaporPressure(sTemp)*sVaporPressure(aInputTemp)-sRH)*aACRate/60
      
      #
      d_sRH_dt = aRH_Evaporation+aRH_Vent
      #    if (sRH>=1) {
      #      aHRateEvaporation = 0
      #    }
      if (sRH+d_sRH_dt>=1) {
        #      aHRateEvaporation = (1-sRH)/d_sRH_dt*aHRateEvaporation
        #      aHRateEvaporation=0
        #      aRH_Evaporation = 0
        d_sRH_dt = 1 - sRH
      }
      if (sRH+d_sRH_dt<0) {d_sRH_dt = - sRH}
      
      
      
      d_sTemp_dt = aRateVent + (aHRateSheep- aHRateEvaporation)/aHeatCapacity
      aRH_Temp = sRH*( sVaporPressure(sTemp)/sVaporPressure(sTemp+d_sTemp_dt) - 1)
      d_sRH_dt = aRH_Evaporation+aRH_Vent+aRH_Temp
      aApparentTemperature = apparentTemperature(sTemp,sRH)
      
      return (list(c(d_sTemp_dt,d_sRH_dt),aRateVent=aRateVent,
                   aHEvap = aHRateEvaporation, aWM = aWaterMass,
                   aAT=aApparentTemperature)
      )
    })
  }
  
  o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                    parms=auxs, method="euler"))
  
  return (tail(o,1)[c("sTemp","sRH","aAT")])
}

headers<-c("iACR","iT","iRH","oT","oRH","finalAT")
#names(df)<-headers

df <- as.data.frame(matrix(,ncol=4,nrow=0))
#dataAC = data.frame(ACR,Temp,RH,AT)
ACRl <- c(20,40)
Templ <- c(20,30)
RHl <- c(0.5,0.7)
for (iacr in ACRl) {
  for (itemp in Templ) {
    for (irh in RHl)
    {
      results = ACRtoAT(iacr,itemp,irh)
      df <- rbind(df,c(iacr,itemp,irh,
                       results[["sTemp"]],
                       results[["sRH"]],
                       results[["aAT"]]))
    }
  }
}
names(df)<-headers
df
