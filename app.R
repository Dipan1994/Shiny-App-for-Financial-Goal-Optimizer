library(shiny)
library(ggplot2)
library(mvtnorm)
library(scales)
library(shinybusy)

# setwd("C://Users/HP/Desktop/Finance App")

# Stocks_Mean = c(.134274,.193745,.179223)
# Stocks_Cov_Mat = matrix(c(0.05911,0.06349,0.06686,0.06349,0.08411,0.09023,0.06686,0.09023,0.08411),
#                         nrow = 3)
# 
# set.seed(1)
# mvn_Stocks = rmvnorm(100000,Stocks_Mean,Stocks_Cov_Mat)
# 
# LC_Returns = mvn_Stocks[,1]
# MC_Returns = mvn_Stocks[,2]
# SC_Returns = mvn_Stocks[,3]
# 
# Bond_Returns = rnorm(100000, 0.088413,0.033425)
# RE_Returns = rnorm(100000,.044705,0.066365)
# FD_Returns = 0.05
# Cash_Returns = 0.035
# 
# Inflation = 0.05
# 
# LC_Returns = matrix(LC_Returns,nrow = 1000,ncol = 100)
# MC_Returns = matrix(LC_Returns,nrow = 1000,ncol = 100)
# SC_Returns = matrix(LC_Returns,nrow = 1000,ncol = 100)
# Bond_Returns = matrix(LC_Returns,nrow = 1000,ncol = 100)
# RE_Returns = matrix(LC_Returns,nrow = 1000,ncol = 100)
# save(LC_Returns,MC_Returns,SC_Returns,Bond_Returns,RE_Returns,FD_Returns,Cash_Returns,Inflation,
#      file = "Returns.RData")

# Weights = c(0.2,.1,.1,.1,0,.3,.2)


# Weight_Mat = data.frame(LC = 1,MC = 0, SC= 0 ,Bond = 0, RE = 0, FD = 0, Cash = 0)
# count = 1
# 
# for (i in 0:10) {
#   for (j in 0:10) {
#     for (k in 0:10) {
#       for (l in 0:10) {
#         for (m in 0:10) {
#           for (n in 0:10) {
#             if((i+j+k+l+m+n)*10==100) {
#               Weight_Mat[count,] = c(i,j,k,l,0,m,n)/10
#               count = count+1
#             }
#           }
#         }
#       }
#     }
#   }
# } 
# 
# save(Weight_Mat,file = "Weights.RData")

load("Returns.RData")
load("Weights.RData")

#Sims

Sims = function(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
                Contribution, Contri_Growth, Contri_Start, Contri_End,
                T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
                T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
                T5_Amount, T5_Start,T5_End,Annual_Expenses, Weights) {
  
  
  Weights = as.vector(Weights)
  # Weights = c(Weights[1],Weights[2],Weights[3],Weights[4],Weights[5],Weights[6],Weights[7])
  
  Max_Horizon = max(T1_End,T2_End,T3_End,T4_End,T5_End)
  
  
  Balance = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  Balance[,1] = LC_Balance+MC_Balance+SC_Balance+Bond_Balance+FD_Balance+RE_Balance+Cash_Balance
  
  LC_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  MC_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  SC_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  Bond_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  FD_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  RE_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  Cash_Bal = matrix(rep(0,(Max_Horizon+1)*1000),nrow = 1000)
  
  LC_Bal[,1] = LC_Balance
  MC_Bal[,1] = MC_Balance
  SC_Bal[,1] = SC_Balance
  Bond_Bal[,1] = Bond_Balance
  FD_Bal[,1] = FD_Balance
  RE_Bal[,1] = RE_Balance
  Cash_Bal[,1] = Cash_Balance
  
  Emergency_Fund = pmin(Cash_Bal[,1],Annual_Expenses/2)
  Cash_Bal[,1] = Cash_Bal[,1] - Emergency_Fund
  Emergency_Fund_Left = pmax(Annual_Expenses/2- Emergency_Fund,0)
  
  Contri_Array = rep(0,Max_Horizon)
  
  WD_Mat = matrix(rep(0,Max_Horizon*1000),nrow = 1000)
  
  for (i in 1:Max_Horizon) {
  # for (i in 1:4) {
    Contri = ifelse(i<=Contri_End && i>=Contri_Start,1,0)* Contribution *((1+Contri_Growth)/(1+Inflation))^(i-1)
    Contri_Array[i] = Contri
    
    LC_Bal[,i+1] = LC_Bal[,i]*(1+LC_Returns[,i])/(1+Inflation) + Contri * as.numeric(Weights[1])
    MC_Bal[,i+1] = MC_Bal[,i]*(1+MC_Returns[,i])/(1+Inflation) + Contri * as.numeric(Weights[2])
    SC_Bal[,i+1] = SC_Bal[,i]*(1+SC_Returns[,i])/(1+Inflation) + Contri * as.numeric(Weights[3])
    Bond_Bal[,i+1] = Bond_Bal[,i]*(1+Bond_Returns[,i])/(1+Inflation) + Contri * as.numeric(Weights[4])
    RE_Bal[,i+1] = RE_Bal[,i]*(1+RE_Returns[,i])/(1+Inflation) + Contri * as.numeric(Weights[5])
    FD_Bal[,i+1] = FD_Bal[,i]*(1+FD_Returns)/(1+Inflation) + Contri * as.numeric(Weights[6])
    Cash_Bal[,i+1] = Cash_Bal[,i]*(1+Cash_Returns)/(1+Inflation) + Contri * as.numeric(Weights[7])
    
    Emergency_Fund = Emergency_Fund * (1+Cash_Returns)/(1+Inflation)
    Emergency_Fund = Emergency_Fund+pmin(Cash_Bal[,i+1],Emergency_Fund_Left)
    Cash_Bal[,i+1] = pmax(Cash_Bal[,i+1]-Emergency_Fund_Left,0)
    Emergency_Fund_Left = pmax(Annual_Expenses/2- Emergency_Fund,0)
    
    WD = 0 
    if (T1_Start<=i&&T1_End>=i) {
      WD = WD+T1_Amount
    } 
    if (T2_Start<=i&&T2_End>=i) {
      WD = WD+T2_Amount
    } 
    if (T3_Start<=i&&T3_End>=i) {
      WD = WD+T3_Amount
    } 
    if (T4_Start<=i&&T4_End>=i) {
      WD = WD+T4_Amount
    } 
    if (T5_Start<=i&&T5_End>=i) {
      WD = WD+T5_Amount
    } 
    
    if (WD > 0) {
      WD_Mat[,i] = apply(cbind(LC_Bal[,i+1],MC_Bal[,i+1],SC_Bal[,i+1],Bond_Bal[,i+1],RE_Bal[,i+1],FD_Bal[,i+1],Cash_Bal[,i+1]),
                         1, function(x) ifelse(sum(x)<=WD, sum(x),WD))
      
      Pro_Rata = cbind(LC_Bal[,i+1],MC_Bal[,i+1],SC_Bal[,i+1],Bond_Bal[,i+1],RE_Bal[,i+1],FD_Bal[,i+1],Cash_Bal[,i+1])/(LC_Bal[,i+1]+MC_Bal[,i+1]+SC_Bal[,i+1]+Bond_Bal[,i+1]+RE_Bal[,i+1]+FD_Bal[,i+1]+Cash_Bal[,i+1])
      LC_Bal[,i+1] = LC_Bal[,i+1] - Pro_Rata[,1]*WD
      MC_Bal[,i+1] = MC_Bal[,i+1] - Pro_Rata[,2]*WD
      SC_Bal[,i+1] = SC_Bal[,i+1] - Pro_Rata[,3]*WD
      Bond_Bal[,i+1] = Bond_Bal[,i+1] - Pro_Rata[,4]*WD
      RE_Bal[,i+1] = RE_Bal[,i+1] - Pro_Rata[,5]*WD
      FD_Bal[,i+1] = FD_Bal[,i+1] - Pro_Rata[,6]*WD
      Cash_Bal[,i+1] = Cash_Bal[,i+1] - Pro_Rata[,7]*WD
      
      LC_Bal[,i+1] = pmax(LC_Bal[,i+1],0)
      MC_Bal[,i+1] = pmax(MC_Bal[,i+1],0) 
      SC_Bal[,i+1] = pmax(SC_Bal[,i+1],0)
      Bond_Bal[,i+1] = pmax(Bond_Bal[,i+1],0) 
      RE_Bal[,i+1] = pmax(RE_Bal[,i+1] ,0)
      FD_Bal[,i+1] = pmax(FD_Bal[,i+1] ,0)
      Cash_Bal[,i+1] = pmax(Cash_Bal[,i+1],0)
    }
    
    Balance[,i+1] = LC_Bal[,i+1]+MC_Bal[,i+1]+SC_Bal[,i+1]+Bond_Bal[,i+1]+RE_Bal[,i+1]+FD_Bal[,i+1]+Cash_Bal[,i+1]
    
  }

  return(list(Balance,LC_Bal,MC_Bal,SC_Bal,Bond_Bal,RE_Bal,FD_Bal,Cash_Bal,Emergency_Fund,WD_Mat))
}

Permutations = function(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
                        Contribution, Contri_Growth, Contri_Start, Contri_End,
                        T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
                        T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
                        T5_Amount, T5_Start,T5_End,Annual_Expenses, CL,Weights_Mat) {
  

  
  POS = matrix(rep(0,nrow(Weights_Mat)*max(T1_End,T2_End,T3_End,T4_End,T5_End)),nrow = nrow(Weights_Mat))
  Frac_Target_Met = matrix(rep(0,nrow(Weights_Mat)*max(T1_End,T2_End,T3_End,T4_End,T5_End)),nrow = nrow(Weights_Mat))
  
  WD_Reqd = rep(0,max(T1_End,T2_End,T3_End,T4_End,T5_End))
  
  for (i in 1:max(T1_End,T2_End,T3_End,T4_End,T5_End)) {
    WD_Reqd[i] = ifelse(T1_Start<=i&&T1_End>=i,
                        T1_Amount,0) + ifelse(T2_Start<=i&&T2_End>=i,
                                            T2_Amount,0) + ifelse(T3_Start<=i&&T3_End>=i,T3_Amount,0) + ifelse(T4_Start<=i&&T4_End>=i,
                                                                                                                                T4_Amount,0) + ifelse(T5_Start<=i&&T5_End>=i,T5_Amount,0)
  }
  
  for (i in 1:nrow(Weights_Mat)) {
    Output = Sims(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
                    Contribution, Contri_Growth, Contri_Start, Contri_End,
                    T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
                    T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
                    T5_Amount, T5_Start,T5_End,Annual_Expenses, Weights_Mat[i,])
    
    Bal_Mat = Output[[1]]
    
    POS[i,] = apply(Bal_Mat[,-1],2,function(x) 1-sum(x==0)/length(x))
    
    WD_Mat = Output[[10]]
    
    WD_Mat = apply(WD_Mat,2,function(x) quantile(x,1-CL))
    
    Frac_Target_Met[i,] = apply(cbind(WD_Mat,WD_Reqd),1,function(x) ifelse(x[2]==0,1,x[1]/x[2])) 
  
  }
  return(list(POS,Frac_Target_Met))
}



Plots = function(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
                 Contribution, Contri_Growth, Contri_Start, Contri_End,
                 T1_Amount, T1_Start,T1_End,T1_Wt,T2_Amount, T2_Start,T2_End,T2_Wt,
                 T3_Amount, T3_Start,T3_End,T3_Wt,T4_Amount, T4_Start,T4_End,T4_Wt,
                 T5_Amount, T5_Start,T5_End,T5_Wt,Annual_Expenses, CL, Comf_.5, Comf_.25, Comf_.1) {
  
  if (Comf_.1=="No") {
    Weight_Mat  = Weight_Mat[Weight_Mat$SC==0,]
    Weight_Mat  = Weight_Mat[Weight_Mat$MC==0,]
    Weight_Mat  = Weight_Mat[Weight_Mat$LC<=.5,]
  } else if (Comf_.25=="No") {
    Weight_Mat  = Weight_Mat[Weight_Mat$SC==0,]
    Weight_Mat  = Weight_Mat[Weight_Mat$MC==0,]
  } else if (Comf_.5=="No") {
    Weight_Mat  = Weight_Mat[Weight_Mat$SC==0,]
  } 
  
  Out_put = Permutations(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
                         Contribution, Contri_Growth, Contri_Start, Contri_End,
                         T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
                         T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
                         T5_Amount, T5_Start,T5_End,Annual_Expenses, CL, Weight_Mat)
  
  
  POS = Out_put[[1]]
  Frac_Target_Met = Out_put[[2]]
  
  Year_Weights = rep(0,max(T1_End,T2_End,T3_End,T4_End,T5_End))
  
  for (i in 1:max(T1_End,T2_End,T3_End,T4_End,T5_End)) {
    
    if (T1_Start<=i&&T1_End>=i) {
      Year_Weights[i] = Year_Weights[i] + T1_Wt 
    } 
    if (T2_Start<=i&&T2_End>=i) {
      Year_Weights[i] = Year_Weights[i] + T2_Wt
    } 
    if (T3_Start<=i&&T3_End>=i) {
      Year_Weights[i] = Year_Weights[i] + T3_Wt
    } 
    if (T4_Start<=i&&T4_End>=i) {
      Year_Weights[i] = Year_Weights[i] + T4_Wt
    } 
    if (T5_Start<=i&&T5_End>=i) {
      Year_Weights[i] = Year_Weights[i] + T5_Wt
    } 
    
    Year_Weights[i] = min(Year_Weights[i],10)
    
  }
  
  POS_Wt = apply(POS, 1, function(x) sum(x*Year_Weights))
  
  Winner = Weight_Mat[which.max(POS_Wt),] * Contribution
  
  Contri_Mat = data.frame(Year = rep(c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End)),6), 
                          Investment = c(rep("Large Cap",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                         rep("Mid Cap",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                         rep("Small Cap",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                         rep("Bond",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                         rep("FD",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                         rep("Cash",max(T1_End,T2_End,T3_End,T4_End,T5_End))),
                          Contribution = rep(0,6*max(T1_End,T2_End,T3_End,T4_End,T5_End)))
  
  for (i in 1:nrow(Contri_Mat)) {
    if (Contri_Mat$Year[i]>=Contri_Start && Contri_Mat$Year[i]<=Contri_End) {
      if (Contri_Mat$Investment[i]=="Large Cap") {
        Contri_Mat$Contribution[i] = Winner[1] * (1+Contri_Growth)^Contri_Mat$Year[i]
      }
      if (Contri_Mat$Investment[i]=="Mid Cap") {
        Contri_Mat$Contribution[i] = Winner[2]* (1+Contri_Growth)^Contri_Mat$Year[i]
      }
      if (Contri_Mat$Investment[i]=="Small Cap") {
        Contri_Mat$Contribution[i] = Winner[3]* (1+Contri_Growth)^Contri_Mat$Year[i]
      }
      if (Contri_Mat$Investment[i]=="Bond") {
        Contri_Mat$Contribution[i] = Winner[4]* (1+Contri_Growth)^Contri_Mat$Year[i]
      }
      if (Contri_Mat$Investment[i]=="FD") {
        Contri_Mat$Contribution[i] = Winner[6]* (1+Contri_Growth)^Contri_Mat$Year[i]
      }
      if (Contri_Mat$Investment[i]=="Cash") {
        Contri_Mat$Contribution[i] = Winner[7]* (1+Contri_Growth)^Contri_Mat$Year[i]
      }
    }
  }
  
  Contri_Mat$Contribution = as.numeric(Contri_Mat$Contribution)
  p1 = ggplot() +
    geom_bar(data = Contri_Mat,aes(x = Year,y=Contribution,fill = Investment),stat = 'identity') +
    scale_x_continuous(breaks = c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End))) +
    theme_bw() +
    labs(title = "Best Investment Scheme") +
    theme(legend.position = 'top')
  
  
  WD_Targets = rep(0,max(T1_End,T2_End,T3_End,T4_End,T5_End))
  for (i in 1:max(T1_End,T2_End,T3_End,T4_End,T5_End)) {
    if (T1_Start<=i&&T1_End>=i) {
      WD_Targets[i] = WD_Targets[i] + T1_Amount
    } 
    if (T2_Start<=i&&T2_End>=i) {
      WD_Targets[i] = WD_Targets[i] + T2_Amount
    } 
    if (T3_Start<=i&&T3_End>=i) {
      WD_Targets[i] = WD_Targets[i] + T3_Amount
    } 
    if (T4_Start<=i&&T4_End>=i) {
      WD_Targets[i] = WD_Targets[i] + T4_Amount
    } 
    if (T5_Start<=i&&T5_End>=i) {
      WD_Targets[i] = WD_Targets[i] + T5_Amount
    } 
  }
  WD_Targets = data.frame(Year = c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End)),WD = WD_Targets)
  
  POS_Winner = POS[which.max(POS_Wt),]
  POS_Winner[WD_Targets$WD==0] = 0
  
  WD_Targets$POS = POS_Winner
  
  POS_Winner = data.frame(Year =c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End)),POS = POS_Winner )
  
  p2 = ggplot() +
    geom_bar(data = WD_Targets, aes(x = Year, y = WD, fill = POS),stat = 'identity', position = "dodge") +
    scale_fill_gradient(low = "red",  high = "blue") +
    scale_x_continuous(breaks = c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End))) +
    scale_y_continuous(labels = dollar_format(prefix = "")) +
    theme_bw() +
    labs(title = "Withdrawal Targets",y = "Withdrawal Targets") +
    theme(legend.position = 'none')
  
  p3 = ggplot() +
    geom_bar(data = POS_Winner, aes(x = Year, y = POS*100),stat = 'identity', position = "dodge", fill = "green") +
    scale_x_continuous(breaks = c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End))) +
    scale_y_continuous(labels = dollar_format(prefix = "")) +
    theme_bw() +
    labs(title = "Probability of Success of Withdrawal Targets",y = "Probability of Success") +
    theme(legend.position = 'none')
  
  Frac_Target_Met = apply(Frac_Target_Met, 2, function(x) quantile(x,probs = 1-CL))
  Frac_Target_Met[WD_Targets$WD==0] = 0  
  
  Frac_Target_Met = data.frame(Year =c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                               Met = Frac_Target_Met*WD_Targets$WD,
                               Unmet = (1-Frac_Target_Met)*WD_Targets$WD)
  
  count = 0
  Target_Met = data.frame(Year =rep(c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End)),2),
                          Target = c(rep("Unmet",max(T1_End,T2_End,T3_End,T4_End,T5_End)),
                                     rep("Met",max(T1_End,T2_End,T3_End,T4_End,T5_End))),
                          Value = 0)
  for (i in 1:nrow(Frac_Target_Met)) {
    Target_Met$Value[i] = Frac_Target_Met$Unmet[i]
    Target_Met$Value[nrow(Frac_Target_Met)+i] = Frac_Target_Met$Met[i]
  }
  Target_Met$Target = factor(Target_Met$Target, levels = unique(Target_Met$Target))
  
  p4 = ggplot() +
    geom_bar(data = Target_Met, aes(x = Year, y = Value, fill = Target),stat = 'identity') +
    scale_x_continuous(breaks = c(1:max(T1_End,T2_End,T3_End,T4_End,T5_End))) +
    scale_y_continuous(labels = dollar_format(prefix = "Rs")) +
    theme_bw() +
    labs(title = paste("Targets Fulfilment at ",CL*100,"% Confodence",sep = ''),y = "") +
    theme(legend.position = "top")
  
  return(list(p1,p2,p3,p4))
  
  
}



# #Inputs
# 
# #Portfolio Today
# LC_Balance = 10000
# MC_Balance = 5000
# SC_Balance = 3000
# Bond_Balance = 4000
# RE_Balance = 100000
# FD_Balance = 50000
# Cash_Balance = 25000
# 
# Contribution = 20000
# Contri_Growth = .1
# Contri_Start = 1
# Contri_End = 12
# 
# Tax_Rate = 0.2
# 
# #Targets in today money and horizon
# T1_Amount = 200000
# T1_Start = 1
# T1_End = 1
# T1_Wt = 10
# 
# T2_Amount = 5000
# T2_Start = 5
# T2_End = 10
# T2_Wt = 5
# 
# T3_Amount = 60000
# T3_Start = 15
# T3_End = 15
# T3_Wt = 7
# 
# T4_Amount = 600000
# T4_Start = 7
# T4_End = 7
# T4_Wt = 8
# 
# T5_Amount = 5000
# T5_Start = 8
# T5_End = 8
# T5_Wt = 3
# 
# Annual_Expenses = 100000
# 
# CL = 0.5
# 
# Comf_.5 = "No"
# Comf_.25 = "No"
# Comf_.1 = "No"

#Comfortable with 50% market dip


# a = Sims(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
#          Contribution, Contri_Growth, Contri_Start, Contri_End,
#          T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
#          T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
#          T5_Amount, T5_Start,T5_End,Annual_Expenses, Weight_Mat[1,])
# x=a[[1]]

# Plots(LC_Balance,MC_Balance,SC_Balance,Bond_Balance,RE_Balance,FD_Balance,Cash_Balance,
#                  Contribution, Contri_Growth, Contri_Start, Contri_End,
#                  T1_Amount, T1_Start,T1_End,T2_Amount, T2_Start,T2_End,
#                  T3_Amount, T3_Start,T3_End,T4_Amount, T4_Start,T4_End,
#                  T5_Amount, T5_Start,T5_End,Annual_Expenses, CL, Comf_.5, Comf_.25, Comf_.1)


# Plots(50000,0,0,0,0,0,0,10000,0,1,5,
#       150000,5,5,10,
#       0,1,1,0,
#       0,1,1,0,
#       0,1,1,0,
#       0,1,1,0,
#       0,0.9,"Yes","No","No")
      
ui = fluidPage(
  
  add_busy_spinner(spin = "cube-grid"),
  
  titlePanel("Target Projections"),
  
  h3("Risk Apetite"),
  br(),
  fluidRow(column(width = 4,selectInput("Comf_5","Are you comfortable if market falls 50%", choices = c("No","Yes"))),
           column(width = 4,conditionalPanel(condition = "input.Comf_5=='Yes'",
                            selectInput("Comf_25","Are you comfortable if market falls 25%", choices = c("No","Yes")))),
                  column(width = 4,conditionalPanel(condition = "input.Comf_25=='Yes'",
                            selectInput("Comf_1","Are you comfortable if market falls 10%", choices = c("No","Yes"))))),
  br(),
  h3("Current Portfolio"),
  br(),
  fluidRow(column(width = 3, numericInput("LC_Balance","Large Cap Balance",0)),
           column(width = 3, numericInput("MC_Balance","Mid Cap Balance",0)),
           column(width = 3, numericInput("SC_Balance","Small Cap Balance",0)),
           column(width = 3, numericInput("Bond_Balance","Bond Balance",0))),
  fluidRow(column(width = 3, numericInput("RE_Balance","Real Estate Balance",0)),
           column(width = 3, numericInput("FD_Balance","FD Balance",0)),
           column(width = 3, numericInput("Cash_Balance","Cash Balance",0))),
  
  br(),
  h3("Annual Contribution towards Portfolio"),
  br(),
  fluidRow(column(width = 3, numericInput("Contribution","Annual Contribution",0)),
           column(width = 3, numericInput("Contri_Growth","Annual Contribution Growth in %",0)),
           column(width = 3, numericInput("Contri_Start","Annual Contribution Start Year",1)),
           column(width = 3, numericInput("Contri_End","Annual Contribution End Year",10))),
  
  
  br(),
  h3("Target 1"),
  br(),
  fluidRow(column(width = 3, numericInput("T1_Amount","Target Amount in today rupees",0)),
           column(width = 3, numericInput("T1_Start","Target Withdrawal Start Year",5)),
           column(width = 3, numericInput("T1_End","Target Withdrawal End Year",5)),
           column(width = 3, numericInput("T1_Wt","How important is this target from 1 to 10?",10))),
  
  br(),
  selectInput("T2_Yes","Do you have another target?", choices = c("No","Yes")),
  br(),
  conditionalPanel(condition = "input.T2_Yes=='Yes'",h3("Target 2")),
  conditionalPanel(condition = "input.T2_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T2_Yes=='Yes'",
               fluidRow(column(width = 3, numericInput("T2_Amount","Target Amount in today rupees",0)),
               column(width = 3, numericInput("T2_Start","Target Withdrawal Start Year",5)),
               column(width = 3, numericInput("T2_End","Target Withdrawal End Year",5)),
               column(width = 3, numericInput("T2_Wt","How important is this target from 1 to 10?",10)))),
  
  conditionalPanel(condition = "input.T2_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T2_Yes=='Yes'",
                   selectInput("T3_Yes","Do you have another target?", choices = c("No","Yes"))),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",h3("Target 3")),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",
                    fluidRow(column(width = 3, numericInput("T3_Amount","Target Amount in today rupees",0)),
                             column(width = 3, numericInput("T3_Start","Target Withdrawal Start Year",5)),
                             column(width = 3, numericInput("T3_End","Target Withdrawal End Year",5)),
                             column(width = 3, numericInput("T3_Wt","How important is this target from 1 to 10?",10)))),
  
  
  conditionalPanel(condition = "input.T2_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",
                   selectInput("T4_Yes","Do you have another target?", choices = c("No","Yes"))),
  conditionalPanel(condition = "input.T3_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T4_Yes=='Yes'",h3("Target 4")),
  conditionalPanel(condition = "input.T2_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T4_Yes=='Yes'",
                  fluidRow(column(width = 3, numericInput("T4_Amount","Target Amount in today rupees",0)),
                           column(width = 3, numericInput("T4_Start","Target Withdrawal Start Year",5)),
                           column(width = 3, numericInput("T4_End","Target Withdrawal End Year",5)),
                           column(width = 3, numericInput("T4_Wt","How important is this target from 1 to 10?",10)))),
  
  
  conditionalPanel(condition = "input.T3_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T4_Yes=='Yes'",
                   selectInput("T5_Yes","Do you have another target?", choices = c("No","Yes"))),
  conditionalPanel(condition = "input.T5_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T5_Yes=='Yes'",h3("Target 5")),
  conditionalPanel(condition = "input.T5_Yes=='Yes'",br()),
  conditionalPanel(condition = "input.T5_Yes=='Yes'",
                  fluidRow(column(width = 3, numericInput("T5_Amount","Target Amount in today rupees",0)),
                           column(width = 3, numericInput("T5_Start","Target Withdrawal Start Year",5)),
                           column(width = 3, numericInput("T5_End","Target Withdrawal End Year",5)),
                           column(width = 3, numericInput("T5_Wt","How important is this target from 1 to 10?",10)))),
  
  fluidRow(column(width = 3, numericInput("Annual_Expenses","If you want to have half your annual expenses as emergency fund in cash enter. Otherwise not.",0)),
           column(width = 3, numericInput("CL","Confidence Level of Meeting Targets",90))),
  
  br(),
  actionButton("button", "Run Projections"),
  
  br(),
  plotOutput("Plot1"),
  plotOutput("Plot2"),
  plotOutput("Plot3"),
  plotOutput("Plot4")

)

server = function(input,output,session) {
  
 Out_put <- eventReactive(input$button, {
    Plots(input$LC_Balance,input$MC_Balance,input$SC_Balance,input$Bond_Balance,
          input$RE_Balance,input$FD_Balance,input$Cash_Balance,
          input$Contribution, input$Contri_Growth/100, input$Contri_Start, input$Contri_End,
          input$T1_Amount, input$T1_Start,input$T1_End,input$T1_Wt,
          
          ifelse(input$T2_Yes=="Yes",input$T2_Amount,0),
          ifelse(input$T2_Yes=="Yes",input$T2_Start,1),
          ifelse(input$T2_Yes=="Yes",input$T2_End,1),
          ifelse(input$T2_Yes=="Yes",input$T2_Wt,0),
          
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes",input$T3_Amount,0),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes",input$T3_Start,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes",input$T3_End,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes",input$T3_Wt,0),
          
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes",input$T4_Amount,0),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes",input$T4_Start,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes",input$T4_End,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes",input$T4_Wt,0),
          
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes"&&input$T5_Yes=="Yes",input$T5_Amount,0),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes"&&input$T5_Yes=="Yes",input$T5_Start,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes"&&input$T5_Yes=="Yes",input$T5_End,1),
          ifelse(input$T2_Yes=="Yes"&&input$T3_Yes=="Yes"&&input$T4_Yes=="Yes"&&input$T5_Yes=="Yes",input$T5_Wt,0),
          
          input$Annual_Expenses, input$CL/100, 
          input$Comf_5, 
          ifelse(input$Comf_5=="Yes",input$Comf_25,"No"), 
          ifelse(input$Comf_25=="Yes",input$Comf_1,"No"))
  })
 
 output$Plot1 = renderPlot({
   Out_put()[[1]]
 })
 
 output$Plot2 = renderPlot({
   Out_put()[[2]]
 })
 output$Plot3 = renderPlot({
   Out_put()[[3]]
 })
 output$Plot4 = renderPlot({
   Out_put()[[4]]
 })
  
  
}

shinyApp(ui = ui, server = server)







