# Case Studies - Report 1 (Felix Fikowski, 280189)

################################################################################
#Laden benötigter Pakete
{
  library(ggplot2)
  library(xtable)
  library(ggpubr)
  library(vars)
}
################################################################################

#Setzen des Dateipfades

#Import of the data
{
  setwd("C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies")
    
  df <- read.csv("2022-02.csv")
  df <- as.data.frame(df)
  head(df)
  dim(df)
  #[1] 256 247
  
  #remove uninformative rows:
  df <- df[-c(1:2, 255:256), ] 
  
  colnames(df)
  df <- df[, c("sasdate","GDPC1","CUMFNS","UNRATESTx","CPIAUCSL",
                    "FEDFUNDS","M1REAL","S.P.500")]
  
  head(df)
  
  dim(df)
  #[1] 256   7
}

#xtable(con_table)

################################################################################
#plot of the different time series

#plot GDPC1
GDPC1_plot<-plot(df$GDPC1, xlab = "t", ylab = expression(GDPC1[t]), type = "o",
     main = "GDPC1 Time Series")

#plot CUMFNS
CUMFNS_plot<-plot(df$CUMFNS, xlab = "t", ylab = expression(CUMFNS[t]), type = "o",
                 main = "CUMFNS Time Series")

#plot UNRATESTx
UNRATESTx_plot<-plot(df$UNRATESTx, xlab = "t", ylab = expression(UNRATESTx[t]), type = "o",
                 main = "UNRATESTx Time Series")

#plot CPIAUCSL
CPIAUCSL_plot<-plot(df$CPIAUCSL, xlab = "t", ylab = expression(CPIAUCSL[t]), type = "o",
                 main = "CPIAUCSL Time Series")

#plot FEDFUNDS
FEDFUNDS_plot<-plot(df$FEDFUNDS, xlab = "t", ylab = expression(FEDFUNDS[t]), type = "o",
                 main = "FEDFUNDS Time Series")

#plot M1REAL
M1REAL_plot<-plot(df$M1REAL, xlab = "t", ylab = expression(M1REAL[t]), type = "o",
                 main = "M1REAL Time Series")

#plot S.P.500
S.P.500_plot<-plot(df$S.P.500, xlab = "t", ylab = expression(S.P.500[t]), type = "o",
                 main = "S.P.500 Time Series")
################################################################################
#Transform the data as mentioned in the project description
{
  matrix <- data.matrix(df, rownames.force = NA)
  
  #transform GDP (GDP1)
  GDP <- matrix[,2]
  GDP_per <- diff(GDP,lag=1, differences = 1)/GDP[1:251]*100
  
  #transform manufacturing industry capacity utilization (CUMFNS)
  CUMFNS <- matrix[2:252,3]
  
  #transform unemploymentrate (UNRATESTx )
  unemp <- matrix[2:252,4]
  head(unemp)
  
  #transform consumerprice index (CPIAUCSL  )
  CPIAUCSL <- matrix[,5]
  CPIAUCSL_per <- diff(CPIAUCSL,lag=1, differences = 1)/CPIAUCSL[1:251]*100
  head(CPIAUCSL_per)
  
  #transform Federal Funds rate (FEDFUNDS)
  FEDFUNDS <- matrix[2:252,6]
  head(FEDFUNDS)
  
  #transform M1 money stock (M1REAL)
  M1REAL <- matrix[,7]
  M1REAL_per <- diff(M1REAL,lag=1, differences = 1)/M1REAL[1:251]*100
  head(M1REAL_per)
  
  #transform S.P.500 (S.P.500)
  S.P.500 <- matrix[,8]
  S.P.500_per <- diff(S.P.500,lag=1, differences = 1)/S.P.500[1:251]*100
  head(S.P.500_per)
  

  dates <- as.Date(df[4:254,1], "%m/%d/%Y")
  
  per_matrix <- cbind(GDP_per, CUMFNS, unemp, CPIAUCSL_per, FEDFUNDS, M1REAL_per, S.P.500_per)
  head(per_matrix)
  
  dates <- as.Date(df[2:252,1], "%m/%d/%Y")
  
  data <- data.frame(dates,per_matrix[,1], per_matrix[,2], per_matrix[,3],
                     per_matrix[,4], per_matrix[,5], per_matrix[,6], per_matrix[,7])
  colnames(data) <- c("dates", "GDP_per", "CUMFNS", "unemp", 
                      "CPIAUCSL_per", "FEDFUNDS", "M1REAL_per", "S.P.500_per")
  #5-Number summary of the data:
  
  GDPC1<-summary(data$GDP_per)
  CUMFNS<-summary(data$CUMFNS)
  UNRATESTx<-summary(data$unemp)
  CPIAUCSL<-summary(data$CPIAUCSL_per)
  FEDFUNDS<-summary(data$FEDFUNDS)
  M1REAL<-summary(data$M1REAL_per)
  S.P.500<-summary(data$S.P.500_per)
  
  
  compare1<-cbind(GDPC1,CUMFNS, UNRATESTx, CPIAUCSL, FEDFUNDS, M1REAL, S.P.500)
  con_table <- round(compare1,2)
  con_table
}

#         GDPC1 CUMFNS UNRATESTx CPIAUCSL FEDFUNDS M1REAL S.P.500
#Min.    -8.94  63.76      2.81    -2.29     0.06  -3.82  -27.33
#1st Qu.  0.34  75.71      3.92     0.50     1.94  -0.54   -0.86
#Median   0.75  78.94      4.75     0.79     4.74   0.47    2.00
#Mean     0.74  79.28      4.86     0.91     4.83   1.49    1.96
#3rd Qu.  1.16  82.84      5.47     1.14     6.55   1.28    5.61
#Max.     7.55  91.57     12.25     3.95    17.78 207.56   20.12

xtable(con_table)

################################################################################
#TAsk a)
#plot generation of the 7 time series
{
  series_GDP_per <- ggplot(data = data, aes(x=dates, y=GDP_per)) +
    geom_line() + 
    ylab("GDP gr. [%]") +
    xlab("")
  
  series_CUMFNS <-ggplot(data, aes(x=dates, y=CUMFNS)) +
    geom_line() + 
    ylab("CUMFNS [%]") +
    xlab("")
  
  series_unemp <-ggplot(data, aes(x=dates, y=unemp)) +
    geom_line() + 
    ylab("UNRATESTx [%]") +
    xlab("")
  
  series_CPIAUCSL_per <-ggplot(data, aes(x=dates, y=CPIAUCSL_per)) +
    geom_line() + 
    ylab("Inflation [%]") +
    xlab("")
  
  series_FEDFUNDS <-ggplot(data, aes(x=dates, y=FEDFUNDS)) +
    geom_line() + 
    ylab("FEDFUNDS [%]") +
    xlab("")
  
  series_M1REAL_per <-ggplot(data, aes(x=dates, y=M1REAL_per)) +
    geom_line() + 
    ylab("M1REAL gr. [%]") +
    xlab("year")
  
  series_S.P.500_per <-ggplot(data, aes(x=dates, y=S.P.500_per)) +
    geom_line() +
    ylab("S.P.500 gr. [%]") +
    xlab("year")
  
  figure <- ggarrange(series_GDP_per, series_CUMFNS, series_unemp, series_CPIAUCSL_per, 
                      series_FEDFUNDS, series_M1REAL_per, series_S.P.500_per,
                      ncol = 2, nrow = 4)
  figure <- figure +theme(plot.margin = margin(0.5,0,0,0, "cm"))
  
  ggsave("series.pdf",plot =figure)
  figure
}

################################################################################
#task b)

#Forecast using an AR(1) model
forecast_ar <- c()
for(i in 3:(length(data$GDP_per)-1)) {
  GDP_ar<-ar.ols(data$GDP_per[1:i], aic = FALSE,order.max = 1)
  GDP_ar_forecast <- predict(GDP_ar, n.ahead = 1)
  new_value <- GDP_ar_forecast$pred           
  forecast_ar <- c(forecast_ar, new_value)    
}

data_forecast <- data.frame(dates[4:251],GDP_per[4:251], forecast_ar)
colnames(data_forecast) <- c("dates", "GDP_per", "forecast_ar")


ar_plot <- ggplot(data_forecast, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth")) + 
  geom_line(aes(y = forecast_ar, colour = "AR(1) forecast"), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")
ar_plot

ggsave("ar_plot1.pdf",plot = ar_plot)
#calculate the RMSE

RMSE_AR <- sqrt(mean((data_forecast$GDP_per - data_forecast$forecast_ar)^2))
RMSE_AR

################################################################################
#task c)
#Forecast using an VAR(1) model

var_forecast <- c()
for(i in 9:250) {
  var_model <- VAR(data[c(1:i),c(2,3,4,5,6,7,8)], p = 1, type = "cons", season = NULL, exog = NULL)
  var_forecast_total <- predict(var_model, n.ahead = 1)
  new_value <- var_forecast_total$fcst$GDP_per[1]           
  var_forecast <- c(var_forecast, new_value)    
}
length(var_forecast)
head(data)
data_var_forecast <- data.frame(data_forecast[7:248,1:3],var_forecast)
colnames(data_var_forecast) <- c("dates", "GDP_per", "ar_forecast", "var_forecast")

var1_plot <- ggplot(data_var_forecast, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth")) + 
  geom_line(aes(y = ar_forecast, colour = "AR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = var_forecast, colour = "VAR(1) forecast"), linetype = "dashed") +
  xlab("Time") + ylab("GDP Growth in %") +
  theme(legend.position="bottom")

ggsave("var1_plot.pdf",plot =var1_plot)

#calculate the RMSE

RMSE_VAR <- sqrt(mean((data_var_forecast$GDP_per - data_var_forecast$var_forecast)^2))
RMSE_VAR
################################################################################
#d)

#Calculate the t statistics for the Granger causality test

for (i in 1:(251-8)){
  
  ones_felix = rep(1,(7+i))
  yt1_felix = data$GDP_per[1:(7+i)]
  yt2_felix = data$CUMFNS[1:(7+i)]
  yt3_felix = data$unemp[1:(7+i)]
  yt4_felix = data$CPIAUCSL_per[1:(7+i)]
  yt5_felix = data$FEDFUNDS[1:(7+i)]
  yt6_felix = data$M1REAL_per[1:(7+i)]
  yt7_felix = data$S.P.500_per[1:(7+i)]
  
  z_felix = rbind(ones_felix,yt1_felix,yt2_felix,yt3_felix,yt4_felix,yt5_felix,yt6_felix,yt7_felix)
  Y = rbind(data$GDP_per[2:(8+i)],
            data$CUMFNS[3:(9+i)],
            data$unemp[3:(9+i)],
            data$CPIAUCSL_per[2:(8+i)],
            data$FEDFUNDS[3:(9+i)],
            data$M1REAL_per[2:(8+i)],
            data$S.P.500_per[2:(8+i)])
}

C = solve(z_felix%*%t(z_felix))
B = Y%*%t(z_felix)
A = B%*%C

Resd_1 = Y- (A%*%z_felix)

head(z_felix)
var_P1_1 = solve(z_felix%*%t(z_felix))
var_P1_2 = ((Resd_1%*%t(Resd_1))/(250-(7*1)-1))
var_P1 = kronecker(var_P1_1,var_P1_2)

dim(var_P1_1)

# T-test statistic for GDP

t_test_GDP= A[1,2]/sqrt(var_P1[8,8])
t_test_GDP

#t_test_GDP  1.448

# For a large sample size it follows a normal distribution
# at the level of significance alpha = 0.05 we have that for a 
# 2 sided test the critical value is (+/-) 1.96
# because 0.05203193 < 1.96 we do not reject the null hypothesis, which indicates
# that the parameter estimation for lag 1 GDP could be 0.

t_test_CUMFNS= A[1,3]/sqrt(var_P1[15,15])
t_test_CUMFNS

# t_test_CUMFNS = 4.456844

t_test_UNRATESTx= A[1,4]/sqrt(var_P1[22,22])
t_test_UNRATESTx

# t_test_UNRATESTx = 4.03169

t_test_CPIAUCSL= A[1,5]/sqrt(var_P1[29,29])
t_test_CPIAUCSL

# t_test_CPIAUCSL = -1.607346 

t_test_FEDFUNDS= A[1,6]/sqrt(var_P1[36,36])
t_test_FEDFUNDS

# t_test_FEDFUNDS =  -2.013595

t_test_M1REAL= A[1,7]/sqrt(var_P1[43,43])
t_test_M1REAL

# t_test_M1REAL = 4.618165 

t_test_S.P.500= A[1,8]/sqrt(var_P1[50,50])
t_test_S.P.500

# t_test_S.P.500 = 3.453106

test_res <- cbind(t_test_GDP, t_test_CUMFNS, t_test_UNRATESTx, t_test_CPIAUCSL, t_test_FEDFUNDS, t_test_M1REAL, t_test_S.P.500)
tests <- data.frame(test_res)
colnames(tests) <- c("GDPC1", "CUMFNS", "UNRATESTx", "CPIAUCSL", "FEDFUNDS", "M1REAL", "S&P 500")

xtable(tests)

################################################################################
#e)

#determine the AIC criteria up to lag-order 4

var_model_lag_sel <- VARselect(data[c(1:251),c(2,3,4,5,6,7,8)], lag.max = 4, type = "const",season = NULL, exogen = NULL)
var_model_lag_sel$criteria
xtable(var_model_lag_sel$criteria)
varp_model <- VAR(data[c(1:33),c(2,3,4,5,6,7,8)], p = 4, type = "cons", season = NULL, exog = NULL)
varp_model

#VAR(p) process for the p that minimizes the AIC criteria
varp_forecast <- c()
for(i in 25:250) {
  varp_model <- VAR(data[c(1:i),c(2,3,4,5,6,7,8)], p = which.min(var_model_lag_sel$criteria[1,]), type = "cons", season = NULL, exog = NULL)
  varp_forecast_total <- predict(varp_model, n.ahead = 1)
  new_value <- varp_forecast_total$fcst$GDP_per[1]           
  varp_forecast <- c(varp_forecast, new_value)    
}


data_varp_forecast <- data.frame(data_var_forecast[17:242,1:4],varp_forecast)
colnames(data_varp_forecast) <- c("dates", "GDP_per", "forecast", "var_forecast", "varp_forecast")


varp_plot <- ggplot(data_varp_forecast, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth")) + 
  geom_line(aes(y = varp_forecast, colour = "VAR(3) forecasted GDP growth")) +
  ggtitle("Forecast of GDP Growth") + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")
varp_plot

varp_plot_total <- ggplot(data_varp_forecast, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth")) + 
  geom_line(aes(y = forecast, colour = "AR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = var_forecast, colour = "VAR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = varp_forecast, colour = "VAR(3) forecast"), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")
varp_plot_total

ggsave("var3_plot.pdf",plot =varp_plot_total)


RMSE_VARp <- sqrt(mean((data_varp_forecast$GDP_per - data_varp_forecast$varp_forecast)^2))
RMSE_VARp

################################################################################
#f)

#calculate the RSME for different time frames and models
dim(data_varp_forecast)
data_varp_forecast[219:226,]
#60s

RMSE_AR1_60 <- sqrt(mean((data_varp_forecast$GDP_per[1:18] - data_varp_forecast$forecast[1:18])^2))
RMSE_AR1_60

RMSE_VAR1_60 <- sqrt(mean((data_varp_forecast$GDP_per[1:18] - data_varp_forecast$var_forecast[1:18])^2))
RMSE_VAR1_60

RMSE_VAR3_60 <- sqrt(mean((data_varp_forecast$GDP_per[1:18] - data_varp_forecast$varp_forecast[1:18])^2))
RMSE_VAR3_60

RMSE_60 <- rbind(RMSE_AR1_60,RMSE_VAR1_60,RMSE_VAR3_60)

#70s

RMSE_AR1_70 <- sqrt(mean((data_varp_forecast$GDP_per[19:58] - data_varp_forecast$forecast[19:58])^2))
RMSE_AR1_70

RMSE_VAR1_70 <- sqrt(mean((data_varp_forecast$GDP_per[19:58] - data_varp_forecast$var_forecast[19:58])^2))
RMSE_VAR1_70

RMSE_VAR3_70 <- sqrt(mean((data_varp_forecast$GDP_per[19:58] - data_varp_forecast$varp_forecast[19:58])^2))
RMSE_VAR3_70

RMSE_70 <- rbind(RMSE_AR1_70,RMSE_VAR1_70,RMSE_VAR3_70)


#80s

RMSE_AR1_80 <- sqrt(mean((data_varp_forecast$GDP_per[59:98] - data_varp_forecast$forecast[59:98])^2))
RMSE_AR1_80

RMSE_VAR1_80 <- sqrt(mean((data_varp_forecast$GDP_per[59:98] - data_varp_forecast$var_forecast[59:98])^2))
RMSE_VAR1_80

RMSE_VAR3_80 <- sqrt(mean((data_varp_forecast$GDP_per[59:98] - data_varp_forecast$varp_forecast[59:98])^2))
RMSE_VAR3_80

RMSE_80 <- rbind(RMSE_AR1_80,RMSE_VAR1_80,RMSE_VAR3_80)

#90s

RMSE_AR1_90 <- sqrt(mean((data_varp_forecast$GDP_per[99:138] - data_varp_forecast$forecast[99:138])^2))
RMSE_AR1_90

RMSE_VAR1_90 <- sqrt(mean((data_varp_forecast$GDP_per[99:138] - data_varp_forecast$var_forecast[99:138])^2))
RMSE_VAR1_90

RMSE_VAR3_90 <- sqrt(mean((data_varp_forecast$GDP_per[99:138] - data_varp_forecast$varp_forecast[99:138])^2))
RMSE_VAR3_90

RMSE_90 <- rbind(RMSE_AR1_90,RMSE_VAR1_90,RMSE_VAR3_90)

#00s

RMSE_AR1_00 <- sqrt(mean((data_varp_forecast$GDP_per[139:178] - data_varp_forecast$forecast[139:178])^2))
RMSE_AR1_00

RMSE_VAR1_00 <- sqrt(mean((data_varp_forecast$GDP_per[139:178] - data_varp_forecast$var_forecast[139:178])^2))
RMSE_VAR1_00

RMSE_VAR3_00 <- sqrt(mean((data_varp_forecast$GDP_per[139:178] - data_varp_forecast$varp_forecast[139:178])^2))
RMSE_VAR3_00

RMSE_00 <- rbind(RMSE_AR1_00,RMSE_VAR1_00,RMSE_VAR3_00)

#10s

RMSE_AR1_10 <- sqrt(mean((data_varp_forecast$GDP_per[179:218] - data_varp_forecast$forecast[179:218])^2))
RMSE_AR1_10

RMSE_VAR1_10 <- sqrt(mean((data_varp_forecast$GDP_per[179:218] - data_varp_forecast$var_forecast[179:218])^2))
RMSE_VAR1_10

RMSE_VAR3_10 <- sqrt(mean((data_varp_forecast$GDP_per[179:218] - data_varp_forecast$varp_forecast[179:218])^2))
RMSE_VAR3_10
RMSE_10 <- rbind(RMSE_AR1_10,RMSE_VAR1_10,RMSE_VAR3_10)

#20s

RMSE_AR1_20 <- sqrt(mean((data_varp_forecast$GDP_per[219:226] - data_varp_forecast$forecast[219:226])^2))
RMSE_AR1_20

RMSE_VAR1_20 <- sqrt(mean((data_varp_forecast$GDP_per[219:226] - data_varp_forecast$var_forecast[219:226])^2))
RMSE_VAR1_20

RMSE_VAR3_20 <- sqrt(mean((data_varp_forecast$GDP_per[219:226] - data_varp_forecast$varp_forecast[219:226])^2))
RMSE_VAR3_20

RMSE_20 <- rbind(RMSE_AR1_20,RMSE_VAR1_20,RMSE_VAR3_20)

#total
RMSE_AR1_total <- sqrt(mean((data_varp_forecast$GDP_per[1:226] - data_varp_forecast$forecast[1:226])^2))
RMSE_AR1_total

RMSE_VAR1_total <- sqrt(mean((data_varp_forecast$GDP_per[1:226] - data_varp_forecast$var_forecast[1:226])^2))
RMSE_VAR1_total

RMSE_VAR3_total <- sqrt(mean((data_varp_forecast$GDP_per[1:226] - data_varp_forecast$varp_forecast[1:226])^2))
RMSE_VAR3_total

RMSE_total <- rbind(RMSE_AR1_total,RMSE_VAR1_total,RMSE_VAR3_total)
RSME <- cbind(RMSE_60,RMSE_70,RMSE_80,RMSE_90,RMSE_00,RMSE_10,RMSE_20,RMSE_total)
RSME

xtable(RSME,digits=c(0,4,4,4,4,4,4,4,4))

