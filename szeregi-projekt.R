library(TSstudio)
library(forecast)
library(stats)
library(astsa)
library(dtplyr)



df = read.table('C:/Users/klaud/Documents/Szeregi czasowe/rowery.csv',sep=';') 
time_series = ts(as.double(df$V2), frequency=12) #sprawdzać nazwy kolumn
plot(time_series, main = 'Tak wygląda początkowy szereg czasowy')

# wygładzanie trendu metodą średniej ruchomej 
ts_filter = stats::filter(time_series, rep(1/5,5))

plot(time_series, main = 'Wygładzanie trendu metoda średniej ruchomej')
lines(ts_filter,col="red")


# model trendu liniowego 
ts_linear_trend = tslm(time_series ~ trend)
ts_fitted_vals = fitted.values(ts_linear_trend)
summary(ts_linear_trend)

plot(time_series)
lines(ts_filter, col='blue')
lines(ts_fitted_vals, col='red')

# model trendu liniowego - analiza residuow
res_linear_trend = residuals(ts_linear_trend)
plot(res_linear_trend)
qqnorm(res_linear_trend)
abline(a = 0, b =1, col = 'red')
shapiro.test(res_linear_trend)

# model trendu wielomianowego 
poly2 = tslm(time_series ~ trend + poly(trend,2))
poly4 = tslm(time_series ~ trend + poly(trend,4))
plot(time_series)
lines(fitted.values(poly2), col = 'blue')
lines(fitted.values(poly4),col = 'red')

# regresja nielokalna
ap = time_series

plot(ts_filter, col = 'red')
lines(lowess(ap, f=0.10), type='l')


# Sezonowość
ts_trend_seasonality = tslm(time_series ~ trend + season)
ts_trend_seasonality2 = fitted.values(ts_trend_seasonality)

plot(time_series)
lines(ts_filter, col='blue')
lines(fitted.values(ts_linear_trend), col='green')
lines(ts_trend_seasonality2, col = 'red') 
#brak sezonowości


plot(decompose(time_series))


par(mfrow=c(2,2))
plot(ts_filter, col = 'magenta', main="Średnia")
lines(time_series)

plot(time_series,main="Regresja")
lines(lowess(ap, f=0.20), type='l', col="darkorchid")

plot(time_series, main="Wielomianowy")
lines(fitted.values(poly2), col = 'salmon')
lines(fitted.values(poly4),col = 'red')

plot(time_series,main="Liniowy")
lines(ts_fitted_vals, col='red')




new_ts<-diff(time_series)
nt2<-time_series-ts_filter

plot(nt2, main="Wykres bez trendu")
lines(new_ts,col='magenta')

mean(new_ts,na.rm=TRUE)
mean(nt2, na.rm=TRUE)
var(new_ts,na.rm = TRUE)
var(nt2,na.rm=TRUE)