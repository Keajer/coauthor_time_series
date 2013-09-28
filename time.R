#time series analysis

#this function fits a best ts model according to AIC
get.best.arima <- function(x.ts, maxord = c(3,3,3,3,3,3)){
        best.aic <- 1e8
        n <- length(x.ts)
        for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
        for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6]){
                fit <- arima(x.ts, order = c(p,d,q),
                seas = list(order = c(P,D,Q),
                frequency(x.ts)), method = "CSS")
                fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
                if (fit.aic < best.aic){
                        best.aic <- fit.aic
                        best.fit <- fit
                        best.model <- c(p,d,q,P,D,Q)
                }
        }
list(best.aic, best.fit, best.model)
}

#test on area1:
acf(p1.a) #white noise or posssibly MA(2) spike at 2
par(mfrow = c(1, 2))
acf(p1.b);pacf(p1.b)#AR(1) because acf decays smoothly like a expoential 
par(mfrow = c(1, 2))
acf(p1.c);pacf(p1.c) # AR(1) likely..
par(mfrow = c(1, 2))
acf(p1.d);pacf(p1.d)#ARMA(1, 1)
acf(p1.e)#white noise...

get.best.arima(p1.d) #trying to test seasonal model first, got the parameters: 0 0 1 2 0 1
m<- arima(p1.d, order = c(0, 0, 1), seas = list(order = c(2, 0, 1), frequency(p1.d)), method = "CSS")
tsdiag(m)#the p-values show partially significant

z <- spec.pgram(p1.d, taper = 0)
abline(v = 1/6, lty = "dashed")
abline(v = 1/12, lty = "dotted")
z.order <- order(z$spec,decreasing=T)
z.max <- z$freq[z.order[1]]
period <- 1/z.max #this tells you the period covers the entire period..
#try model w/o seasonal effect
m1 <- arima(p1.d, order = c(1, 0, 1),  method = 'CSS')
tsdiag(m1) #shows good fit without seasonal effect.

#area2:
x11()
par(mfrow = c(1, 2))
acf(p2.a);pacf(p2.a)
par(mfrow = c(1, 3))
acf(p2.b)
acf(p2.c)
acf(p2.d)
par(mfrow = c(1, 2))
acf(p2.e);pacf(p2.e)
#all shows white noises..

#area3
par(mfrow = c(1, 2))
acf(p3.a);pacf(p3.a)#AR
acf(p3.b)#white noise
par(mfrow = c(1, 2))
acf(p3.c);pacf(p3.c)#MA
get.best.arima(p3.c)
m<- arima(p1.c, order = c(2, 0, 3), seas = list(order = c(3, 1, 2), frequency(p1.d)), method = "CSS")
tsdiag(m)#the p-values show partially significant
z <- spec.pgram(p3.c, taper = 0)
abline(v = 1/6, lty = "dashed")
abline(v = 1/12, lty = "dotted")
z.order <- order(z$spec,decreasing=T)
z.max <- z$freq[z.order[1]]
period <- 1/z.max #this tells you the period covers the entire period..
pacf(p3.c, lag = 2)
m1 <- arima(p1.d, order = c(0, 1, 1),  method = 'CSS')
tsdiag(m1) #shows good fit without seasonal effect.

par(mfrow = c(1, 2))
acf(p3.d);pacf(p3.d)#AR(1)
par(mfrow = c(1, 2))
acf(p3.e);pacf(p3.e)#white noise

#area 5
par(mfrow = c(1, 2))
acf(p5.a);pacf(p5.a)
par(mfrow = c(1, 2))
acf(p5.b)
acf(p5.c)
par(mfrow = c(1, 2))
acf(p5.d);pacf(p5.d)
acf(p5.e)

#area 7
par(mfrow = c(1, 2))
acf(p7.a);pacf(p7.a)
par(mfrow = c(1, 2))
acf(p7.b)
acf(p7.c)
par(mfrow = c(1, 2))
acf(p7.d);pacf(p7.d)
par(mfrow = c(1, 2))
acf(p7.e);pacf(p7.e)



par(mfrow  = c(6, 2))
pacf(a1accu)
pacf(a2accu)
pacf(a3accu)
pacf(a4accu)
pacf(a5accu)
pacf(a6accu)
pacf(a7accu)
pacf(a8accu)
pacf(a9accu)
pacf(a10accu)
pacf(a11accu)


