rm(list = ls())

#2.14

ratio <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3)
viscosity <- c(0.45, 0.2, 0.34, 0.58, 0.7, 0.57, 0.55, 0.44)


plot(ratio, viscosity, main="Ratio-Viscosity Scatterplot", 
     xlab="Ratio ", ylab="Viscosity ", pch=19)

linear_model = lm(viscosity ~ ratio)
summary(linear_model)

newdata = data.frame(ratio)

#confidence interval
predict(linear_model, interval="confidence",
        newdata)

#prediction interval
predict(linear_model, interval="prediction",
        newdata)

plot(x=ratio, y=viscosity,
     xlab="Ratio", ylab="Viscosity",
     ylim=c(-0.2, +1.2),
     panel.last = c(lines(sort(ratio), fitted(linear_model)[order(ratio)]),
                    lines(sort(ratio), 
                          predict(linear_model, 
                                  interval="confidence")[order(ratio), 2], col="blue"),
                    lines(sort(ratio), 
                          predict(linear_model, 
                                  interval="confidence")[order(ratio), 3], col="blue"),
                    lines(sort(ratio), 
                          predict(linear_model, 
                                  interval="prediction")[order(ratio), 2], col="red"),
                    lines(sort(ratio), 
                          predict(linear_model, 
                                  interval="prediction")[order(ratio), 3], col="red")))

#2.15

volume <- c(2084, 2084, 2273, 2273, 2273,
            2463, 2463, 2651, 2652, 2652,
            2842, 2842, 3030, 3031, 3031,
            3221, 3221, 3409, 3410, 3600,
            3600, 3788, 3789, 3789, 3979,
            3979, 4167, 4168, 4168, 4358, 
            4358, 4546, 4547)

pressure <- c(4599, 4600, 5044, 5043, 5044,
              5488, 5487, 5931, 5932, 5932,
              6380, 6380, 6818, 6817, 6818,
              7266, 7268, 7709, 7710, 8156,
              8158, 8597, 8599, 8600, 9048,
              9048, 9484, 9487, 9487, 9936,
              9939, 10377, 10379)

model <- lm(volume ~ pressure)

summary(model)

#2.30

temperature <- c(21, 24, 32, 47, 50, 59,
                 68, 74, 62, 50, 41, 30)

usage <- c(185.79, 214.47, 288.03, 424.84, 454.68, 539.03,
           621.55, 675.06, 562.03, 452.93, 369.95, 273.98)

model_temperature <- lm(usage ~ temperature)
summary(model_temperature)

sxy <- sum(temperature * usage) - (sum(temperature) * sum(usage)) / length(usage)
sxx <- sum(temperature^2) - sum(temperature)^2 / length(usage)
sxx
syy <- sum(usage^2) - sum(usage)^2 / length(usage)
syy


r_2 = ((9.20847)^2*(sxx))/(syy)
r = (r_2)^(0.5)
r

#t0 calculate

t0 = (r*(10^(0.5)))/((1-r)^(0.5))
t0



