
sapply(list.files("../R", full = TRUE), source)
library("e1071")
library("formatR")

m <- structural(ind60 ~ x1 + x2 + x3) +
     structural(dem60 ~ y1 + y2 + y3 + y4 | aaa,
                param = c(dem60 = "manuel")) +
     structural(dem65 ~ y5 + y6 + y7 + I(y8 * 2),
                param = c("I(y8 * 2)" = "ppp")) +
     structural(dem65 ~ z1*z2) +
     measurement(dem60 ~ ind60) +
     intercept(item1 ~ 1) +
     covariance(item1 ~ item2) #+ group(bbb)
m


dat <- data.frame(aaa = gl(2, 5), bbb = gl(5, 2), c = 3)
dat$y1 <- dat$y2 <- dat$y3 <- dat$y4 <- dat$y5 <- runif(10)

m <- m + dataset(dat)
m


m <- m + constraint(dem65_z1 == 1)                        # Fixed parameter
m

m <- m + constraint(ind60_x2 == ind60_x1) +               # Equality
         constraint(ind60_x3 == ind60_x1)
m

m <- m + constraint(dem60_y1 > (dem60_y2 + dem60_y3)^2)   # Inequality (currently inactive)
m

summary(m)

