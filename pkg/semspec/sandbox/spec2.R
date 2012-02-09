
sapply(list.files("../R", full = TRUE), source)
library("e1071")
library("formatR")
library("qgraph")

m <- measurement(ind60 ~ x1 + x2 + x3) +
     measurement(dem60 ~ y1 + y2 + y3 + y4 | aaa,
                param = c(dem60 = "manuel")) +
     measurement(dem65 ~ y5 + y6 + y7 + I(y8 * 2),
                param = c("I(y8 * 2)" = "ppp")) +
     measurement(dem65 ~ z1*z2) +
     structural(dem60 ~ ind60) +
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




######################################################################

library("lavaan")

data("HolzingerSwineford1939")

m2 <- measurement(visual ~ x1 + x2 + x3) +
    measurement(textual ~ x4 + x5 + x6) +
    measurement(speed ~ x7 + x8 + x9)

m2 <- m2 + dataset(HolzingerSwineford1939)

m2
summary(m2)
plot(m2)

######################################################################

library("semPLS")

library("lavaan")
data("HolzingerSwineford1939")

## non-sense model
m3 <- measurement(visual ~ x1 + x2 + x3) +
    measurement(textual ~ x4 + x5 + x6) +
    measurement(speed ~ x7 + x8 + x9) +
    structural(speed ~ textual + visual) +
    structural(textual ~ visual)

m3 <- m3 + dataset(HolzingerSwineford1939)

m3
summary(m3)
plot(m3)   # does not work

semfit_semPLS(m3)

######################################################################

library("sem")

library("lavaan")
data("HolzingerSwineford1939")

## measurement model
m4 <- measurement(visual ~ x1 + x2 + x3) +
    measurement(textual ~ x4 + x5 + x6) +
    measurement(speed ~ x7 + x8 + x9)

m4 <- m4 + dataset(HolzingerSwineford1939)

m4
summary(m4)

## MV variances
m4 <- m4 + covariance(x1 ~ x1) +
  covariance(x2 ~ x2) +
  covariance(x3 ~ x3) +
  covariance(x4 ~ x4) +
  covariance(x5 ~ x5) +
  covariance(x6 ~ x6) +
  covariance(x7 ~ x7) +
  covariance(x8 ~ x8) +
  covariance(x9 ~ x9) 

## LV variances
m4 <- m4 + covariance(visual ~ visual) +
    covariance(textual ~ textual) +
    covariance(speed ~ speed)


## LVs covariance
m4 <- m4 + covariance(visual ~ textual) +
    covariance(visual ~ speed) +
    covariance(textual ~ speed)    


m4 <- m4 + constraint(visual_x1 == 1) +
    constraint(textual_x4 == 1) +
    constraint(speed_x7 == 1)

m4
summary(m4)
plot(m4)   # does not work

semfit_sem(m4)
semfit_sem(m4, start=start_values(m4, visual_speed=0.5))
semfit_sem(m4, start=start_values(m4, visual_speed=0.2))

sem4 <- semfit_sem(m4)

start <- sem4[!is.na(sem4$sem_fit), "sem_fit"] + rnorm(21, 0, 0.001)
names(start) <- sem4[!is.na(sem4$sem_fit),"param"]
start_values(m4, start)

sem42 <- semfit_sem(m4, start=start_values(m4, start))

cbind(sem42[, c("param", "sem_fit")], sem4[, "sem_fit"])
