
m <- latent(ind60 ~ x1 + x2 + x3) +
     latent(dem60 ~ y1 + y2 + y3 + y4 | aaa,
            param = c(dem60 = "manuel")) +
     latent(dem65 ~ y5 + y6 + y7 + I(y8 * 2),
            param = c("I(y8 * 2)" = "ppp")) +
     latent(dem65 ~ z1*z2) +
     regression(dem60 ~ ind60) +
     intercept(item1 ~ 1) +
     covariance(item1 ~ item2) # + group(bbb)
m

m <- m + dataset(data.frame(aaa = gl(2, 5), bbb = gl(5, 2), c = 3))
m

m <- m + constraint(demo65_z1 >= 100)
str(m, 2)



a <- constraint(demo65_z1 >= 100)
b <- constraint(b1 == (b2 + b3)^2)



### Another example:
m <- latent(visual ~ x1 + x2 + x3) +
     latent(textual ~ x4 + x5 + x6) +
     latent(speed ~ x7 + x8 + x9)

r <- semrepr(m)


