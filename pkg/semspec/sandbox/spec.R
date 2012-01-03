
## sapply(list.files("../R", full = TRUE), source)

m1 <- latent(ind60 ~ x1 + x2 + x3)
m2 <- latent(dem60 ~ y1 + y2 + y3 + y4)
m3 <- regression(dem60 ~ ind60)
m4 <- covariance(item1 ~ item2)
m5 <- latent(dem60 ~ y1 + y2 + y3 + y4, group = x7)  ## use | to group?

str(m1 + m2)
str(m1 + m3)
str(m1 + m5)


m <- latent(ind60 ~ x1 + x2 + x3) +
     latent(dem60 ~ y1 + y2 + y3 + y4, group = aaa) +
     latent(dem65 ~ y5 + y6 + y7 + y8) +
     regression(dem60 ~ ind60) +
     group(x7) +
     intercept(item1)

str(m)
str(m + data(iris))



### Check contents:

has_formula(m)
has_data(m)
has_data(m + data(iris))
has_global_group(m)
has_constraint(m)
has_intercept(m)


### Replacement of group and data:

m$group

(m + group(testtest))$group
str(m + data(iris) + data(women))  ## the right most data is the used one



### Model with constraints:

latent(y ~ b1 * x1 + b2 * x2 + b3 * x3) +
constraint(b1 == (b2 + b3)^2) +
constraint(b1 > exp(b2 + b3))



### Reflective vs formative model:

latent(MV1 + MV2 ~ LV)
latent(LV ~ MV1 + MV2)

