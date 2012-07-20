
## @knitr unnamed-chunk-1
library("semspec")

sprint <- function(x, h = 6, t = 0) {
  o <- capture.output(print(x))

  oh <- head(o, h)
  ot <- tail(o, t)

  cat(paste(c(oh, "...", ot), collapse = "\n"), "\n")
}

sprint2 <- function(x, s = 1, e = Inf, sdots = TRUE, edots = TRUE) {
  o <- capture.output(print(x))
  n <- length(o)
  
  if ( e > n ) e <- n
  
  o <- o[s:e]
  
  if ( s > 1 & sdots ) o <- c("...", o)
  if ( e < n & edots ) o <- c(o, "...")
  
  cat(paste(o, collapse = "\n"), "\n")
}


## @knitr unnamed-chunk-2
## Model formulas:
y ~ f1 + x1 + x2


## @knitr c1
## Structural models:
regression(y ~ f1 + x1 + x2)


## @knitr c1
## Structural models:
regression(y ~ f1 + x1 + x2)


## @knitr c2
## Structural models:
regression(y ~ f1 + x1 + x2) +
## Measurement models:
latent(f1 ~ y1 + y2 + y3)


## @knitr c2
## Structural models:
regression(y ~ f1 + x1 + x2) +
## Measurement models:
latent(f1 ~ y1 + y2 + y3)


## @knitr c3
## Structural models:
regression(y ~ f1 + x1 + x2) +
## Measurement models:
latent(f1 ~ y1 + y2 + y3) +
## Covariances and intercepts:
covariance(y1 ~ y2) + intercept(y1 ~ 1)


## @knitr c3
## Structural models:
regression(y ~ f1 + x1 + x2) +
## Measurement models:
latent(f1 ~ y1 + y2 + y3) +
## Covariances and intercepts:
covariance(y1 ~ y2) + intercept(y1 ~ 1)


## @knitr c4
## Interactions:
regression(y ~ f1 + x1*x2)


## @knitr c5
## Arithmetic expressions:
regression(y ~ f1 + x1 + I(3.1415 * x2))


## @knitr c6
## Parameter labels:
regression(y ~ f1 + x1 + I(3.1415 * x2),
           param = c("I(3.1415 * x2)" = "pix2"))


## @knitr c7
## Groups:
regression(y ~ f1 + x1) + latent(f1 ~ y1 + y2 | g1)


## @knitr c8
## Global group:
regression(y ~ f1 + x1) + latent(f1 ~ y1 + y2 | g1) + group(g2)


## @knitr unnamed-chunk-3
set.seed(1234)
N <- 100
dat <- data.frame(x1 = rnorm(N, mean = 0, sd = 1),
                  y1 = rnorm(N, mean = 5, sd = 3),
                  y2 = rnorm(N, mean = 100, sd = 20),
                  g1 = gl(2, N/2))


## @knitr c9
## Model specification:
regression(y ~ f1 + x1) + 
latent(f1 ~ y1 + y2)


## @knitr unnamed-chunk-4
## Model specification:
regression(y ~ f1 + x1) + 
latent(f1 ~ y1 + y2) +
## Dataset:
dataset(dat)


## @knitr unnamed-chunk-5
## Model specification:
regression(y ~ f1 + x1 | g1) + 
latent(f1 ~ y1 + y2) + 
## Dataset:
dataset(dat)


## @knitr unnamed-chunk-6
## Model specification:
regression(y ~ f1 + x1 | g1) + 
latent(f1 ~ y1 + y2) + 
## Dataset:
dataset(dat) +
## Constraints:
constraint(f1_y1 == 10)


## @knitr unnamed-chunk-7
## Model specification:
regression(y ~ f1 + x1 | g1) + 
latent(f1 ~ y1 + y2) + 
## Dataset:
dataset(dat) +
## Constraints:
constraint(f1_y1 == 10) +
constraint(y_f1:2 == y_f1:1)


## @knitr unnamed-chunk-8
library("lavaan")
data("HolzingerSwineford1939")


## @knitr unnamed-chunk-9
## Measurement model
m <- latent(visual ~ x1 + x2 + x3) +
     latent(textual ~ x4 + x5 + x6) +
     latent(speed ~ x7 + x8 + x9)
m <- m + dataset(HolzingerSwineford1939)
## MV variances:
m <- m + covariance(x1 ~ x1) + covariance(x2 ~ x2) +
         covariance(x3 ~ x3) + covariance(x4 ~ x4) +
         covariance(x5 ~ x5) + covariance(x6 ~ x6) +
         covariance(x7 ~ x7) + covariance(x8 ~ x8) +
         covariance(x9 ~ x9) 
## LV variances:
m <- m + covariance(visual ~ visual) +
         covariance(textual ~ textual) +
         covariance(speed ~ speed)
## LV covariance:
m <- m + covariance(visual ~ textual) +
         covariance(visual ~ speed) +
         covariance(textual ~ speed)    
## Constraints:
m <- m + constraint(visual_x1 == 1) +
         constraint(textual_x4 == 1) +
         constraint(speed_x7 == 1)


## @knitr unnamed-chunk-10
## Model specification summary:
summary(m)


## @knitr unnamed-chunk-11
sprint2(summary(m), s = 1, e = 5)
sprint2(summary(m), s = 26, e = 39, sdots = FALSE)


## @knitr unnamed-chunk-12
sprint2(summary(m), s = 40, e = 57)
sprint2(summary(m), s = 66, sdots = FALSE)


## @knitr unnamed-chunk-13
## Model specification plot (via qgraph):
plot(m)


## @knitr unnamed-chunk-14
regression(y ~ f1 + x1 + x2) +
latent(f1 ~ y1 + y2 + y3) +
constraint(y1_y2 == 10) + dataset(dat)


## @knitr unnamed-chunk-15
m2 <- regression(y ~ f1 + x1 + x2) +
      latent(f1 ~ y1 + y2 + y3) +
      constraint(y1_y2 == 10) #+ dataset(dat)
sprint(m2, 4, 0)


## @knitr unnamed-chunk-16
## Translation for the sem package:
as_sem_syntax(m)


## @knitr unnamed-chunk-17
t <- c(as_sem_syntax(m)[1:12], "...")
cat(paste(t, collapse = ""), "\n")


## @knitr unnamed-chunk-18
## Model fit with the sem package:
semfit_sem(m)


## @knitr unnamed-chunk-19
## ... semPLS and lavaan packages:
as_semPLS_syntax(m); semfit_semPLS(m)
as_lavaan_syntax(m); semfit_lavaan(m)


