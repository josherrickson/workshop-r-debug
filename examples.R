# 1.

mt <- attr(mf, "terms")
y <- model.response(mf, "numeric")
w <- as.vector(model.weights(mf))
if (!is.null(w) && !is.numeric(w)) 
  stop("'weights' must be a numeric vector")
offset <- as.vector(model.offset(mf))
if (!is.null(offset)) {
  if (length(offset) != NROW(y)) 
    stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                  length(offset), NROW(y)), domain = NA)
}


# 2.

foo <- function(x) {
  x <- x + 1
  bar(x)
}

bar <- function(x) {
  a <- baz(x)
  return(a)
}

baz <- function(x) {
  sum(x, "a")
}
foo(3)

# 3.

foo <- function(x, y = 3) {
  browser()
  bar(x, y)
}
bar <- function(a, b) {
  out <- b
  for (i in 1:a) {
    out <- out + b
  }
  out
}
foo(2)

# 4.

smartSum <- function(x, y) {
  tryCatch({
    sum(x, y)
  },
  error = function(e) {
    print(e)
    sum(as.numeric(x), as.numeric(y))
  })
}

# 5.
x <- 3
x == 2
x <- NA
x == 2
x <- NULL
x == 2
!is.null(x) && !is.na(x) && x == 2

# 6.
reps <- 10000
n <- 100
beta0 <- 2
beta1 <- .7

save <- vector()

for (i in 1:reps) {
  x <- rnorm(n)
  y <- beta0 + beta1*x + rnorm(n)
  
  # Add rounding.
  rounded <- sample(1:n, .25*n, replace = FALSE)
  x[rounded] <- ceiling(x[rounded])
  
  coef <- lm(y ~ x)$coef[2]
  save <- c(save, coef)
}
save <- data.frame(save)
ggplot(save, aes(x = save)) + geom_density() + geom_vline(xintercept = beta1)

# 7.
data(Orange)
lm(circumference ~ age, data = Orange)
y <- matrix(Orange$circumference, ncol = 1)
x <- cbind(1, Orange$age)
.lm.fit(x, y)$coef
library(microbenchmark)
microbenchmark(lm(circumference ~ age, data = Orange),
              .lm.fit(x, y)$coef)

# 8.
y <- matrix(y, ncol = 1)
x <- cbind(1, x)

coef <- .lm.fit(x, y)$coef[2]

# 9.
x <- rnorm(100)
microbenchmark(mean(x),
               sum(x)/length(x))