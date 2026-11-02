## kedd -- gallery of kernels
## Run with: demo("kernels", package = "kedd")

library(kedd)

kernels <- c("gaussian", "epanechnikov", "uniform", "triangular", "biweight",
             "triweight", "tricube", "cosine", "silverman", "logistic")

## 1. The ten named kernels (r = 0) -------------------------------------------
for (k in kernels) plot(kernel.fun(kernel = k))

## 2. Derivatives of the biweight kernel up to its last non-zero order --------
for (r in 0:4) plot(kernel.fun(kernel = "biweight", deriv.order = r))

## 3. Convolutions K^(r) * K^(r), used by the cross-validation criteria -------
plot(kernel.conv(kernel = "gaussian"))
plot(kernel.conv(kernel = "epanechnikov", deriv.order = 1L))
plot(kernel.conv(kernel = "logistic", deriv.order = 2L))

## 4. Kernel constants mu2(K), mu4(K), R(K^(r)) --------------------------------
t(sapply(kernels, function(k) Cpp_kernel_constants(k, 0L)[c("mu2", "mu4", "R")]))

## 5. The power family Pow(s,t): K(x) = c (1 - |x|^s)^t on [-1, 1] -----------
##    pow(2,1) = epanechnikov, pow(2,2) = biweight, pow(3,3) = tricube, ...
all.equal(kernel.fun(kernel = "pow(2,2)")@y, kernel.fun(kernel = "biweight")@y)

for (st in list(c(1, 2), c(2, 2), c(2, 6), c(4, 2), c(1.5, 2.5)))
  plot(kernel.fun(kernel = kernel.pow(st[1], st[2])))

## normalising constant and moments of a non-standard member
Cpp_kernel_constants(kernel.pow(1.5, 2.5), 0L)

## derivatives of a non-integer member
plot(kernel.fun(kernel = "pow(1.5,2.5)", deriv.order = 1L))
