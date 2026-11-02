## kedd -- quick tour
## Run with: demo("kedd", package = "kedd")

library(kedd)
set.seed(2027)
x <- c(rnorm(200, -1, 0.5), rnorm(150, 1.5, 0.7))   # bimodal sample

## 1. A kernel and its derivatives -------------------------------------------
plot(kernel.fun(kernel = "biweight"))
plot(kernel.fun(kernel = "biweight", deriv.order = 1L))

## 2. Bandwidth selection -----------------------------------------------------
bw <- h.ucv(x)                 # unbiased (least-squares) cross-validation
bw                             # show() method
plot(bw)                       # CV curve, h* marked

## 3. Density estimate with this bandwidth ------------------------------------
f0 <- dkde(data = x, h = bw@h)
plot(f0)

## 4. First derivative: a larger bandwidth is needed (h ~ n^{-1/(2r+5)}) --------
bw1 <- h.ucv(x, deriv.order = 1L)
c(r0 = bw@h, r1 = bw1@h)
plot(dkde(data = x, h = bw1@h, deriv.order = 1L))

## 5. Other kernels: logistic and the power family Pow(s,t) ------------------
h.bcv(x, kernel = "logistic")
h.bcv(x, kernel = kernel.pow(2, 4))   # same as kernel = "pow(2,4)"

## 6. Several criteria on one ggplot2 figure ----------------------------------
p <- plot(h.ucv(x))
p$plot + lines(h.bcv(x), color = "darkred") + lines(h.mcv(x), color = "darkorange")

## 7. Example datasets: Marron & Wand (1992) mixtures (?"kedd-data") ---------
f_claw <- function(t) 0.5 * dnorm(t) +
  rowSums(sapply(0:4, function(l) 0.1 * dnorm(t, l / 2 - 1, 0.1)))
pc <- plot(dkde(data = claw, h = h.ucv(claw)@h))
pc + ggplot2::stat_function(fun = f_claw, linetype = "dashed")
plot(dkde(data = bimodal, h = h.scv(bimodal, deriv.order = 1)@h, deriv.order = 1))

## 8. Threads: 0 = all cores (default), k = k threads, identical results ------
c(all = Cpp_omp_threads(0), one = Cpp_omp_threads(1))
all.equal(h.ucv(x, nthreads = 1)@h, h.ucv(x, nthreads = 0)@h)
