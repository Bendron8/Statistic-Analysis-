library(ggplot2)

f <- function(x) {
  return(x^4 - 4*x^3 - 2*x^2 - x + 5)
}

normal <- function(mi, sigma) {
  u1 <- runif(1)
  u2 <- runif(1)
  X <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  Y <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  return(mi + sigma * X)
}

u<- runif(1)
x_t <- 8*u-3
N <- 10^5
sigma <- 1


values <- c()
k <- c()


for (i in 1:N) {
  values <- c(values, x_t)
  x_lin <- normal(x_t, sigma)
  alpha <- exp(-f(x_lin)) / exp(f(x_t))
  a <- runif(1)
  
  if (a <= alpha) {
    x_t <- x_lin
  }
}

x_min <- values[which.min(sapply(values, f))]
f_min <- f(x_min)
print(paste("Mínimo encontrado: x =", x_min, ", f(x) =", f_min))

comprimento <- seq(-3, 5, length.out = N)
k <- sapply(comprimento, f)

ggplot(data.frame(x = comprimento, y = k), aes(x = x, y = y)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(aes(x = x_min, y = f_min), color = "red", size = 3) +
  geom_vline(xintercept = x_min, color = "red", linetype = "dashed", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "x",
       y = "f(x)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1))

}