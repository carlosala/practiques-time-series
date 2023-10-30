library(graphics)
library(readxl)
library(stats)
library(zoo)

# we're going to calculate the factor between 2023Q2 prices and all others
# inflation data comes monthly, we're using average between 3 months
# it comes as percentage annual variation, so we divide by 400 to get the quarterly
get_inflation <- function() {
  exc <- read_excel("inflacion-baleares.xlsx", range = "A10:B267", col_names = FALSE)
  data <- exc$"...2"
  data_len <- length(data) / 3
  inf_vec <- c(1)
  for (i in 2:data_len) {
    avg <- (data[i * 3 - 2] + data[i * 3 - 1] + data[i * 3]) / 3 / 4 / 100
    inf_vec[i] <- inf_vec[i - 1] * (1 + avg)
  }
  # we reverse the data to return it ordered by time
  return(rev(inf_vec))
}

exc_cost <- read_excel("coste-trabajador-baleares.xlsx", range = "A10:B95", col_names = FALSE)
# starts with 2023, we need to reverse the data!
cost <- rev(exc_cost$"...2")
inflation <- get_inflation()
cost_inf_adj <- c()
for (i in seq_along(cost)) {
  cost_inf_adj[i] <- cost[i] * inflation[i]
}

balears <- ts(cost, start = c(2002, 1), end = c(2023, 2), frequency = 4)
balears_avg <- mean(balears)
balears_avg4 <- rollmean(balears, 4)
balears_avg12 <- rollmean(balears, 12)
balears_inf_adj <- ts(cost_inf_adj, start = c(2002, 1), end = c(2023, 2), frequency = 4)
balears_inf_adj_avg <- mean(balears_inf_adj)
balears_inf_adj_avg4 <- rollmean(balears_inf_adj, 4)
balears_inf_adj_avg12 <- rollmean(balears_inf_adj, 12)
plot.zoo(cbind(balears, balears_avg, balears_avg4, balears_avg12),
  main = "Cost per treballador a les Illes Balears",
  plot.type = "single",
  col = c("purple", "green", "blue", "cyan"),
  xlab = "Temps", ylab = "Cost (euros)"
)
legend("topleft",
  c("Sèrie", "Mitjana", "Mitjana mòbil (1 any)", "Mitjana mòbil (3 anys)"),
  fill = c("purple", "green", "blue", "cyan"),
)
plot.zoo(cbind(balears_inf_adj, balears_inf_adj_avg, balears_inf_adj_avg4, balears_inf_adj_avg12),
  main = "Cost per treballador a les Illes Balears (ajustat inflació)",
  plot.type = "single",
  col = c("purple", "green", "blue", "cyan"),
  xlab = "Temps", ylab = "Cost (euros)"
)
legend("bottomleft",
  c("Sèrie", "Mitjana", "Mitjana mòbil (1 any)", "Mitjana mòbil (3 anys)"),
  fill = c("purple", "green", "blue", "cyan"),
)

plot.zoo(ts(inflation, start = c(2002, 1), end = c(2023, 2), frequency = 4),
  main = "Valor real de l'euro",
  plot.type = "single", col = "red",
  xlab = "Temps", ylab = "Valor (€ 2023 Q2)",
  log = "y"
)

# let's calculate the linear regression!
t <- c(seq_along(balears_inf_adj))
lin_reg <- lm(unclass(balears_inf_adj) ~ t)
a <- lin_reg$coefficients[1]
b <- lin_reg$coefficients[2]
summary(lin_reg)
balears_dec <- decompose(balears_inf_adj)
bal_sea <- balears_dec$seasonal
(bal_sea[3] + a + b * 87) * 1.01
(bal_sea[4] + a + b * 88) * 1.01^2

plot.zoo(cbind(balears_inf_adj, bal_sea + a + b * t),
  main = "Cost per treballador a les Illes Balears (ajustat per inflació)",
  plot.type = "single",
  col = c("blue", "red"),
  xlab = "Temps", ylab = "Cost (euros)"
)
legend("bottomleft",
  c("Sèrie", "Regressió (estacional)"),
  fill = c("blue", "red"),
)

balears_inf_adj_hw <- HoltWinters(balears_inf_adj)
hw_pred <- predict(balears_inf_adj_hw, n.ahead = 2, prediction.interval = TRUE, level = 0.95)
hw_pred * 1.01 # inflation-adjusted for Q3
hw_pred * 1.01^2 # and Q4

hw_pred * 1.01 / balears_inf_adj[83] * 100
hw_pred * 1.01^2 / balears_inf_adj[84] * 100

plot(balears_inf_adj_hw,
  main = "Cost per treballador a les Illes Balears (ajustat per inflació)",
  plot.type = "single",
  col = c("blue", "red"),
  xlab = "Temps", ylab = "Cost (euros)"
)
legend("bottomleft",
  c("Sèrie", "Suavitzat Holt-Winters"),
  fill = c("blue", "red"),
)
