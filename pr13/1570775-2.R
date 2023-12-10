library(TSA)
library(fGarch)
library(forecast)
library(lmtest)
library(readxl)
library(stats)
library(tseries)
library(zoo)

exc <- read_excel("muertes-semanales-las-palmas-hombres.xlsx", range = "B11:B730", col_names = FALSE)
# starts with 2023, we need to reverse the data!
deaths_vec <- rev(exc$"...1")
start <- as.yearmon(as.Date("2010-01-01"))

# take only 13 years (2010 - 2022)
deaths <- ts(deaths_vec[1:(52 * 13)], start = start, frequency = 52)
deaths_avg <- mean(deaths)
deaths_avg52 <- rollmean(deaths, 52)
plot.zoo(cbind(deaths, deaths_avg, deaths_avg52),
  main = "Morts (sexe masculí). Las Palmas",
  plot.type = "single",
  col = c("purple", "green", "red"),
  xlab = "Temps", ylab = "Morts setmanals"
)
legend("topleft",
  c("Sèrie", "Mitjana", "Mitjana mòbil (52 setmanes)"),
  fill = c("purple", "green", "red"),
)

# apartat b
summary(lm(deaths ~ seq_along(deaths)))

# apartat c
deaths_seasonal <- decompose(deaths)$seasonal
plot.zoo(deaths_seasonal[1:52],
  main = "Component estacional de la sèrie.",
  plot.type = "single",
  col = "red",
  xlab = "Setmana", ylab = "Component estacional"
)

# only take 13 years (2010 - 2022)
deaths_no_seas <- ts(deaths_vec[1:(52 * 13)] - deaths_seasonal, start = start, frequency = 52)
deaths_no_seas_avg <- mean(deaths_no_seas)
deaths_no_seas_avg52 <- rollmean(deaths_no_seas, 52)
plot.zoo(cbind(deaths_no_seas, deaths_no_seas_avg, deaths_no_seas_avg52),
  main = "Morts (sexe masculí). Las Palmas. Sense component estacional",
  plot.type = "single",
  col = c("purple", "green", "red"),
  xlab = "Temps", ylab = "Morts setmanals"
)
legend("topleft",
  c("Sèrie (sense component estacional)", "Mitjana", "Mitjana mòbil (52 setmanes)"),
  fill = c("purple", "green", "red"),
)
matrix_dades <- matrix(data = deaths, nrow = 52)
boxplot(t(matrix_dades), main = "Boxplot. Morts per setmana de l'any.")

# apartat d
# només tenim en compte del 2020 en endavant
covid_marker <- c(rep(0, 52 * 10), rep(1, 52 * 2), rep(0, 52))
summary(lm(deaths ~ covid_marker))

# apartat e
deaths_diff1_52 <- diff(diff(deaths), 52)
deaths_diff1_52_avg <- mean(deaths_diff1_52)
deaths_diff1_52_avg52 <- rollmean(deaths_diff1_52, 52)
plot.zoo(cbind(deaths_diff1_52, deaths_diff1_52_avg, deaths_diff1_52_avg52),
  main = "Sèrie diferenciada.",
  plot.type = "single",
  col = c("purple", "green", "red"),
  xlab = "Temps", ylab = "Morts setmanals (diferenciades)"
)
legend("topleft",
  c("Sèrie", "Mitjana", "Mitjana mòbil (52 setmanes)"),
  fill = c("purple", "green", "red"),
)

# apartat f
matrix_diff <- matrix(data = deaths_diff1_52[1:(floor(length(deaths_diff1_52) / 52) * 52)], nrow = 52)
boxplot(t(matrix_diff), main = "Boxplot")

# apartat g
adf.test(deaths_diff1_52)

# apartat h
acf(deaths_diff1_52, main = "ACF Sèrie diferenciada")
pacf(deaths_diff1_52, main = "PACF Sèrie diferenciada")
eacf(deaths_diff1_52)

# apartat i
deaths_fit <- auto.arima(deaths)
coeftest(deaths_fit)

# apartat j
deaths_covid_fit <- auto.arima(deaths, xreg = covid_marker)
coeftest(deaths_covid_fit)

# apartat k
start_2023 <- as.yearmon(as.Date("2023-01-01"))
forecast_2023 <- forecast(deaths, h = 52, model = deaths_fit)
# take only 2023
deaths_2023 <- ts(deaths_vec[(52 * 13 + 1):length(deaths_vec)], start = start_2023, frequency = 52)
plot.zoo(deaths_2023,
  main = "Morts (sexe masculí). Las Palmas",
  plot.type = "single",
  col = "purple",
  xlab = "Temps", ylab = "Morts setmanals"
)
lines(forecast_2023$mean, col = "red")
legend("topleft",
  c("Valors reals", "Forecast"),
  fill = c("purple", "red"),
)

# apartat l
shapiro.test(deaths_fit$residuals)
shapiro.test(deaths_fit$residuals^2)
Box.test(deaths_fit$residuals, type = "Ljung-Box")
Box.test(deaths_fit$residuals^2, type = "Ljung-Box")

# apartat m
AIC(garch(deaths, order = c(1, 0)))
AIC(garch(deaths, order = c(1, 1)))
AIC(garch(deaths, order = c(2, 1)))
coef(garch(deaths, order = c(2, 1)))
