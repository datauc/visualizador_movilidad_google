library(ggplot2)
library(EpiEstim)
library(xts)

# Selección país ----------------------------------------------------------

pais <- 'Chile'

download.file('https://covid.ourworldindata.org/data/ecdc/full_data.csv', 'full_data.csv')
data <- read.csv(file = 'full_data.csv')
data_subset <- data[data$location == paste(pais), ]
tiempo <- as.Date(data_subset[, 'date'])
casos <- pmax(data_subset[, 'new_cases'], 0)
incidencia<- data.frame(dates = tiempo, I = casos)

prom_si <- 4.7
std_si <- 2.9
delta_si <- 30
si_distr_discreta<- discr_si(seq(0, delta_si), prom_si, std_si)

# R estimado --------------------------------------------------------------

res <- estimate_R(incid = incidencia,
                  method = 'non_parametric_si',
                  config = make_config(list(si_distr = si_distr_discreta)))

t2 = tail(tiempo, -7)
serieR <- xts(res$R[, c('Median(R)')], order.by = t2)
serieRl <- xts(res$R[, c('Quantile.0.025(R)')], order.by = t2)
serieRu <- xts(res$R[, c('Quantile.0.975(R)')], order.by = t2)

# Curva,SI distribución y R estimado ------------------------------------

plot(res, legend = FALSE)

# Guardar serie -----------------------------------------------------------

readr::write.csv(cbind('Date' = time(t2), 'Rt' = serieR, 'Quantile_2.5_Low' = serieRl,
                       'Quantile_97.5_Upper' = serieRu), file = 'resultRtPT.csv')

# Plot R estimado ---------------------------------------------------------

plot_estimR <- plot(serieR, ylim = c(0, 5), main = 'R(t)', xlab = 'R', ylab = 'Tempo')

lines(serieRl)
lines(serieRu)

ref <- xts(rep(1.0, length(serieR)), order.by = time(serieR))

lines(ref, col = 'red')
