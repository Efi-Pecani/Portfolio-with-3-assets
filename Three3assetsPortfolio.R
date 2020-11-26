install.packages("data.table")
library(scales)
library(data.table)
# load the data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)
# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)
# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)
# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)
# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))
three_assets[, wz := 1 - wx - wy]
# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]
# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets
##               wx          wy           wz       er_p       sd_p
##      1: 0.000000 0.000000000 1.000000e+00 0.04000000 0.03005254
##      2: 0.000000 0.001001001 9.989990e-01 0.03998999 0.03002259
##      3: 0.000000 0.002002002 9.979980e-01 0.03997998 0.02999265
##      4: 0.000000 0.003003003 9.969970e-01 0.03996997 0.02996272
##      5: 0.000000 0.004004004 9.959960e-01 0.03995996 0.02993281
##     ---
## 500348: 0.996997 0.003003003 4.336809e-17 0.06984550 0.04984333
## 500349: 0.997998 0.000000000 2.002002e-03 0.06990552 0.04989176
## 500350: 0.997998 0.001001001 1.001001e-03 0.06989551 0.04989239
## 500351: 0.998999 0.000000000 1.001001e-03 0.06993552 0.04994212
## 500352: 1.000000 0.000000000 0.000000e+00 0.06996551 0.04999250
# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

