## Load Data
library("readr")
library("dplyr")
URL <-  "https://bit.ly/hadcrutv4"
gtemp <- read_table(URL, col_types = "nnnnnnnnnnnn", col_names = FALSE) |>
    select(num_range("X", 1:2)) |>
    setNames(nm = c("Year", "Temperature"))

# look at the data
gtemp

# fit our first GAM
library("mgcv")
m <- gam(Temperature ~ s(Year), data = gtemp, method = "REML")

# look at the model
library("gratia")

overview(m)

summary(m)

# plot the fitted smooth
draw(m, residuals = TRUE, rug = FALSE)
# or plot(m)

# model diagnostics
appraise(m)
# or
# layout(matrix(1:4, ncol = 2, byrow = TRUE))
# gam.check(m)
# layout(1)