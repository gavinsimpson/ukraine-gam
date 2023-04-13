## ----setup, include=FALSE, cache=FALSE----------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(cache = TRUE, dev = 'svg', echo = TRUE, message = FALSE, warning = FALSE,
                      fig.height=6, fig.width = 1.777777*6)

library('here')
library('mgcv')
library('gratia')
library('ggplot2')
library('purrr')
library('mvnfast')
library("tibble")
library('gganimate')
library('tidyr')
library("knitr")
library("viridis")
library('readr')
library('dplyr')
library("patchwork")
library("forcats")

## plot defaults
theme_set(theme_minimal(base_size = 16, base_family = 'Fira Sans'))

## constants
anim_width <- 1900
anim_height <- anim_width / 1.77777777
anim_dev <- 'png'
anim_res <- 250


## ----use-course, echo = TRUE, eval = FALSE------------------------------------
## usethis::use_course("https://bit.ly/ukraine-gam")


## ----hadcrut-temp-example, echo = FALSE---------------------------------------
library("readr")
library("dplyr")
URL <-  "https://bit.ly/hadcrutv4"
gtemp <- read_table(URL, col_types = "nnnnnnnnnnnn", col_names = FALSE) %>%
    select(num_range("X", 1:2)) %>% setNames(nm = c("Year", "Temperature"))

## Plot
gtemp_plt <- ggplot(gtemp, aes(x = Year, y = Temperature)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = expression(Temeprature ~ degree * C))
gtemp_plt


## ----hadcrut-temp-example, echo = FALSE---------------------------------------
library("readr")
library("dplyr")
URL <-  "https://bit.ly/hadcrutv4"
gtemp <- read_table(URL, col_types = "nnnnnnnnnnnn", col_names = FALSE) %>%
    select(num_range("X", 1:2)) %>% setNames(nm = c("Year", "Temperature"))

## Plot
gtemp_plt <- ggplot(gtemp, aes(x = Year, y = Temperature)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = expression(Temeprature ~ degree * C))
gtemp_plt


## ----hadcrut-temp-polynomial, echo = FALSE------------------------------------
p <- c(1,3,8,15)
N <- 300
newd <- with(gtemp, data.frame(Year = seq(min(Year), max(Year), length = N)))
polyFun <- function(i, data = data) {
    lm(Temperature ~ poly(Year, degree = i), data = data)
}
mods <- lapply(p, polyFun, data = gtemp)
pred <- vapply(mods, predict, numeric(N), newdata = newd)
colnames(pred) <- p
newd <- cbind(newd, pred)
polyDat <- gather(newd, Degree, Fitted, - Year)
polyDat <- mutate(polyDat, Degree = ordered(Degree, levels = p))
gtemp_plt + geom_line(data = polyDat, mapping = aes(x = Year, y = Fitted, colour = Degree),
                      size = 1.5, alpha = 0.9) +
    scale_color_brewer(name = "Degree", palette = "PuOr") +
    theme(legend.position = "right")


## ----read-hadcrut, echo = TRUE------------------------------------------------
## Load Data
library("readr")
library("dplyr")
URL <-  "https://bit.ly/hadcrutv4"
gtemp <- read_table(URL, col_types = "nnnnnnnnnnnn", col_names = FALSE) %>%
    select(num_range("X", 1:2)) %>% setNames(nm = c("Year", "Temperature"))


## ----show-hadcrut, echo = TRUE------------------------------------------------
gtemp


## ----hadcrutemp-fitted-gam, echo = TRUE, results = 'hide'---------------------
library("mgcv")
m <- gam(Temperature ~ s(Year), data = gtemp, method = "REML")
summary(m)


## ----hadcrutemp-fitted-gam, echo = FALSE--------------------------------------
library("mgcv")
m <- gam(Temperature ~ s(Year), data = gtemp, method = "REML")
summary(m)


## ----hadcrtemp-plot-gam, echo = FALSE-----------------------------------------
N <- 300
newd <- as_tibble(with(gtemp, data.frame(Year = seq(min(Year), max(Year), length = N))))
pred <- as_tibble(as.data.frame(predict(m, newdata = newd, se.fit = TRUE,
                                        unconditional = TRUE)))
pred <- bind_cols(newd, pred) %>%
    mutate(upr = fit + 2 * se.fit, lwr = fit - 2*se.fit)

ggplot(gtemp, aes(x = Year, y = Temperature)) +
    geom_point() +
    geom_ribbon(data = pred,
                mapping = aes(ymin = lwr, ymax = upr, x = Year), alpha = 0.4, inherit.aes = FALSE,
                fill = "#fdb338") +
    geom_line(data = pred,
              mapping = aes(y = fit, x = Year), inherit.aes = FALSE, size = 1, colour = "#025196") +
    labs(x = 'Year', y = expression(Temeprature ~ degree * C))


## ----hadcrtemp-plot-gam, echo = FALSE-----------------------------------------
N <- 300
newd <- as_tibble(with(gtemp, data.frame(Year = seq(min(Year), max(Year), length = N))))
pred <- as_tibble(as.data.frame(predict(m, newdata = newd, se.fit = TRUE,
                                        unconditional = TRUE)))
pred <- bind_cols(newd, pred) %>%
    mutate(upr = fit + 2 * se.fit, lwr = fit - 2*se.fit)

ggplot(gtemp, aes(x = Year, y = Temperature)) +
    geom_point() +
    geom_ribbon(data = pred,
                mapping = aes(ymin = lwr, ymax = upr, x = Year), alpha = 0.4, inherit.aes = FALSE,
                fill = "#fdb338") +
    geom_line(data = pred,
              mapping = aes(y = fit, x = Year), inherit.aes = FALSE, size = 1, colour = "#025196") +
    labs(x = 'Year', y = expression(Temeprature ~ degree * C))


## ----smooth-fun-animation, results = FALSE, echo = FALSE, cache = TRUE--------
f <- function(x) {
    x^11 * (10 * (1 - x))^6 + ((10 * (10 * x)^3) * (1 - x)^10)
}

draw_beta <- function(n, k, mu = 1, sigma = 1) {
    rmvn(n = n, mu = rep(mu, k), sigma = diag(rep(sigma, k)))
}

weight_basis <- function(bf, x, n = 1, k, ...) {
    beta <- draw_beta(n = n, k = k, ...)
    out <- sweep(bf, 2L, beta, '*')
    colnames(out) <- paste0('f', seq_along(beta))
    out <- as_tibble(out)
    out <- add_column(out, x = x)
    out <- pivot_longer(out, -x, names_to = 'bf', values_to = 'y')
    out
}

random_bases <- function(bf, x, draws = 10, k, ...) {
    out <- rerun(draws, weight_basis(bf, x = x, k = k, ...))
    out <- bind_rows(out)
    out <- add_column(out, draw = rep(seq_len(draws), each = length(x) * k),
                      .before = 1L)
    class(out) <- c("random_bases", class(out))
    out
}

plot.random_bases <- function(x, facet = FALSE) {
    plt <- ggplot(x, aes(x = x, y = y, colour = bf)) +
        geom_line(lwd = 1, alpha = 0.75) +
        guides(colour = FALSE)
    if (facet) {
        plt + facet_wrap(~ draw)
    }
    plt
}

normalize <- function(x) {
    rx <- range(x)
    z <- (x - rx[1]) / (rx[2] - rx[1])
    z
}

set.seed(1)
N <- 500
data <- tibble(x     = runif(N),
               ytrue = f(x),
               ycent = ytrue - mean(ytrue),
               yobs  = ycent + rnorm(N, sd = 0.5))

k <- 10
knots <- with(data, list(x = seq(min(x), max(x), length = k)))
sm <- smoothCon(s(x, k = k, bs = "cr"), data = data, knots = knots)[[1]]$X
colnames(sm) <- levs <- paste0("f", seq_len(k))
basis <- pivot_longer(cbind(sm, data), -(x:yobs), names_to = 'bf')
basis

set.seed(2)
bfuns <- random_bases(sm, data$x, draws = 20, k = k)

smooth <- bfuns %>%
    group_by(draw, x) %>%
    summarise(spline = sum(y)) %>%
    ungroup()

p1 <- ggplot(smooth) +
    geom_line(data = smooth, aes(x = x, y = spline), lwd = 1.5) +
    labs(y = 'f(x)', x = 'x') +
    theme_minimal(base_size = 14, base_family = 'Fira Sans')

smooth_funs <- animate(
    p1 + transition_states(draw, transition_length = 4, state_length = 2) +
    ease_aes('cubic-in-out'),
    nframes = 200, height = anim_height, width = anim_width, res = anim_res, dev = anim_dev)

anim_save('resources/spline-anim.gif', smooth_funs)


## ----basis-functions, fig.height=6, fig.width = 1.777777*6, echo = FALSE------
ggplot(basis,
       aes(x = x, y = value, colour = bf)) +
    geom_line(lwd = 2, alpha = 0.5) +
    guides(colour = FALSE) +
    labs(x = 'x', y = 'b(x)') +
    theme_minimal(base_size = 16, base_family = 'Fira Sans')


## ----basis-function-animation, results = 'hide', echo = FALSE, cache = TRUE----
bfun_plt <- plot(bfuns) +
    geom_line(data = smooth, aes(x = x, y = spline),
              inherit.aes = FALSE, lwd = 1.5) +
    labs(x = 'x', y = 'f(x)') +
    theme_minimal(base_size = 14, base_family = 'Fira Sans')

bfun_anim <- animate(
    bfun_plt + transition_states(draw, transition_length = 4, state_length = 2) + 
    ease_aes('cubic-in-out'),
    nframes = 200, height = anim_height, width = anim_width, res = anim_res, dev = anim_dev)

anim_save('resources/basis-fun-anim.gif', bfun_anim)


## ----example-data-figure, fig.height=6, fig.width = 1.777777*6, echo = FALSE----
data_plt <- ggplot(data, aes(x = x, y = ycent)) +
    geom_line(col = 'goldenrod', lwd = 2) +
    geom_point(aes(y = yobs), alpha = 0.2, size = 3) +
    labs(x = 'x', y = 'f(x)') +
    theme_minimal(base_size = 16, base_family = 'Fira Sans')
data_plt


## ----basis-functions-anim, results = "hide", echo = FALSE, cache = TRUE-------
sm2 <- smoothCon(s(x, k = k, bs = "cr"), data = data, knots = knots)[[1]]$X
beta <- coef(lm(ycent ~ sm2 - 1, data = data))
wtbasis <- sweep(sm2, 2L, beta, FUN = "*")
colnames(wtbasis) <- colnames(sm2) <- paste0("F", seq_len(k))
## create stacked unweighted and weighted basis
basis <- as_tibble(rbind(sm2, wtbasis)) %>%
    add_column(x = rep(data$x, times = 2),
               type = rep(c('unweighted', 'weighted'), each = nrow(sm2)),
               .before = 1L)
##data <- cbind(data, fitted = rowSums(scbasis))
wtbasis <- as_tibble(rbind(sm2, wtbasis)) %>%
    add_column(x      = rep(data$x, times = 2),
               fitted = rowSums(.),
               type   = rep(c('unweighted', 'weighted'), each = nrow(sm2))) %>%
    pivot_longer(-(x:type), names_to = 'bf')
basis <- pivot_longer(basis, -(x:type), names_to = 'bf')

p3 <- ggplot(data, aes(x = x, y = ycent)) +
    geom_point(aes(y = yobs), alpha = 0.2) +
    geom_line(data = basis,
              mapping = aes(x = x, y = value, colour = bf),
              lwd = 1, alpha = 0.5) +
    geom_line(data = wtbasis,
              mapping = aes(x = x, y = fitted), lwd = 1, colour = 'black', alpha = 0.75) +
    guides(colour = FALSE) +
    labs(y = 'f(x)', x = 'x') +
    theme_minimal(base_size = 14, base_family = 'Fira Sans')

crs_fit <- animate(p3 + transition_states(type, transition_length = 4, state_length = 2) + 
                   ease_aes('cubic-in-out'),
                   nframes = 100, height = anim_height, width = anim_width, res = anim_res,
                   dev = anim_dev)

anim_save('./resources/gam-crs-animation.gif', crs_fit)


## ----hadcrut-temp-penalty, echo = FALSE---------------------------------------
K <- 40
lambda <- c(10000, 1, 0.01, 0.00001)
N <- 300
newd <- with(gtemp, data.frame(Year = seq(min(Year), max(Year), length = N)))
fits <- lapply(lambda, function(lambda) gam(Temperature ~ s(Year, k = K, sp = lambda), data = gtemp))
pred <- vapply(fits, predict, numeric(N), newdata = newd)
op <- options(scipen = 100)
colnames(pred) <- lambda
newd <- cbind(newd, pred)
lambdaDat <- gather(newd, Lambda, Fitted, - Year)
lambdaDat <- transform(lambdaDat, Lambda = factor(paste("lambda ==", as.character(Lambda)),
                                                  levels = paste("lambda ==", as.character(lambda))))

gtemp_plt + geom_line(data = lambdaDat, mapping = aes(x = Year, y = Fitted, group = Lambda),
                      size = 1, colour = "#e66101") +
    facet_wrap( ~ Lambda, ncol = 2, labeller = label_parsed)
options(op)


## ----whole-basis-proces, echo = FALSE, fig.height = 4, fig.width = 1.777777 * 6----
K <- 13
df <- data.frame(x = seq(0, 1, length = 200))
knots <- data.frame(x = seq(0, 1, length.out = 11))
bs <- basis(s(x, bs = "ps", k = K), data = df,
    knots = list(x = seq(-3, 13) / 10))

# let's weight the basis functions (simulating model coefs)
set.seed(1)
betas <- data.frame(bf = factor(seq_len(K)), beta = rnorm(K))

unwtd_bs_plt <- bs |>
    draw() +
    geom_vline(aes(xintercept = x), data = knots, linetype = "dotted",
        alpha = 0.5)

# we need to merge the weights for each basis function with the basis object
bs <- bs |>
    left_join(betas, by = join_by("bf" == "bf")) |>
    mutate(value_w = value * beta)

# weighted basis
wtd_bs_plt <- bs |>
    ggplot(aes(x = x, y = value_w, colour = bf, group = bf)) +
    geom_line(show.legend = FALSE) +
    geom_vline(aes(xintercept = x), data = knots, linetype = "dotted",
        alpha = 0.5) +
    labs(y = expression(f(x)), x = "x")

# now we want to sum the weighted basis functions for each value of `x`
spl <- bs |>
    group_by(x) |>
    summarise(spline = sum(value_w))

take <- c(83, 115)
pts <- bs |>
    group_by(bf) |>
    slice(take)

# now plot
bs_plt <- bs |>
    ggplot(aes(x = x, y = value_w, colour = bf, group = bf)) +
    geom_line(show.legend = FALSE) +
    geom_line(aes(x = x, y = spline), data = spl, linewidth = 1.25,
              inherit.aes = FALSE) +
    geom_vline(aes(xintercept = x), data = knots, linetype = "dotted",
        alpha = 0.5) +
    geom_vline(xintercept = c(df$x[take]), linetype = "dashed",
        alpha = 1) +
    geom_point(data = pts, aes(x = x, y = value_w, colour = bf, group = bf),
        size = 2, show.legend = FALSE) +
    geom_point(data = slice(spl, take), aes(x = x, y = spline),
        size = 3, colour = "red", inherit.aes = FALSE) +
    labs(y = expression(f(x)), x = "x")

unwtd_bs_plt + wtd_bs_plt + bs_plt + plot_layout(ncol = 3)


## ----whole-basis-proces-2-model-----------------------------------------------
dat <- data_sim("eg1", seed = 4)
m <- gam(y ~ s(x0) + s(x1) + s(x2, bs = "bs") + s(x3),
         data = dat, method = "REML")


## ----whole-basis-proces-2-model-draw, fig.height = 5, fig.width = 1.777777 * 6----
draw(m) + plot_layout(ncol = 4)


## ----whole-basis-proces-2, echo = FALSE, fig.height = 6, fig.width = 1.777777 * 6----
# data to evaluate the basis at
# using the CRAN version of {gratia}, we need `m`
ds <- data_slice(m, x2 = evenly(x2, n = 200))
# from 0.9.0 (or current GitHub version) you can do
# ds <- data_slice(dat, x2 = evenly(x2, n = 200))

# generate a tidy representation of the fitted basis functions
x2_bs <- basis(m, term = "s(x2)", data = ds)

# compute values of the spline by summing basis functions at each x2
x2_spl <- x2_bs |>
    group_by(x2) |>
    summarise(spline = sum(value))

# evaluate the spline at the same values as we evaluated the basis functions
x2_sm <- smooth_estimates(m, "s(x2)", data = ds) |>
    add_confint()

take <- c(65, 175)
pts <- x2_bs |>
    group_by(bf) |>
    slice(take)

# now plot
x2_bs |>
    ggplot(aes(x = x2, y = value, colour = bf, group = bf)) +
    geom_line(show.legend = FALSE) +
    geom_ribbon(aes(x = x2, ymin = lower_ci, ymax = upper_ci),
                data = x2_sm,
                inherit.aes = FALSE, alpha = 0.2) +
    geom_line(aes(x = x2, y = est), data = x2_sm,
              linewidth = 1.5, inherit.aes = FALSE) +
    geom_vline(xintercept = c(ds$x2[take]), linetype = "dashed",
        alpha = 1) +
    geom_point(data = pts, aes(x = x2, y = value, colour = bf, group = bf),
        size = 2, show.legend = FALSE) +
    geom_point(data = slice(x2_sm, take), aes(x = x2, y = est),
        size = 3, colour = "red", inherit.aes = FALSE) +
    labs(y = expression(f(x2)), x = "x2")


## ----tprs-vs-tensor-product-setup, echo = FALSE-------------------------------
# following shows how tensor pruduct deals nicely with 
# badly scaled covariates (range of x 5% of range of z )
test1 <- function(x, z, sx = 0.3, sz = 0.4) {
  x <- x * 20
  (pi ** sx * sz) * (1.2 * exp(-(x - 0.2)^2 / sx^2 - (z - 0.3)^2 / sz^2) +
    0.8 * exp(-(x - 0.7)^2 / sx^2 - ( z - 0.8)^2 / sz^2))
}
n <- 500
x <- runif(n) / 20
z <- runif(n)
xs <- seq(0, 1, length = 30) / 20
zs <- seq(0, 1, length = 30)
pr <- tibble(x = rep(xs, 30), z = rep(zs, rep(30, 30)))
truth <- matrix(test1(pr$x, pr$z), 30, 30)
f <- test1(x, z)
y <- f + rnorm(n) * 0.2
df <- tibble(y = y, x = x, z = z)
truth_df <- pr %>% mutate(f = test1(x, z))
m_tprs <- gam(y ~ s(x, z), data = df, method = "REML")
m_te <- gam(y ~ te(x, z), data = df, method = "REML")

truth_plt <- truth_df %>%
  ggplot(aes(x = x, y = z, fill = f)) +
    geom_raster() +
    scale_fill_distiller(palette = "RdBu", type = "div") +
    geom_contour(aes(z = f), colour = "black", bins = 8) +
    labs(title = "f(x,z)")


## ----draw-tprs-vs-tensor-product-truth, echo = FALSE--------------------------
old_par <- par(mar = c(0, 2, 0, 0), bg = NA)
truth_plt +
  wrap_elements(panel = ~ persp(xs, zs, truth), clip = FALSE) +
  plot_layout(ncol = 2)
par(old_par)


## ----tprs-vs-tensor-product---------------------------------------------------
df
m_tprs <- gam(y ~ s(x, z), data = df, method = "REML")
m_te   <- gam(y ~ te(x, z), data = df, method = "REML")


## ----draw-tprs-vs-tensor-product, message = FALSE, out.width = "95%"----------
truth_plt + (draw(m_tprs) + coord_cartesian()) + draw(m_te) + plot_layout(ncol = 3)


## ----plot-tprs-vs-tensor-product-fake, eval = FALSE---------------------------
## layout(matrix(1:3, ncol = 3))
## persp(xs, zs, truth)
## vis.gam(m_tprs)
## vis.gam(m_te)
## layout(1)


## ----plot-tprs-vs-tensor-product, echo = FALSE, fig.width = 6, fig.width = 18----
old_par <- par(mar = c(0, 2, 0, 0), bg = NA)
persp1 <- wrap_elements(panel = ~ persp(xs, zs, truth), clip = FALSE)
persp2 <- wrap_elements(panel = ~ vis.gam(m_tprs), clip = FALSE)
persp3 <- wrap_elements(panel = ~ vis.gam(m_te), clip = FALSE)
plt <- persp1 + labs(title = "Truth") + 
  persp2 + labs(title = "TPRS") +
  persp3 + labs(title = "Tensor Product") +
  plot_layout(ncol = 3)
plt
par(old_par)


## ----echo = FALSE, out.width = "50%"------------------------------------------
knitr::include_graphics("resources/wood-gams-2ed-fig-5-17-tensor-product.svg")


## ----ranefs-------------------------------------------------------------------
m_nlme <- lme(travel ~ 1, data = Rail, ~ 1 | Rail, method = "REML") 

m_gam  <- gam(travel ~ s(Rail, bs = "re"), data = Rail, method = "REML")


## ----misspecify, echo = FALSE, out.width = "95%"------------------------------
set.seed(15)
model_list = c("right model",
               "wrong distribution",
               "heteroskedasticity",
               "dependent data",
               "wrong functional form")
n <- 60
sigma=1
x <- seq(-1,1, length=n)
model_data <- as.data.frame(expand.grid( x=x,model=model_list))
model_data$y <- 5*model_data$x^2 + 2*model_data$x
for(i in model_list){
  if(i == "right model"){
    model_data[model_data$model==i, "y"] <- model_data[model_data$model==i, "y"]+ 
      rnorm(n,0, sigma)
  } else if(i == "wrong distribution"){
    model_data[model_data$model==i, "y"] <- model_data[model_data$model==i, "y"]+ 
      rt(n,df = 3)*sigma
  } else if(i == "heteroskedasticity"){
    model_data[model_data$model==i, "y"] <- model_data[model_data$model==i, "y"]+  
      rnorm(n,0, sigma*10^(model_data[model_data$model==i, "x"]))
  } else if(i == "dependent data"){
    model_data[model_data$model==i, "y"] <- model_data[model_data$model==i, "y"]+ 
      arima.sim(model = list(ar=c(.7)), n = n,sd=sigma) 
  } else if(i=="wrong functional form") {
    model_data[model_data$model==i, "y"] <- model_data[model_data$model==i, "y"]+ 
      rnorm(n,0, sigma) + ifelse(model_data[model_data$model==i, "x"]>0, 5,-5)
  }
}
ggplot(aes(x,y), data= model_data)+
  geom_point()+
  geom_line(color=ifelse(model_data$model=="dependent data", "black",NA))+
  facet_wrap(~model)+
  geom_smooth(method=gam, formula = y~s(x,k=12),method.args = list(method="REML"))+
  theme(strip.text = element_text(size=16))


## ----sims, include=TRUE,echo=TRUE---------------------------------------------
set.seed(2)
n <- 400
x1 <- rnorm(n)
x2 <- rnorm(n)
y_val <- 1 + 2*cos(pi*x1) + 2/(1+exp(-5*(x2)))
y_norm <- y_val + rnorm(n, 0, 0.5)
y_negbinom <- rnbinom(n, mu = exp(y_val),size=10)
y_binom <- rbinom(n,1,prob = exp(y_val)/(1+exp(y_val)))


## ----sims_plot,fig.width = 11, fig.height = 5.5, echo = FALSE-----------------
p1 <- ggplot(data.frame(x = x1, y = y_norm),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x1", title = "Gaussian")

p2 <- ggplot(data.frame(x = x2, y = y_norm),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x2", title = "Gaussian")

p3 <- ggplot(data.frame(x = x1, y = y_negbinom),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x1", title = "Negative binomial")

p4 <- ggplot(data.frame(x = x2, y = y_negbinom),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x2", title = "Negative binomial")

p5 <- ggplot(data.frame(x = x1, y = y_binom),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x1", title = "Binomial")

p6 <- ggplot(data.frame(x = x2, y = y_binom),
             aes(x = x, y = y)) +
    geom_point() + labs(x = "x2", title = "Binomial")

#plot_grid(p1, p3, p5, p2, p4, p6, ncol = 3, align = 'hv', axis = 'lrtb')
wrap_plots(p1, p3, p5, p2, p4, p6, ncol = 3)


## ----gam_check_norm1, fig.keep="none", include=TRUE,echo=TRUE, fig.width=11, fig.height = 5.5, fig.align="center"----
norm_model_1 <- gam(y_norm ~ s(x1, k = 4) + s(x2, k = 4), method = 'REML')
gam.check(norm_model_1)


## ----gam_check_norm2, fig.keep="none", include=TRUE, echo=TRUE, fig.width=15, fig.height = 5.5,fig.align="center"----
norm_model_2 <- gam(y_norm ~ s(x1, k = 12) + s(x2, k = 4), method = 'REML')
gam.check(norm_model_2)


## ----gam_check_norm3, fig.keep="none", include=TRUE, echo=TRUE----------------
norm_model_3 <- gam(y_norm ~ s(x1, k = 12) + s(x2, k = 12),method = 'REML')
gam.check(norm_model_3)


## ----gam_check_norm4, echo = FALSE--------------------------------------------
thm <- theme_bw(base_size = 12, base_family = 'Fira Sans')
p1 <- draw(norm_model_1) & thm
p2 <- draw(norm_model_2) & thm
p3 <- draw(norm_model_3) & thm

## plot_grid(p1, p2, p3, nrow = 3, align = 'hv', axis = 'lrtb')
wrap_plots(p1, p2, p3, nrow = 3)


## ----alt-basis-dim-check-1----------------------------------------------------
norm_model_1 <- gam(y_norm ~ s(x1, k = 4) + s(x2, k = 4), method = "REML")
k.check(norm_model_1)


## ----alt-basis-dim-check-2----------------------------------------------------
res <- resid(norm_model_1, type = "deviance")

res_model <- gam(res ~ s(x1, k = 12) + s(x2, k = 12),
  method = "REML",
  family = quasi(link = "identity", variance = "constant"))
edf(res_model)


## ----alt-basis-dim-check-3, fig.align = "center", out.width = "95%"-----------
draw(res_model)


## ----gam_check_plots1, include=TRUE, echo=TRUE, results="hide", out.width = "90%", fig.align = "center"----
norm_model <- gam(y_norm ~ s(x1, k=12) + s(x2, k=12), method="REML")
gam.check(norm_model, rep = 500)


## ----gam_check_plots2, include=T, echo=TRUE, results="hide", out.width = "90%", fig.align = "center"----
pois_model <- gam(y_negbinom ~ s(x1, k=12) + s(x2, k=12), family=poisson, method="REML")
gam.check(pois_model, rep = 500)


## ----gam_check_plots3, include=T,echo=TRUE, results="hide", out.width = "90%", fig.align = "center"----
negbin_model <- gam(y_negbinom ~ s(x1, k=12) + s(x2, k=12), family = nb, method="REML")
gam.check(negbin_model, rep = 500)


## ----appraise-gam-check-example, fig.height = 5.5-----------------------------
appraise(negbin_model, method = 'simulate')


## ----cross-validated, echo = FALSE--------------------------------------------
knitr::include_graphics("resources/cross-validated.png")


## ----load-galveston-----------------------------------------------------------
galveston <- read_csv("https://bit.ly/gam-galveston") %>%
    mutate(datetime = as.POSIXct(paste(DATE, TIME),
                                 format = '%m/%d/%y %H:%M', tz = "CDT"),
           STATION_ID = factor(STATION_ID),
           DoY = as.numeric(format(datetime, format = '%j')),
           ToD = as.numeric(format(datetime, format = '%H')) +
               (as.numeric(format(datetime, format = '%M')) / 60))
galveston


## ----galveston-full-model-----------------------------------------------------
knots <- list(DoY = c(0.5, 366.5))
m <- bam(MEASUREMENT ~
             s(ToD, k = 10) +
             s(DoY, k = 12, bs = "cc") +
             s(YEAR, k = 30) +
             s(LONGITUDE, LATITUDE, k = 100, bs = "ds", m = c(1, 0.5)) +
             ti(DoY, YEAR, bs = c("cc", "tp"), k = c(12, 15)) +
             ti(LONGITUDE, LATITUDE, ToD, d = c(2,1), bs = c("ds", "tp"),
                m = list(c(1, 0.5), NA), k = c(20, 10)) +
             ti(LONGITUDE, LATITUDE, DoY, d = c(2,1), bs = c("ds", "cc"),
                m = list(c(1, 0.5), NA), k = c(25, 12)) +
             ti(LONGITUDE, LATITUDE, YEAR, d = c(2,1), bs = c("ds", "tp"),
                m = list(c(1, 0.5), NA), k = c(25, 15)),
         data = galveston, method = "fREML", knots = knots,
         nthreads = c(6, 1), discrete = FALSE)


## ----galveston-simple-model---------------------------------------------------
m.sub <- bam(MEASUREMENT ~
             s(ToD, k = 10) +
             s(DoY, k = 12, bs = "cc") +
             s(YEAR, k = 30) +
             s(LONGITUDE, LATITUDE, k = 100, bs = "ds", m = c(1, 0.5)) +
             ti(DoY, YEAR, bs = c("cc", "tp"), k = c(12, 15)),
         data = galveston, method = "fREML", knots = knots,
         nthreads = c(4, 1), discrete = TRUE)


## ----galveston-compare-models-aic---------------------------------------------
AIC(m, m.sub)


## ----galveston-compare-models-anova-------------------------------------------
anova(m, m.sub, test = "F")


## ----galveston-full-model-summary---------------------------------------------
summary(m)


## ----galveston-full-model-plot, fig.height = 5.5------------------------------
plot(m, pages = 1, scheme = 2, shade = TRUE)


## ----galveston-full-model-draw, fig.height = 14, fig.width = 1.777777*14, fig.align = "center", out.width = "90%"----
draw(m, scales = "free", rug = FALSE, n = 50) +  plot_layout(widths = 1) &
  theme(strip.text.x = element_text(size = 8))


## ----galveston-full-predict---------------------------------------------------
pdata <- data_slice(m, ToD = 12, DoY = 180,
                    YEAR = evenly(YEAR, by = 1),
                    LONGITUDE = evenly(LONGITUDE, n = 50),
                    LATITUDE  = evenly(LATITUDE, n = 50))
fv <- fitted_values(m, data = pdata)
# set fitted values to NA for grid points that are too far from the data
ind <- too_far(pdata$LONGITUDE, pdata$LATITUDE,
               galveston$LONGITUDE, galveston$LATITUDE, dist = 0.1)
fv <- fv %>%
  mutate(fitted = if_else(ind, NA_real_, fitted))


## ----galveston-full-predict-plot, fig.show = 'hide', fig.height = 10, fig.width = 1.777777*10----
plt <- ggplot(fv, aes(x = LONGITUDE, y = LATITUDE)) +
    geom_raster(aes(fill = fitted)) + facet_wrap(~ YEAR, ncol = 12) +
    scale_fill_viridis(name = expression(degree*C), option = "plasma",
      na.value = "transparent") +
    coord_quickmap() +
    scale_x_continuous(guide = guide_axis(n.dodge = 2,
                                          check.overlap = TRUE)) +
    theme(legend.position = "top")
plt


## ----galveston-full-predict-plot, echo = FALSE, fig.height = 10, fig.width = 1.777777*10----
plt <- ggplot(fv, aes(x = LONGITUDE, y = LATITUDE)) +
    geom_raster(aes(fill = fitted)) + facet_wrap(~ YEAR, ncol = 12) +
    scale_fill_viridis(name = expression(degree*C), option = "plasma",
      na.value = "transparent") +
    coord_quickmap() +
    scale_x_continuous(guide = guide_axis(n.dodge = 2,
                                          check.overlap = TRUE)) +
    theme(legend.position = "top")
plt


## ----galveston-animation, echo = FALSE, results = 'hide'----------------------
p <- ggplot(fv, aes(x = LONGITUDE, y = LATITUDE, frame = YEAR)) +
    geom_raster(aes(fill = fitted)) +
    scale_fill_viridis(name = expression(degree*C), option = "plasma",
                       na.value = "transparent") +
    coord_quickmap() +
    theme(legend.position = "right") +
    labs(x = "Longitude", y = "Latitude")

anim <- p + transition_time(YEAR) +
    ggtitle("Year {round(frame_time, 0)}")

anim <- animate(anim,
                nframes = 200, height = anim_height, width = anim_width,
                res = 100, dev = anim_dev)

anim_save('./resources/galveston-animation.gif', anim)


## ----galveston-trends-by-month, fig.show = "hide"-----------------------------
ds <- data_slice(m, ToD = 12, DoY = c(1, 90, 180, 270),
  YEAR = evenly(YEAR, n = 250),
  LONGITUDE = -94.8751, LATITUDE  = 29.50866)
fv <- fitted_values(m, data = ds, scale = "response")

plt2 <- ggplot(fv, aes(x = YEAR, y = fitted, group = factor(DoY))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.2) +
    geom_line() + facet_wrap(~ DoY, scales = "free_y") +
    labs(x = NULL, y = expression(Temperature ~ (degree * C)))
plt2


## ----galveston-trends-by-month, echo = FALSE----------------------------------
ds <- data_slice(m, ToD = 12, DoY = c(1, 90, 180, 270),
  YEAR = evenly(YEAR, n = 250),
  LONGITUDE = -94.8751, LATITUDE  = 29.50866)
fv <- fitted_values(m, data = ds, scale = "response")

plt2 <- ggplot(fv, aes(x = YEAR, y = fitted, group = factor(DoY))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.2) +
    geom_line() + facet_wrap(~ DoY, scales = "free_y") +
    labs(x = NULL, y = expression(Temperature ~ (degree * C)))
plt2


## ----setup-rat-hormone-example, echo = FALSE----------------------------------
rats_url <- "https://bit.ly/rat-hormone"
rats <- read_table(rats_url, col_types = "dddddddddddd-")
# ignore the warning - it"s due to trailing white space at the ends of each
#   row in the file

rats <- rats %>%
    mutate(treatment = fct_recode(factor(group, levels = c(1, 2, 3)),
                                  Low = "1",
                                  High = "2",
                                  Control = "3"),
           treatment = fct_relevel(treatment, c("Control", "Low", "High")),
           subject = factor(subject))

plt_labs <- labs(y = "Head height (distance in pixels)",
                 x = "Age in days",
                 colour = "Treatment")

rat_plt <- ggplot(rats, aes(x = time, y = response,
                            group = subject, colour = treatment)) +
    geom_line() +
    facet_wrap(~ treatment, ncol = 3) +
    plt_labs


## ----plot-rat-data, echo = FALSE----------------------------------------------
rat_plt


## ----obs-per-rat, echo = FALSE------------------------------------------------
rats %>%
    na.omit() %>%
    count(subject) %>%
    count(n, name = "n_rats")


## ---- fig.align = "center", out.width = "95%", echo = FALSE-------------------
knitr::include_graphics("resources/lawton-et-al-hgam-locust-paper-fig.svg")

