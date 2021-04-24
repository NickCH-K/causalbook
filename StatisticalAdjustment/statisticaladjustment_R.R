{
library(tidyverse)
library(vtable)
library(cowplot)
library(Cairo)
library(extrafont)
library(modelsummary)
library(Cairo)
library(lubridate)
library(ggpubr)
library(jtools)
}

# Residual example
set.seed(2897)

tb <- tibble(x = runif(30)) %>%
  mutate(y = .6*x + rnorm(30)) %>%
  mutate(yreal = .6*x) %>%
  arrange(x)

ggplot(tb, aes(x = x,
               y = y)) + 
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_line(aes(y = yreal)) + 
  geom_segment(aes(x = x, xend = x, y = y, yend = predict(lm(y~x, data = tb))[20]), data = tb[20,],
               size = 1, linetype = 'dashed') +
  geom_segment(aes(x = x, xend = x, y = y, yend = .6*x), data = tb[22,],
               size = 1, linetype = 'dashed') +
  scale_x_continuous(limits = c(0, 1)) +
  annotate(geom = 'text', family = 'Garamond', size = 15/.pt,
           label = 'Residual',
           x = .62, y = 0) +
  annotate(geom = 'text', family = 'Garamond', size = 15/.pt,
           label = 'Error',
           x = .81, y = -.08) +
  annotate(geom = 'text', family = 'Garamond', size = 15/.pt,
           label = 'OLS',
           x = .95, y = .83) +
  annotate(geom = 'text', family = 'Garamond', size = 15/.pt,
           label = 'True\nModel',
           x = .95, y = .47) +
  theme_pubr() +
  labs(x = "X",
       y = "Y")+
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('residual.pdf', width = 6, height = 5,device=cairo_pdf)


# GDP graph
data("economics")
ggplot(economics %>% filter(date <= as.Date('2008-01-01')), aes(x = date, y = unemploy/pop)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  annotate(geom = 'text', x = as.Date('1970-01-01'), y = .03, label = 'OLS Fit', family = 'Garamond', size = 13/.pt) +
  theme_pubr() + 
  expand_limits(x = as.Date('2010-02-01')) +
  labs(x = 'Month',
       y = 'US Unemployment Rate') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('unemployment.pdf', width = 6, height = 5,device=cairo_pdf)

# Instagram
set.seed(5403)
ig <- tibble(`Hours per Day` = runif(100, min = 0, max = 10)) %>%
  mutate(`Followers` = runif(100, min = 0, max = 3*`Hours per Day`))
ggplot(ig, aes(x = `Hours per Day`, y = `Followers`)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  annotate(geom = 'text', x = 2.5, y = 5.5, label = 'OLS Fit', family = 'Garamond', size = 13/.pt) +
  theme_pubr() + 
  labs(x = 'Hours per Day on Instagram',
       y = 'Followers (Thousands)') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('instagram.pdf', width = 6, height = 5,device=cairo_pdf)

# Simulated model
set.seed(31564)
tb <- tibble(X = rnorm(200)) %>%
  mutate(Y = 3 + .2*X + rnorm(200))
summary(lm(Y~X, data = tb))
est <- coef(lm(Y~X,data = tb))[2] %>% unname()
se <- summary(lm(Y~X,data = tb))$coefficients[2,2]

pnorm(est, sd = se)

densdata <- tibble(x = -1000:1000/2000) %>%
  mutate(y = dnorm(x, sd = se))

ggplot() + 
  geom_ribbon(data=densdata %>% filter(x <= -est),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_ribbon(data=densdata %>% filter(x >= est),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_vline(aes(xintercept = c(-est, est)), linetype = 'dashed') +
  geom_line(data=densdata, mapping=aes(x = x, y = y)) +
  scale_x_continuous(breaks = c(-.4, -est,  0,  est, .4),
                     labels = function(x) scales::number(x, accuracy = .001)) +
  labs(x = 'X',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave('estimate_vs_dist.pdf', width = 6, height = 5,device=cairo_pdf)

ests <- c()

set.seed(31564)
for (i in 1:1000) {
  tb <- tibble(X = rnorm(200)) %>%
    mutate(Y = 3 + .2*X + rnorm(200))
  summary(lm(Y~X, data = tb))
  ests[i] <- coef(lm(Y~X,data = tb))[2] %>% unname()
}

tb <- tibble(x = density(ests)$x,
             y = density(ests)$y)
rejl <- .2 - 1.96*se
rejr <- .2 + 1.96*se
p1 <- ggplot() + 
  geom_ribbon(data=tb %>% filter(x <= rejl),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_ribbon(data=tb %>% filter(x >= rejr),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_line(data=tb, mapping=aes(x = x, y = y)) +
  geom_vline(aes(xintercept = .2), linetype = 'dashed') + 
  annotate(geom = 'label', hjust = .5, x = .2, y = 2, label = 'True\nNull\nValue', family = 'Garamond', size = 13/.pt) +
  annotate(geom = 'text', hjust = .5, x = -.02, y = 1.25, label = 'Estimates\nRejecting\nthe Null', family = 'Garamond', size = 13/.pt) +
  annotate(geom = 'text', hjust = .5, x = .43, y = 1.25, label = 'Estimates\nRejecting\nthe Null', family = 'Garamond', size = 13/.pt) +
  labs(x = 'X',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

rejl <- 0 - 1.96*se
rejr <- 0 + 1.96*se
p2 <- ggplot() + 
  geom_ribbon(data=tb %>% filter(x >= rejr),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_line(data=tb, mapping=aes(x = x, y = y)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  annotate(geom = 'label', hjust = .5, x = 0, y = 2, label = 'False\nNull\nValue', family = 'Garamond', size = 13/.pt) +
  annotate(geom = 'text', hjust = .5, x = .4, y = 1.3, label = 'Estimates\nRejecting\nthe Null', family = 'Garamond', size = 13/.pt) +
  labs(x = 'X',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(p1,p2)
ggsave('true_false_null.pdf', width = 8, height = 4,device=cairo_pdf)

# Regression tables

res <- read_csv('restaurant_inspections.csv')

#table 1 regressions
tab1 <- res %>%
  group_by(business_name) %>%
  mutate(NumberofLocations = n()) %>%
  mutate(Year = year(mdy(inspection_date))) %>%
  select(inspection_score, Year, NumberofLocations) %>%
  filter(!is.na(inspection_score)) %>%
  rename(`Inspection Score` = inspection_score,
    `Number of Locations` = NumberofLocations,
    `Year of Inspection` = Year)

st(tab1,
   out = 'latex', file = 'restaurant_summary.tex',
   anchor = 'tab:statisticaladjustment-restaurantsumm',
   title = 'Summary Statistics for Restaurant Inspection Data')

m1 <- lm(`Inspection Score`~`Number of Locations`, data = tab1)
m2 <- lm(`Inspection Score`~`Number of Locations` + `Year of Inspection`, data = tab1)
knitr::opts_current$set(label = 'statisticaladjustment-restaurantregs')
msummary(list('Inspection Score' = m1,'Inspection Score' = m2),
         stars = TRUE,
         gof_omit = 'AIC|BIC|Lik',
         output = 'restaurant_regs.tex')
knitr::opts_current$set(label = 'statisticaladjustment-restaurantregs2')
msummary(list('Inspection Score' = m1,'Inspection Score' = m2),
         stars = TRUE,
         gof_omit = 'AIC|BIC|Lik',
         output = 'restaurant_regs2.tex')


# Polynomial example

set.seed(1000)
tb <- tibble(X = runif(100)) %>%
  mutate(Y = X + 4*X^2 - 5*X^3 + .3*rnorm(100))

ggplot(tb, aes(x = X, y = Y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', color = 'black', se = FALSE) + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = 'black', se = FALSE, linetype = 'dashed') +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2) + I(x^3), color = 'black', se = FALSE) + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())
ggsave('linear_square.pdf', width = 6, height = 4,device=cairo_pdf)

ggplot(tb, aes(x = X, y = Y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', color = 'black', se = FALSE) + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = 'black', se = FALSE, linetype = 'dashed') +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2) + I(x^3), color = 'black', se = FALSE) + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, degree = 10), color = 'black', se = FALSE) + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())
ggsave('ten_degree.pdf', width = 6, height = 4,device=cairo_pdf)

p1 <- ggplot(tb, aes(x = X, y = resid(lm(Y~X, data = tb)))) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')+ 
  scale_y_continuous(limits = c(-.75,.75)) +
  labs(y = 'Residual',
       title = 'Residuals from Linear OLS') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(tb, aes(x = X, y = resid(lm(Y~X + I(X^2), data = tb)))) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')+ 
  scale_y_continuous(limits = c(-.75,.75)) +
  labs(y = 'Residual',
       title = 'Residuals from Two-Order Polynomial') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(p1,p2)
ggsave('linear_sq_resid.pdf', width = 8, height = 4,device=cairo_pdf)

# Polynomial regression
res <- read_csv('restaurant_inspections.csv')

#table 1 regressions
tab1 <- res %>%
  group_by(business_name) %>%
  mutate(NumberofLocations = n()) %>%
  mutate(Year = year(mdy(inspection_date))) %>%
  select(inspection_score, Year, NumberofLocations) %>%
  filter(!is.na(inspection_score)) %>%
  rename(`Inspection Score` = inspection_score,
         `Number of Locations` = NumberofLocations,
         `Year of Inspection` = Year)


m1 <- lm(`Inspection Score`~`Number of Locations` + I(`Number of Locations`^2), data = tab1)

knitr::opts_current$set(label = 'statisticaladjustment-restaurantpoly')
msummary(m1, output = 'restaurant_polynomial.tex',
          coef_map = c('(Intercept)' = 'Constant',
                        '`Number of Locations`' = 'Number of Locations',
                       'I(`Number of Locations`^2)' = 'Number of Locations Squared'),
          caption = 'A Regression with a Second-Order Polynomial using Restaurant Inspection Data',
         stars = TRUE,
         gof_omit = 'AIC|BIC|Lik')


# Outliers

set.seed(1000)
df <- data.frame(X = c(runif(30), 6)) %>%
  mutate(Y = 1.5*X + 2 + .5*rnorm(31))
df[31,2] <- df[31,2] +4

lin1 <- lm(Y~X, data = df[1:30,])
lin2 <- lm(Y~X, data = df)
log1 <- lm(log(Y)~log(X), dat = df[1:30,])
log2 <- lm(log(Y)~log(X), dat = df)

p1 <- ggplot(df, aes(x = X, y = Y)) + 
  geom_point() + 
  geom_smooth(color = 'black', method = 'lm', se = FALSE) + 
  geom_line(aes(x = X, y = predict(lin1, newdata = df)), color = 'black', linetype = 'dashed') + 
  annotate(geom = 'text', label = 'OLS fit\nwithout outlier', hjust = .5, x = 5, y = 5, size = 13/.pt, family = 'Garamond') +
  annotate(geom = 'text', label = 'OLS fit\nwith outlier', hjust = .5, x = 4, y = 14, size = 13/.pt, family = 'Garamond') +
  labs(title = 'Linear Regression Without Transformation') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(df, aes(x = log(X), y = log(Y))) + 
  geom_point() + 
  geom_smooth(color = 'black', method = 'lm', se = FALSE) + 
  geom_line(aes(x = log(X), y = predict(log1, newdata = df)), color = 'black', linetype = 'dashed') + 
  annotate(geom = 'text', label = 'OLS fit\nwithout outlier', hjust = .5, x = 1, y = 1, size = 13/.pt, family = 'Garamond') +
  annotate(geom = 'text', label = 'OLS fit\nwith outlier', hjust = .5, x = .25, y = 1.75, size = 13/.pt, family = 'Garamond') +
  labs(title = 'Linear Regression With Transformation') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(p1,p2)
ggsave('log_outliers.pdf', width = 8, height = 4,device=cairo_pdf)


# Interaction model

# Regression tables

res <- read_csv('restaurant_inspections.csv')

#interaction table regressions
inx <- res %>%
  group_by(business_name) %>%
  mutate(NumberofLocations = n(),
         Weekend = lubridate::wday(lubridate::mdy(inspection_date)) %in% c(1,7)) %>%
  mutate(Year = year(mdy(inspection_date))) %>%
  select(inspection_score, Year, NumberofLocations, Weekend) %>%
  filter(!is.na(inspection_score)) %>%
  rename(`Inspection Score` = inspection_score,
         `Number of Locations` = NumberofLocations,
         `Year of Inspection` = Year)

m1 <- lm(`Inspection Score`~`Number of Locations` + `Year of Inspection` + Weekend, data = inx)
m2 <- lm(`Inspection Score`~`Number of Locations` + `Year of Inspection` + Weekend + `Number of Locations`:`Year of Inspection`, data = inx)
m3 <- lm(`Inspection Score`~`Number of Locations` + `Year of Inspection` + Weekend + `Number of Locations`:Weekend, data = inx)

knitr::opts_current$set(label = 'statisticaladjustment-restaurantregsinx')
msummary(list('Inspection Score' = m1,'Inspection Score' = m2, 'Inspection Score' = m3),
         coef_map = c('`Number of Locations`' = 'Number of Locations',
                      'WeekendTRUE' = 'Weekend',
                      '`Year of Inspection`' = 'Year of Inspection',
                      '`Number of Locations`:`Year of Inspection`' = 'Number of Locations x Year of Inspection',
                      '`Number of Locations`:WeekendTRUE' = 'Number of Locations x Weekend'),
         stars = TRUE,
         gof_omit = 'AIC|BIC|Lik',
         output = 'restaurant_regs_inx.tex')


# For code examples
res %>%
  group_by(business_name) %>%
  mutate(NumberofLocations = n(),
         Weekend = lubridate::wday(lubridate::mdy(inspection_date)) %in% c(1,7)) %>%
  mutate(Year = year(mdy(inspection_date))) %>%
  select(inspection_score, Year, NumberofLocations, Weekend) %>%
  filter(!is.na(inspection_score)) %>%
  write_csv('restaurant_data.csv')


## Probit/logit graph

set.seed(200)
tib <- tibble(X = runif(250)*8) %>%
  mutate(Y = ((X - 4)/4 + rnorm(250)*.5 > 0)*1)

ggplot(tib, aes(x = X, y = Y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', linetype =) + 
  geom_smooth(method = 'glm', method.args = list(family = binomial(link = 'logit')), 
              se = FALSE, color = 'black') +
  theme_pubr() + 
  scale_y_continuous(breaks = c(0,1)) + 
  annotate(geom = 'text', x = 1.75, y = .35, color = 'black', label = 'OLS Prediction',
           size = 13/.pt, family = 'Garamond') +
  annotate(geom = 'text', x = 3.75, y = .35, color = 'black', label = 'Logit',
           size = 13/.pt, family = 'Garamond') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))

ggsave('ols_logit.pdf', width = 6, height = 5,device=cairo_pdf)


# Heteroskedasticity demonstratoin
set.seed(200)
tibA <- tibble(X = c(runif(10),runif(10, 2, 3))) %>%
  mutate(Y = c(rnorm(10, 0, 3),rnorm(10)))
set.seed(200)
tibB <- tibble(X = c(runif(10),runif(10, 2, 3))) %>%
  mutate(Y = rnorm(20,0,sd(tibA$Y)))

p1 <- ggplot(tibA, aes(x = X, y = Y)) + 
  geom_point() + 
  theme_pubr()+
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
p2 <- ggplot(tibB, aes(x = X, y = Y)) + 
  geom_point() + 
  theme_pubr()+
  scale_y_continuous(limits = c(min(tibA$Y),max(tibA$Y))) +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
plot_grid(p1,p2)
ggsave('heterosked.pdf', width = 8, height = 4,device=cairo_pdf)
