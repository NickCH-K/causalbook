{
  library(tidyverse)
  library(extrafont)
  library(ggpubr)
  library(gghighlight)
  library(cowplot)
  library(modelsummary)
  library(rdrobust)
  library(fixest)
  library(broom)
  library(ggtext)
}

# Animationggsave('rdd_poly.pdf',width = 7, height = 5, units = 'in', device = cairo_pdf)
set.seed(1000)
tb <- tibble(x = runif(100)) %>%
  mutate(y = x + 2*sqrt(x) + 2*(x>.5) + rnorm(100)) %>%
  group_by(x > .5) %>%
  mutate(pred = predict(loess(y~x))) %>%
  ungroup()

p1 <- ggplot(tb, aes(x= x, y = y)) +
  geom_point() +
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(a) Raw Data') +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text = element_blank(),
        axis.ticks = element_blank())

p2 <- ggplot(tb, aes(x= x, y = y, group = x > .5)) +
  geom_point() +
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  geom_smooth(aes(x = x, y = pred), se = FALSE, color = 'black', size = 1.5) +
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(b) Predict Values Near the Cutoff') +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text = element_blank(),
        axis.ticks = element_blank())

p3 <- ggplot(tb, aes(x= x, y = y, group = x > .5)) +
  geom_point() +
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  geom_vline(aes(xintercept = .55), linetype = 'dashed') +
  geom_vline(aes(xintercept = .45), linetype = 'dashed') +
  gghighlight(abs(x - .5) < .05) +
  geom_smooth(aes(x = x, y = pred), se = FALSE, color = 'black', size = 1.5) +
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(c) Pick a Bandwidth') +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text = element_blank(),
        axis.ticks = element_blank())

p4 <- ggplot(tb, aes(x= x, y = y, group = x > .5)) +
  geom_point() +
  annotate(geom = 'segment', x = .5, xend = .5,
           y = tb %>% dplyr::filter(x <= .5) %>% dplyr::filter(x == max(x)) %>% pull(pred),
           yend = tb %>% dplyr::filter(x > .5 & x <= .55) %>% pull(pred) %>% mean(),
           size = 1.5, linetype = 'solid') +
  gghighlight(abs(x - .5) < .05) +
  geom_smooth(aes(x = x, y = pred), se = FALSE, color = 'black', size = 1.5) +
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(d) Estimate Jump at the Cutoff') +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text = element_blank(),
        axis.ticks = element_blank())


plot_grid(p1,p2,p3,p4, nrow = 2)
ggsave('rdd_anim.pdf',width = 7, height = 5, units = 'in', device = cairo_pdf)

# Treatment rates in sharp and fuzzy
tib <- tibble(x = c(0,.5,.5,1),
              y = c(0,0,1,1))
p1 <- ggplot(tib, aes(x = x, y = y)) + geom_path() +
  labs(x = 'Running Variable', y ='Proportion Treated',
       title = '(a) Sharp Design') +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_x_continuous(breaks = .5, labels = 'Cutoff') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))

tib2 <- tibble(x = (-2500:0)/2500) %>%
  mutate(treatrate = (x+x^2+x^3)/2 + .5 + .5*(x > -.5))

p2 <- ggplot(tib2, aes(x = x, y = treatrate)) + geom_line()+
  labs(x = 'Running Variable', y ='Proportion Treated',
       title = '(b) Fuzzy Design') +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_x_continuous(breaks = -.5, labels = 'Cutoff') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))

plot_grid(p1,p2)

ggsave('treatment_share.pdf',width = 7, height = 3.5, units = 'in', device = cairo_pdf)

tib2 <- tibble(x = (-2500:0)/2500) %>%
  mutate(treatrate = (x+x^2+x^3)/2 + .5 + .5*(x > -.5))

ggplot(tib2, aes(x = x, y = treatrate)) + geom_line()+
  labs(x = 'Running Variable', y ='Proportion Treated') +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_x_continuous(breaks = -.5, labels = 'Cutoff') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
ggsave('fuzzy_treat.pdf', width = 6, height = 5, units = 'in', device = cairo_pdf)

## Fuzzy RD plots for Fetter
vet <- read_csv('fetter_mortgages.csv')

vet <- vet %>%
  dplyr::filter(abs(qob_minus_kw) < 12) %>%
  group_by(qob_minus_kw) %>%
  summarize(mean_treat = mean(vet_wwko), 
            mean_out = mean(home_ownership))

p1 <- ggplot(vet, aes(x = qob_minus_kw, y = mean_out, group = qob_minus_kw > 0)) + 
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = 'Birth Months Relative to Eligibility',
       y = 'Home Ownership Rates',
       title = '(b) RD for Home Ownership',
       caption = 'Copyright American Economic Association. Reproduced with\npermission of the American Economic Journal: Economic Policy.') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        plot.caption = element_text(size = 7.5, hjust = 0))
p2 <- ggplot(vet, aes(x = qob_minus_kw, y = mean_treat, group = qob_minus_kw > 0)) + 
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = 'Birth Months Relative to Eligibility',
       y = 'Eligible Veteran Rates',
       title = '(a) RD for Eligible Vet Status',
       caption = ' \n ') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        plot.caption = element_text(size = 7.5, hjust = 0))

plot_grid(p2,p1)
ggsave('fetter.pdf',width = 7, height = 3.5, units = 'in', device = cairo_pdf)

# Heaping
prbs <- rep(1,100)
prbs[1:100 %% 10 == 0] <- 4
prbs <- prbs/sum(prbs)
set.seed(1000)
tib <- tibble(X = sample(1:100, 5000, replace = TRUE, prob = prbs)) %>%
  mutate(Y = .2*X + 10*(X>55) + 3*(X %% 10 == 0) + rnorm(5000)*3) %>%
  group_by(X) %>%
  summarize(N = n(),
            Y = mean(Y))
p1 <- ggplot(tib, aes(x = X, y = N)) + geom_col() + 
  geom_vline(aes(xintercept = 55), linetype = 'dashed') +
  labs(x = 'Running Variable',
       y = 'Density',
       title = '(a) Running Variable Distribution')+
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p2 <- ggplot(tib, aes(x = X, y = Y, shape = X %% 10 == 0)) + geom_point() + 
  geom_vline(aes(xintercept = 55), linetype = 'dashed') +
  scale_shape_manual(values = c(16,1)) +
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(b) Standard RD Plot')+
  guides(shape = FALSE) +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank())
plot_grid(p1,p2)
ggsave('heaping.pdf',width = 7, height = 3.5, units = 'in', device = cairo_pdf)

# Verying bandwidth
vet <- read_csv('fetter_mortgages.csv') %>%
  mutate(above = qob_minus_kw > 0)

res <- tibble(bw = NA,b = NA, se = NA)
for (b in 4:16) {
  print(b)
  m <- feols(home_ownership ~
               nonwhite  | # Control for race
               bpl + qob | # fixed effect controls
               qob_minus_kw*vet_wwko ~ # Instrument our standard RDD
               qob_minus_kw*above, # with being above the cutoff
             se = 'hetero', # heteroskedasticity-robust SEs
             data = vet %>% dplyr::filter(abs(qob_minus_kw) < b)) 
  ms <- tidy(m)
  res <- res %>%
    add_row(bw = b,
            b = ms %>% dplyr::filter(term == 'fit_vet_wwko') %>% pull(estimate),
            se = ms %>% dplyr::filter(term == 'fit_vet_wwko') %>% pull(std.error))
}

ggplot(res, aes(x = bw, y = b)) + 
  geom_point(size = 2) + 
  geom_line() + 
  geom_errorbar(aes(x = bw, ymin = b - 1.96*se, ymax = b + 1.96*se), width = .2) + 
  scale_x_continuous(breaks = c(4,6,8,10,12,14,16)) + 
  labs(x = 'Bandwidth (Quarters of Birth)',
       y = 'Effect of Military Eligibility',
       caption = '95% confidence intervals shown') + 
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
ggsave('fetter_bandwidth.pdf', width = 6, height = 5, units = 'in', device = cairo_pdf)

# Battistin graph
bat <- read.csv(text = '
x, y
-10.04132231404959, 2.220446049250313e-16
-8.966942148760333, 0.046961325966850875
-8.016528925619838, 0.07182320441988965
-7.024793388429754, 0.016574585635359185
-5.991735537190085, 0.12154696132596698
-4.958677685950416, 0.046961325966850875
-3.9669421487603334, 0.18232044198895037
-2.9752066115702487, 0.17127071823204432
-2.1074380165289277, 0.19889502762430944
-1.0330578512396702, 0.21270718232044206
0, 0.39226519337016585
1.0330578512396684, 0.7458563535911604
2.0661157024793386, 0.6243093922651934
2.9752066115702487, 0.850828729281768
4.008264462809917, 0.8839779005524863
4.9586776859504145, 0.7292817679558012
5.950413223140496, 0.8674033149171272
7.024793388429755, 0.9337016574585636
7.975206611570249, 0.6795580110497238
8.966942148760333, 0.776243093922652
10.041322314049589, 0.5635359116022101
') %>%
  mutate(x = round(x))

element_custom <- function() {
  structure(list(), class = c("element_custom", "element_text"))
}

element_grob.element_custom <- function(element, label="", ...)  {
  disect <- strsplit(label, "\\n")[[1]]
  labels <- lapply(disect, function(x) tryCatch(parse(text=x), 
                                                error = function(e) x))
  hl <-  unit(rep(1, length(labels)), 'strheight', data=labels) + unit(0.1,"line")
  yl <- c(list(unit(0,"line")), 
          lapply(seq_along(labels[-length(labels)]), function(ii) sum(hl[1:ii])))
  
  cl <- do.call(gList, Map(function(label, ii) 
    textGrob(label, y = unit(1,"npc")-yl[[ii]], hjust=0, x=0, vjust=1), 
    label = labels, ii = seq_along(labels)))
  
  gTree(children = cl, cl="sl", heights = hl, gp=gpar(col="grey50",fontsize=8))
}

heightDetails.sl <- function(x) sum(x$heights)
ggplot(bat, aes(x = x, y = y)) +
  geom_line(size = 1.5) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_vline(aes(xintercept = -1), linetype = 'dashed') +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  scale_x_continuous(breaks = c(-10, -5, -2, -1, 0, 1, 2, 5, 10)) +
  labs(x = 'Years to Pension Eligibility',
       y = 'Proportion Retired',
       caption = "Copyright American Economic Association.
       Reproduced with permission of the *American Economic Review.*") +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        plot.caption = element_markdown())
ggsave('battistin.pdf', width = 6, height = 5,device=cairo_pdf)

# Basic linear RDD
set.seed(1000)
tib <- tibble(X = runif(200)) %>%
  mutate(Y = rnorm(200)*.05 + case_when(
    X < .5 ~ -2*X + 4*X^2,
    TRUE ~ .4-X + X^2
  ))
ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point() +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black')+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave('linear_rdd.pdf', width = 6, height = 5,device=cairo_pdf)

set.seed(1000)
tib <- tibble(X = runif(200)) %>%
  mutate(Y = rnorm(200)*.05 + case_when(
    X < .5 ~ -2*X + 4*X^2,
    TRUE ~ .4-X + X^2
  ))
ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point() +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', linetype = 'dashed') +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', data = tib %>% dplyr::filter(abs(X - .5) <= .1))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave('local_problems.pdf', width = 6, height = 5,device=cairo_pdf)



p1 <- ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point(alpha = .05) +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', formula = y ~ poly(x, 2))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(a) Order-2 Polynomial RDD') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p2 <- ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point(alpha = .05) +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', formula = y ~ poly(x, 6))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(b) Order-6 Polynomial RDD') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p3 <- ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point(alpha = .05) +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', formula = y ~ poly(x, 25))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(c) Order-25 Polynomial RDD') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p4 <- ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point(alpha = .05) +
  geom_smooth(size = 1, se = FALSE, method = 'lm', color = 'black', data = tib %>% dplyr::filter(abs(X - .5) <= .1))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome',
       title = '(d) Linear with Bandwidth') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
plot_grid(p1,p2,p3,p4, ncol = 2)
ggsave('rdd_poly.pdf',width = 7, height = 5, units = 'in', device = cairo_pdf)

tib <- tib %>%
  mutate(above = X > .5,
         X_c = X - .5)
lm(Y ~X_c*above, data = tib)

fgen <- function(n) {
  f <- 'Y~X_c*above'
  for (i in 2:n) {
    f <- paste0(f, '+I(X_c^',i,')*above')
  }
  return(as.formula(f))
}

lm(fgen(2), data = tib)
lm(fgen(6), data = tib)

lm(Y ~X_c*above, data = tib %>% dplyr::filter(abs(X_c) < .1))

## Kernel
tib <- tibble(x = c(0,.25,.5,.75,1),
              y = c(0,0,1,0,0))
ggplot(tib, aes(x = x, y = y)) + geom_path() +
  labs(x = 'X', y ='Weight') +
  scale_x_continuous(breaks = c(.25,.5,.75), labels = c('- Bandwidth','Central Value','+ Bandwidth')) +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
ggsave('kernel.pdf', width = 6, height = 3,device=cairo_pdf)

## Kernel RDD
set.seed(1000)
tib <- tibble(X = runif(200)) %>%
  mutate(Y = rnorm(200)*.05 + case_when(
    X < .5 ~ -2*X + 4*X^2,
    TRUE ~ .4-X + X^2
  ))

ggplot(tib, aes(x = X, y = Y, group = X > .5)) +
  geom_point(alpha = .15) +
  geom_line(aes(x = X, y = meanY, group = X > .5),size = 1, data = tib %>%
              dplyr::filter(abs(X - .5) <= .1) %>%
              group_by(X > .5) %>%
              mutate(meanY = mean(Y)))+
  geom_vline(aes(xintercept = .5), linetype = 'dashed') +
  scale_x_continuous(breaks = .5, labels = 'Cutoff')+
  labs(x = 'Running Variable',
       y = 'Outcome') +
  theme_pubr() +
  theme(text         = element_text(size = 14, family="Garamond"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave('kernreg.pdf', width = 6, height = 5,device=cairo_pdf)


# Binned scatterplot, and with controls

gt <- read_csv('Government_Transfers_RDD_Data.csv')

# Use cut() to create bins
# Using breaks to make sure it breaks at 0
# (-15:15)*.02/15 gives 15 breaks from -.02 to .02
binned <- gt %>%
  mutate(Inc_Bins = cut(Income_Centered,
                        breaks = (-15:15)*(.02/15))) %>%
  group_by(Inc_Bins) %>%
  summarize(Support = mean(Support),
            Income = mean(Income_Centered))
# Taking the mean of Income lets us plot data
# rouhgly at the bin midpoints

ggplot(binned, aes(x = Income, y = Support)) +
  geom_line() +
  # Add a cutoff line
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  labs(x = "Centered Income") +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
ggsave('binned_scatter.pdf', width = 6, height = 5,device=cairo_pdf)


# controls!
binnedC <- gt %>%
  mutate(Inc_Bins = cut(Income_Centered,
                        breaks = (-15:15)*(.02/15))) %>%
  group_by(Inc_Bins) %>%
  summarize(Age = mean(Age),
            Education = mean(Education, na.rm = TRUE),
            Income = mean(Income_Centered))
# Taking the mean of Income lets us plot data
# rouhgly at the bin midpoints

p1 <- ggplot(binnedC, aes(x = Income, y = Age)) +
  geom_line() +
  # Add a cutoff line
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  labs(x = "Centered Income",
       y = 'Mean Age in Household') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
p2 <- ggplot(binnedC, aes(x = Income, y = Education)) +
  geom_line() +
  # Add a cutoff line
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  labs(x = "Centered Income",
       y = 'Mean Education in Household') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))

plot_grid(p1,p2)
ggsave('placebo_controls.pdf', width = 7, height = 3.5,device=cairo_pdf)



## Results table for OLS

library(tidyverse); library(modelsummary)

gt <- read_csv('Government_Transfers_RDD_Data.csv')

# Linear term and a squared term with "treated" interactions
m <- lm(Support ~ Income_Centered*Participation +
          I(Income_Centered^2)*Participation, data = gt)

# Add a kernel weight
kweight <- function(x) {
  # To start at a weight of 0 at x = 0,
  # andimpose a bandwidth of .01, we need a "slope" of -1/.01 = 100
  # and to go in either direction use the absolute value
  w <- 1 - 100*abs(x)

  # if further away than .01, the weight is 0, not negative
  w <- ifelse(w < 0, 0, w)

  return(w)
}

# Run the same model but with the weight
mw <- lm(Support ~ Income_Centered*Participation, data = gt,
         weights = kweight(Income_Centered))

# See the results with heteroskedasticity-robust SEs
msummary(list('Quadratic' = m,
              'Linear with Kernel Weight' = mw),
         stars = TRUE,
         vcov = 'robust',
         caption = '\\label{tab:regressiondiscontinuity-rddols}The Effect of Government Payments on Support Regression Discontinuity Estimates',
         output = 'rdd_ols_estimates.tex',
         gof_omit = 'Adj|AIC|BIC|Lik|F')


# Estimate the discontinuity

## density test plot
library(tidyverse); library(rddensity); library(rdrobust)

gt <- read_csv('Government_Transfers_McCrary.csv') %>%
  dplyr::filter(abs(Income_Centered) <= .02) %>%
  mutate(inc_bins = cut(Income_Centered, breaks = .02*(-29:29)/29)) %>%
  group_by(inc_bins) %>%
  summarize(N = n())
ggplot(gt, aes(x = inc_bins, y = N)) + geom_col() +
  geom_vline(aes(xintercept = gt$inc_bins[30]), linetype = 'dashed') +
  scale_x_discrete(breaks = gt$inc_bins[c(1,30,58)],
                   labels = c(-.02,0,.02)) +
  theme_pubr() +
  labs(x = 'Centered Income (Binned)', y = 'Density') +
  theme(text = element_text(family = 'Garamond', size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave('density_test.pdf', width = 6, height = 5,device=cairo_pdf)

# regression kink
set.seed(1001)

tb <- tibble(run = runif(100)) %>%
  mutate(Y = (run-.6) - 1.75*(run-.6)*(run > .6) + rnorm(100)*.1) %>%
  add_row(run = .6, Y = 0) %>%
  add_row(run = .60001, Y = 0)

ggplot(tb, aes(x = run, y = Y , group = run > .6)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  geom_vline(aes(xintercept = .6), linetype = 'dashed') +
  theme_pubr() +
  labs(x = 'Running Variable', y = 'Outcome') +
  theme(text = element_text(family = 'Garamond', size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave('reg_kink.pdf', width = 6, height = 5,device=cairo_pdf)

## Bana et al first stage
# All of these are traced with web plot digitizer
tb <- read.csv(text = 'x,y
               -588.0912162162163, 6.80600865108502
-587.7635319745975, 6.789920031987204
-587.0387715343115, 6.756850137069355
-586.8768569678647, 6.740402071850998
-588.0912162162163, 6.730607837673011
-587.2501599960617, 6.710252821093785
-589.6968690001477, 6.701933633953826
-588.0912162162163, 6.675750366520059
-586.7149424014178, 6.846738422476127
-582.258436715404, 6.826021826190158
-582.5861209570227, 6.814349559839363
-582.7480355234695, 6.776157476878529
-581.2098471422242, 6.765758184997606
-582.2420525033231, 6.719766784644166
-582.5861209570227, 6.69394920819551
-582.5861209570227, 6.685867047120546
-581.2098471422242, 6.837895646455222
-576.6222677595629, 6.8084404682571416
-577.081025697829, 6.794231951663779
-576.6222677595629, 6.755477771854221
-573.640341160833, 6.7438850059976
-577.6708573327426, 6.734016482295971
-576.25526140895, 6.724847580967613
-576.5649230172796, 6.706937066839931
-577.081025697829, 6.676611488737838
-576.3928887904298, 6.846640543782486
-576.6222677595629, 6.784862647533579
-574.3284780682322, 6.825960993380425
-574.3284780682322, 6.77842263094762
-574.3284780682322, 6.7679188324670125
-568.4793143553389, 6.6941734139677465
-574.3284780682322, 6.684266959882714
-555.0606446610545, 6.905540099111869
-555.0606446610545, 6.892856396835205
-555.0606446610545, 6.805761755903698
-555.0606446610545, 6.793484097270182
-555.0606446610545, 6.605755439238446
-555.0606446610545, 6.593799231822423
-553.6843708462561, 6.70634128166915
-552.3080970314577, 6.693407243163341
-544.0504541426673, 6.905294637700474
-543.8538435976961, 6.893902229584356
-544.0504541426673, 6.8058460615753695
-543.8538435976961, 6.794541973686715
-544.0504541426673, 6.606410155937625
-543.8538435976961, 6.595102733509771
-543.362317235268, 6.702826036252166
-542.6741803278688, 6.693966413434626
-523.5210364052577, 6.573122262206229
-519.048146507163, 6.898841741081345
-519.048146507163, 6.800640688169176
-519.048146507163, 6.700148829357146
-519.048146507163, 6.599656970545116
-509.3260071231383, 6.573830522491858
-515.1487040319008, 6.569434892709583
-508.26733495790876, 6.9322804817466945
-514.4605671245016, 6.922826202852192
-514.4605671245016, 6.915539784086365
-514.4605671245016, 6.908253365320538
-514.4605671245016, 6.8863941090230565
-514.4605671245016, 6.8791076902572295
-514.4605671245016, 6.871821271491402
-514.4605671245016, 6.864534852725575
-514.4605671245016, 6.857248433959748
-514.4605671245016, 6.849962015193922
-514.4605671245016, 6.842675596428095
-514.4605671245016, 6.835389177662268
-514.4605671245016, 6.828102758896441
-514.4605671245016, 6.820816340130614
-514.4605671245016, 6.813529921364787
-514.4605671245016, 6.791670665067306
-514.4605671245016, 6.784384246301479
-514.4605671245016, 6.777097827535652
-514.4605671245016, 6.769811408769825
-514.4605671245016, 6.762524990003998
-514.4605671245016, 6.755238571238171
-514.4605671245016, 6.747952152472344
-514.4605671245016, 6.740665733706517
-514.4605671245016, 6.73337931494069
-514.4605671245016, 6.726092896174864
-514.4605671245016, 6.718806477409037
-514.4605671245016, 6.7115200586432096
-514.4605671245016, 6.6896608023457285
-514.4605671245016, 6.682374383579901
-514.4605671245016, 6.675087964814074
-514.4605671245016, 6.667801546048247
-514.4605671245016, 6.66051512728242
-514.4605671245016, 6.653228708516593
-514.4605671245016, 6.645942289750766
-514.4605671245016, 6.638655870984939
-514.4605671245016, 6.631369452219112
-514.4605671245016, 6.624083033453285
-514.4605671245016, 6.616796614687458
-514.4605671245016, 6.609510195921631
-508.26733495790876, 6.590902729817165
-495.26919337370157, 6.576647711285856
-496.4707022596367, 6.569908036785286
-493.12832299512627, 6.932762228441955
-493.12832299512627, 6.590360764784996
-490.37577536552953, 6.654813741170199
-480.95359309498656, 6.574825823516748
-477.98931103234383, 6.932762228441955
-477.98931103234383, 6.590962948154072
-476.38365824841236, 6.570594095695055
-478.33337948604344, 6.654967512994802
-471.0020747418289, 6.665636002009452
-462.8502990695614, 6.932762228441955
-462.8502990695614, 6.590962948154072
-464.72703608974103, 6.575294136890698
-465.6028466991582, 6.569434892709583
-458.59636182382087, 6.666908307384117
-450.46383473637576, 6.674914837095465
-447.71128710677897, 6.932762228441955
-447.71128710677897, 6.590962948154072
-440.8299180327869, 6.682972265639199
-432.5722751439965, 6.932762228441955
-432.5722751439965, 6.590962948154072
-431.1960013291981, 6.690873293780131
-417.433263181214, 6.932762228441955
-417.433263181214, 6.590962948154072
-415.64410722197607, 6.691532087165134
-402.29425121843155, 6.932762228441955
-402.29425121843155, 6.590962948154072
-399.84754221434554, 6.711170457742829
-405.7349357554276, 6.693966413434626
-387.1552392556491, 6.932762228441955
-387.1552392556491, 6.590962948154072
-385.7789654408507, 6.713896911823506
-372.0162272928667, 6.932762228441955
-372.0162272928667, 6.590962948154072
-371.7103886673559, 6.71825907599923
-359.7157800731059, 6.727084658729101
-356.8772153300842, 6.932762228441955
-356.8772153300842, 6.590962948154072
-348.0461250184611, 6.73130930960949
-341.73820336730176, 6.932762228441955
-341.5088243981687, 6.7414109356257494
-341.73820336730176, 6.590962948154072
-330.91151602422093, 6.747059136839091
-326.5991914045193, 6.932762228441955
-326.5991914045193, 6.590962948154072
-318.34154851572885, 6.746627349060375
-311.6220940081837, 6.75794662527146
-311.46017944173684, 6.932762228441955
-311.46017944173684, 6.590962948154072
-298.65024624245933, 6.76270332892484
-296.3211674789544, 6.932762228441955
-296.3211674789544, 6.590962948154072
-281.44020685644665, 6.772978516926562
-287.37538768276477, 6.763932593629215
-281.1821555161719, 6.932762228441955
-281.1821555161719, 6.590962948154072
-275.8887946900243, 6.57275029817133
-272.9245126273815, 6.554744629420959
-270.4013439669178, 6.775000222133369
-266.0431435533895, 6.932762228441955
-266.0431435533895, 6.590962948154072
-260.62979988184907, 6.786768892443022
-263.20457881036776, 6.573172336620908
-256.63860581893374, 6.553429742917648
-250.904131590607, 6.932762228441955
-250.904131590607, 6.590511310627265
-245.80382274753043, 6.792138242742118
-247.0046891153449, 6.57464946613947
-250.904131590607, 6.569633613221378
-241.05848045397232, 6.553158493867411
-235.76511962782456, 6.932762228441955
-235.76511962782456, 6.590962948154072
-231.96398242504802, 6.796402806778523
-232.85965268547238, 6.57367794363736
-225.49599962509802, 6.55327611263187
-220.62610766504213, 6.932762228441955
-220.62610766504213, 6.590962948154072
-220.5009918636968, 6.791640555898852
-217.18542312804607, 6.57367794363736
-212.36846477625159, 6.807189790750366
-217.87356003544528, 6.554601825936293
-205.48709570225964, 6.932762228441955
-205.48709570225964, 6.590962948154072
-197.65953838059374, 6.806781703985072
-198.5139750406144, 6.5530903934722415
-190.3480837394772, 6.932762228441955
-190.3480837394772, 6.590962948154072
-183.46671466548514, 6.818799025844207
-186.9073992024812, 6.807651106224177
-181.40230394328756, 6.553546545667448
-175.2090717766947, 6.932762228441955
-175.2090717766947, 6.590962948154072
-171.17200191995272, 6.827749477986583
-175.2090717766947, 6.818497934159669
-164.1988812583075, 6.5554283677475516
-160.07005981391228, 6.932762228441955
-160.07005981391228, 6.590962948154072
-156.62937527691628, 6.825370351859256
-148.37173238812585, 6.55352148029677
-144.93104785112985, 6.932762228441955
-144.93104785112985, 6.590962948154072
-143.55477403633142, 6.844632692377594
-147.68359548072664, 6.8269987560531336
-133.81499011621963, 6.8536731632133465
-131.260061290799, 6.5539738060331425
-129.79203588834736, 6.932762228441955
-129.79203588834736, 6.590962948154072
-126.00728289765175, 6.848637211781953
-120.84625609215772, 6.8535638244702115
-114.65302392556487, 6.932762228441955
-114.65302392556487, 6.590962948154072
-113.27675011076656, 6.861609245190812
-106.48713262442766, 6.553109529521525
-103.34791758972085, 6.8535477713676425
-99.51401196278243, 6.932762228441955
-99.51401196278243, 6.590962948154072
-91.56220769950278, 6.862458275948879
-90.5682321665928, 6.552643009462882
-95.38519051838728, 6.853605224576835
-84.375, 6.932762228441955
-84.375, 6.590962948154072
-77.493630926008, 6.869337265093962
-74.52934886336527, 6.553285150896906
-69.23598803721757, 6.932762228441955
-69.23598803721757, 6.590962948154072
-63.44115092227685, 6.876388356937926
-55.04978102314169, 6.555193256030922
-54.09697607443513, 6.932762228441955
-54.09697607443513, 6.590962948154072
-49.19400060921578, 6.878631589031054
-38.957964111652586, 6.932762228441955
-41.02237483385022, 6.875671481407436
-38.957964111652586, 6.590962948154072
-39.6461010190518, 6.553528802764609
-32.382433663171355, 6.894140528973595
-23.818952148870153, 6.932762228441955
-23.818952148870153, 6.590962948154072
-21.831001083050296, 6.555524456883914
-17.549260325899695, 6.897960378811438
-8.67994018608772, 6.932762228441955
-8.67994018608772, 6.590962948154072
-7.303666371289296, 6.554231196410325
-11.432487815684567, 6.548127637833756
-3.1748449268941386, 6.9037908696170645
0.664234661754108, 6.573698667901261
6.459071776694714, 6.932039608399064
0.9539765175011325, 6.922826202852192
0.9539765175011325, 6.915539784086365
6.459071776694714, 6.896661335465813
0.9539765175011325, 6.8863941090230565
0.9539765175011325, 6.8791076902572295
0.9539765175011325, 6.871821271491402
0.9539765175011325, 6.864534852725575
0.9539765175011325, 6.857248433959748
0.9539765175011325, 6.849962015193922
0.9539765175011325, 6.842675596428095
0.9539765175011325, 6.835389177662268
0.9539765175011325, 6.828102758896441
0.9539765175011325, 6.820816340130614
0.9539765175011325, 6.813529921364787
0.9539765175011325, 6.80624350259896
0.9539765175011325, 6.798957083833133
0.9539765175011325, 6.791670665067306
0.9539765175011325, 6.784384246301479
0.9539765175011325, 6.777097827535652
0.9539765175011325, 6.769811408769825
0.9539765175011325, 6.762524990003998
0.9539765175011325, 6.755238571238171
0.9539765175011325, 6.747952152472344
0.9539765175011325, 6.740665733706517
0.9539765175011325, 6.73337931494069
0.9539765175011325, 6.726092896174864
0.9539765175011325, 6.718806477409037
0.9539765175011325, 6.7115200586432096
0.9539765175011325, 6.7042336398773825
0.9539765175011325, 6.6969472211115555
0.9539765175011325, 6.6896608023457285
0.9539765175011325, 6.682374383579901
0.9539765175011325, 6.675087964814074
0.9539765175011325, 6.667801546048247
0.9539765175011325, 6.66051512728242
0.9539765175011325, 6.653228708516593
0.9539765175011325, 6.645942289750766
0.9539765175011325, 6.638655870984939
0.9539765175011325, 6.631369452219112
0.9539765175011325, 6.624083033453285
0.9539765175011325, 6.616796614687458
0.9539765175011325, 6.609510195921631
0.9539765175011325, 6.602223777155805
5.279408506867526, 6.5912468345994935
10.679644808743205, 6.9039212581634
16.781125387682778, 6.553557191409151
21.598083739477147, 6.932762228441955
21.598083739477147, 6.590962948154072
24.99828963486152, 6.901746242679398
32.8200086909103, 6.553285150896906
36.737095702259694, 6.932762228441955
36.44217988480284, 6.905674730107956
36.737095702259694, 6.590962948154072
51.87610766504213, 6.932762228441955
51.79009055161714, 6.9029231015926955
51.87610766504213, 6.590962948154072
51.87610766504213, 6.5561206184193
60.72358218874615, 6.901298147407703
67.01511962782456, 6.932762228441955
66.32698272042535, 6.908702326476816
67.01511962782456, 6.590962948154072
65.810880039876, 6.5554370566588185
75.27276251661499, 6.891753541007838
82.154131590607, 6.932762228441955
82.154131590607, 6.590962948154072
82.8422684980062, 6.556021258163402
84.90667922020384, 6.903616553378647
92.59087468616167, 6.8935149273623875
97.29314355338943, 6.932762228441955
97.29314355338943, 6.590962948154072
101.59399922463444, 6.898710640743702
99.68351386330255, 6.55196425523358
105.68841382365974, 6.895270291883246
112.43215551617186, 6.932762228441955
112.43215551617186, 6.590962948154072
116.15619054445006, 6.902057961129273
114.9729687127226, 6.553286728043826
127.57116747895441, 6.932762228441955
127.57116747895441, 6.590962948154072
130.85893270319502, 6.89931094228975
126.19489366415598, 6.550887644942024
142.71017944173684, 6.932762228441955
142.71017944173684, 6.590962948154072
145.46272707133346, 6.900424981522542
146.839000886132, 6.556076458305567
157.84919140451927, 6.932762228441955
157.84919140451927, 6.590962948154072
160.12004319893651, 6.898532620285218
163.35428666371286, 6.553815730850517
172.9882033673017, 6.932762228441955
172.9882033673017, 6.590962948154072
174.36447718210013, 6.909781620685059
176.3524282479201, 6.901016013347746
181.10821887461225, 6.555988138078103
188.12721533008414, 6.932762228441955
188.12721533008414, 6.590962948154072
191.49144021070254, 6.907830164230603
188.40247009304392, 6.899973343995734
196.38485821887457, 6.556548168611344
203.2662272928667, 6.932762228441955
203.2662272928667, 6.590962948154072
204.74836832418805, 6.904972495332351
207.5670829641117, 6.902043349326935
212.67076502732243, 6.553457342988731
218.40523925564912, 6.932762228441955
218.40523925564912, 6.590962948154072
221.6165448235123, 6.904455595539561
224.59847142224191, 6.572899253631881
227.62627381479854, 6.554126053282391
229.7907771780723, 6.90048519985945
233.54425121843155, 6.932762228441955
233.54425121843155, 6.590962948154072
241.11375719982277, 6.573044982007198
239.73748338502435, 6.901463747834199
245.83896396396392, 6.891472522102269
243.1781679220204, 6.554521917159063
248.683263181214, 6.932762228441955
248.683263181214, 6.590511310627265
257.6290429774036, 6.573703703703704
256.42480338945506, 6.893763328002132
259.69345369960126, 6.553754109467325
263.8222751439964, 6.932762228441955
264.05165411312953, 6.9030903120233384
263.8222751439964, 6.590962948154072
275.90290085167146, 6.910392370829445
274.1443287549845, 6.573703703703704
276.2087394771821, 6.553725720822783
278.96128710677897, 6.932762228441955
272.07991803278685, 6.902954151672663
278.96128710677897, 6.590962948154072
288.42316958351796, 6.91446338131414
294.1002990695614, 6.932762228441955
294.1002990695614, 6.904128409242363
294.1002990695614, 6.590962948154072
305.0046223714256, 6.9157945539732815
309.23931103234383, 6.932762228441955
309.23931103234383, 6.590962948154072
319.6989920248118, 6.91487738238038
324.37832299512627, 6.932762228441955
324.37832299512627, 6.590962948154072
329.4901971643775, 6.910453485272557
339.5173349579087, 6.932762228441955
339.5173349579087, 6.590962948154072
343.646156402304, 6.920531454085032
338.8291980505095, 6.911316973210715
353.83058263181215, 6.902357990137277
354.65634692069125, 6.932762228441955
354.65634692069125, 6.590962948154072
365.6665374390784, 6.8958217675892595
369.7953588834737, 6.932762228441955
369.7953588834737, 6.590962948154072
375.64452259636687, 6.890699720111955
383.5580970314577, 6.914726836538112
384.9343708462561, 6.932762228441955
384.9343708462561, 6.590962948154072
394.47653596219163, 6.902909991558931
400.07338280903866, 6.932762228441955
400.07338280903866, 6.590962948154072
408.331025697829, 6.906091843964168
415.2123947718211, 6.932762228441955
415.2123947718211, 6.590962948154072
423.4700376606114, 6.906459360700164
423.4700376606114, 6.898979741436758
430.35140673460353, 6.932762228441955
430.35140673460353, 6.590962948154072
438.4255464480874, 6.9082705386981
445.49041869738596, 6.932762228441955
445.49041869738596, 6.590962948154072
454.81849677546404, 6.910995994606272
460.6294306601684, 6.932762228441955
460.6294306601684, 6.590962948154072
467.8166383596712, 6.919117979968505
473.015894993354, 6.572925789128793
475.76844262295083, 6.931606036373329
475.76844262295083, 6.590962948154072
483.9400683983164, 6.907135562441689
478.5209902525477, 6.9237094051268375
489.53118077093484, 6.573514971789063
490.90745458573326, 6.932762228441955
490.90745458573326, 6.590541419795718
494.786044427438, 6.904248845916178
506.0464665485157, 6.932762228441955
506.0464665485157, 6.590752183974895
506.0464665485157, 6.573514971789063
516.3685201595039, 6.930609422897507
516.3685201595039, 6.592729352703364
517.0566570669031, 6.92260540228353
517.0566570669031, 6.915318983517703
517.0566570669031, 6.908032564751876
517.0566570669031, 6.900746145986049
517.0566570669031, 6.893459727220223
517.0566570669031, 6.886173308454396
517.0566570669031, 6.878886889688569
517.0566570669031, 6.871600470922742
517.0566570669031, 6.864314052156915
517.0566570669031, 6.857027633391088
517.0566570669031, 6.8497412146252605
517.0566570669031, 6.8424547958594335
517.0566570669031, 6.8351683770936065
517.0566570669031, 6.827881958327779
517.0566570669031, 6.820595539561952
517.0566570669031, 6.813309120796125
517.0566570669031, 6.806022702030298
517.0566570669031, 6.798736283264471
517.0566570669031, 6.791449864498644
517.0566570669031, 6.784163445732817
517.0566570669031, 6.77687702696699
517.0566570669031, 6.769590608201163
517.0566570669031, 6.762304189435337
517.0566570669031, 6.75501777066951
517.0566570669031, 6.747731351903683
517.0566570669031, 6.740444933137856
517.0566570669031, 6.733158514372029
517.0566570669031, 6.725872095606202
517.0566570669031, 6.718585676840375
517.0566570669031, 6.711299258074548
517.0566570669031, 6.704012839308721
517.0566570669031, 6.696726420542894
517.0566570669031, 6.689440001777067
517.0566570669031, 6.68215358301124
517.0566570669031, 6.674867164245413
517.0566570669031, 6.667580745479586
517.0566570669031, 6.660294326713759
517.0566570669031, 6.653007907947932
517.0566570669031, 6.645721489182105
517.0566570669031, 6.6384350704162784
517.0566570669031, 6.631148651650451
517.0566570669031, 6.623862232884624
517.0566570669031, 6.616575814118797
517.0566570669031, 6.60928939535297
517.0566570669031, 6.602002976587143
522.5617523260966, 6.573514971789063
')
tb <- tb %>%
  dplyr::filter(x > -500 & x < 500 & !(x == 0.9539765175011325)) %>%
  mutate(x = x*10) %>%
  dplyr::filter(y > 6.6 & y < 6.93)
ggplot(tb, aes(x = x, y = y)) + geom_point() +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  labs(x = 'Regular Earnings, Relative to Cutoff',
       y = 'Log Weekly Benefit Amount',
       caption = "Copyright ? 1999-2021 John Wiley & Sons, Inc. All rights reserved.\nModified slightly from original.") + 
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14))
ggsave('bana_first_stage.pdf', width = 6, height = 5,device=cairo_pdf)

# Log leave duration
tb1 <- read.csv(text = 'x,y
                -5930.199939920836, 2.404218447220091
-5934.8262563613225, 2.394413986517529
-5941.177639949109, 2.380205997032989
-5941.177639949109, 2.3717829646255817
-5884.720896946565, 2.4078284504096765
-5889.42562553011, 2.4008897710532544
-5879.07522264631, 2.3982670416092384
-5890.769833696837, 2.389610828344317
-5877.663804071247, 2.3863177946619607
-5890.6018076759965, 2.3831408903969686
-5884.720896946565, 2.3770703748625386
-5884.720896946565, 2.3747467705154968
-5835.321246819338, 2.4057529940185893
-5835.321246819338, 2.403373316163651
-5830.616518235793, 2.395050591556774
-5849.435432569974, 2.3933736769331357
-5835.321246819338, 2.380509576773219
-5835.321246819338, 2.3721122243426636
-5734.7576733460555, 2.377359545353142
-5616.5513676844785, 2.4367529567703117
-5625.372733778626, 2.422213076148253
-5616.5513676844785, 2.4169933818544864
-5616.5513676844785, 2.398503798521153
-5617.727549830364, 2.357921107567226
-5616.5513676844785, 2.3378560975531397
-5602.437181933842, 2.442846749339772
-5602.437181933842, 2.4403814715619943
-5602.437181933842, 2.4019455498448226
-5595.3800890585235, 2.382808914853248
-5621.256096268024, 2.3809356814272924
-5595.3800890585235, 2.3630419535693132
-5611.846639100932, 2.3612282006879655
-5597.732453350296, 2.3438126303450684
-5608.082856234097, 2.341703618886062
-5568.3278997031375, 2.3775175654244416
-5540.848007749248, 2.4204969678853137
-5506.460718829516, 2.436215170651329
-5506.460718829516, 2.4164929484291067
-5506.460718829516, 2.3979057811837365
-5506.460718829516, 2.3572461602346624
-5506.460718829516, 2.337372939748551
-5503.6378816793895, 2.443231253975827
-5503.6378816793895, 2.4407659761980494
-5503.6378816793895, 2.401915024551959
-5496.580788804071, 2.3834028280278132
-5496.580788804071, 2.3808874742951742
-5496.580788804071, 2.3636754698102207
-5497.992207379135, 2.360850515532231
-5503.6378816793895, 2.344226383219654
-5497.992207379135, 2.3410420085877863
-5503.6378816793895, 2.4385061382350863
-5503.6378816793895, 2.4001916127721232
-5489.523695928753, 2.4227915606446144
-5489.523695928753, 2.3773836549090177
-5293.101277565734, 2.3326338666463458
-5245.239711783128, 2.4399413217212005
-5245.239711783128, 2.4199346443707728
-5245.239711783128, 2.3999279670203455
-5256.6396310432565, 2.3803632989203423
-5245.239711783128, 2.3609023037368697
-5245.239711783128, 2.3408956263864424
-5207.239980916031, 2.43699827272406
-5207.239980916031, 2.434738434761097
-5207.239980916031, 2.432478596798134
-5207.239980916031, 2.430218758835171
-5207.239980916031, 2.427958920872208
-5207.239980916031, 2.4256990829092455
-5207.239980916031, 2.4234392449462825
-5207.239980916031, 2.4166597310573934
-5207.239980916031, 2.4143998930944304
-5207.239980916031, 2.4121400551314673
-5207.239980916031, 2.4098802171685048
-5207.239980916031, 2.4076203792055417
-5207.239980916031, 2.4053605412425787
-5207.239980916031, 2.4031007032796157
-5207.239980916031, 2.3963211893907266
-5207.239980916031, 2.394061351427764
-5207.239980916031, 2.391801513464801
-5207.239980916031, 2.389541675501838
-5207.239980916031, 2.387281837538875
-5207.239980916031, 2.385021999575912
-5207.239980916031, 2.382762161612949
-5207.239980916031, 2.3782424856870232
-5207.239980916031, 2.37598264772406
-5207.239980916031, 2.373722809761097
-5207.239980916031, 2.371462971798134
-5207.239980916031, 2.369203133835171
-5207.239980916031, 2.366943295872208
-5207.239980916031, 2.364683457909245
-5207.239980916031, 2.3579039440203564
-5207.239980916031, 2.3556441060573934
-5207.239980916031, 2.3533842680944304
-5207.239980916031, 2.3511244301314673
-5207.239980916031, 2.3488645921685047
-5207.239980916031, 2.3466047542055417
-5207.239980916031, 2.3443449162425787
-5150.783237913485, 2.338075936688771
-5150.783237913485, 2.3327487311011486
-5214.297073791348, 2.331504143695222
-5108.440680661577, 2.4423506856092736
-4995.527194656488, 2.337869071198628
-4998.350031806615, 2.333605134338776
-5019.050837574216, 2.3316456776134435
-4967.298823155216, 2.4039864753611178
-4953.18463740458, 2.4423679362807467
-4867.41381630456, 2.4080295096412105
-4840.271151399491, 2.3380170562977103
-4848.1123657053995, 2.3336905356175195
-4875.556615776081, 2.33162738144673
-4797.928594147583, 2.4423851869522197
-4769.70022264631, 2.4024156051839736
-4685.015108142494, 2.338034306969183
-4708.53875106022, 2.3331833136014515
-4669.815215795655, 2.374707002790555
-4713.243479643766, 2.331559816316794
-4642.672550890585, 2.442402437623693
-4558.928382103477, 2.4004363176127828
-4529.759064885496, 2.338051557640656
-4586.21580788804, 2.3749217314284707
-4487.416507633588, 2.4424196882951654
-4446.485368956743, 2.4014698091161297
-4374.503021628499, 2.338068808312129
-4360.388835877862, 2.4272125798378186
-4332.1604643765895, 2.4424369389666385
-4261.58953562341, 2.3991163353192033
-4219.2469783715005, 2.338086058983602
-4176.904421119592, 2.4424541896381116
-4162.790235368957, 2.4078171376759463
-4063.990935114504, 2.374663036699828
-4063.9909351145034, 2.338103309655075
-4021.648377862595, 2.4424714403095846
-3965.1916348600507, 2.3854214738029147
-3908.734891857506, 2.3381205603265482
-3866.3923346055976, 2.4424886909810577
-3864.039970313825, 2.3826517529592914
-3754.655030746395, 2.3849201781409772
-3753.4788486005086, 2.338137810998021
-3711.1362913486, 2.4425059416525308
-3667.6175519508056, 2.412465204151353
-3598.222805343511, 2.338155061669494
-3568.9087272460365, 2.3804394129844395
-3555.880248091603, 2.442523192324004
-3584.108619592875, 2.4125258428753185
-3471.195133587786, 2.433110384819891
-3442.966762086514, 2.338172312340967
-3485.309319338422, 2.380590929371643
-3400.6242048346053, 2.4425404429954765
-3358.281647582697, 2.4129711525641273
-3386.5100190839694, 2.4331944996642636
-3287.7107188295163, 2.33818956301244
-3259.482347328244, 2.39015963447291
-3245.3681615776077, 2.4425576936669495
-3160.683047073791, 2.4015164928547565
-3132.4546755725187, 2.338206813683913
-3090.1121183206105, 2.4425749443384226
-3061.8837468193383, 2.412126297364549
-2977.1986323155215, 2.338224064355386
-2961.9987399686825, 2.3913872560813165
-2934.856075063613, 2.4425921950098957
-2864.2851463104325, 2.396376077890868
-2821.942589058524, 2.3382413150268593
-2795.885630749657, 2.3327421105887036
-2779.6000318066153, 2.4426094456813687
-2765.4858460559794, 2.4073033802962835
-2765.4858460559794, 2.3271269648104065
-2665.600839205324, 2.4003489195945065
-2666.6865458015263, 2.338258565698332
-2660.0445760365214, 2.3327958130903976
-2624.343988549618, 2.442626696352842
-2680.8007315521627, 2.407387495140656
-2598.4679813401185, 2.326858121536403
-2538.2474554707373, 2.400788172356517
-2511.430502544529, 2.3380983910751922
-2498.492498939779, 2.333215608202657
-2546.715966921119, 2.3318005415959853
-2469.0879452926206, 2.4426439470243144
-2454.9737595419847, 2.3859254500314857
-2441.8677299163937, 2.3267123925685276
-2483.202131043257, 2.4523365700430277
-2389.6956504452924, 2.4501128016658718
-2419.6882951653943, 2.4549501447863658
-2356.1744592875316, 2.3920902840025575
-2356.1744592875316, 2.338293067041278
-2353.0379735651677, 2.3341161393051237
-2370.288645038168, 2.331981561629105
-2313.831902035623, 2.4426611976957875
-2281.26070414954, 2.3268249809170016
-2251.0237754452924, 2.45184613896057
-2257.3751590330785, 2.3824829430998924
-2200.918416030534, 2.338310317712751
-2207.0933722964373, 2.333138968267379
-2158.575858778626, 2.4426784483672606
-2158.575858778626, 2.41230137029487
-2140.9331265903306, 2.326784961139973
-2097.414387192536, 2.450160970680976
-2058.6908519279696, 2.3988834569577655
-2045.6623727735368, 2.338327568384224
-2073.8907442748086, 2.4548498948614648
-2003.3198155216287, 2.4426956990387336
-2009.368752271901, 2.326775611425391
-2010.3769083969464, 2.452196506352488
-1953.4496925360472, 2.4021885086820283
-1890.4063295165388, 2.3383448190556972
-1848.0637722646306, 2.4427129497102067
-1841.006679389313, 2.32692100620818
-1828.0686757845624, 2.402740007421544
-1763.3786577608143, 2.4199372160697306
-1751.1463634435959, 2.451050878805014
-1749.264472010178, 2.4549590097147616
-1735.1502862595416, 2.33836206972717
-1692.8077290076335, 2.4427302003816798
-1672.4205718122703, 2.3275308213169557
-1651.550878351929, 2.4030769986855987
-1678.6935432569971, 2.4199279491800962
-1594.0084287531809, 2.4507906835530884
-1579.8942430025445, 2.338379320398643
-1564.6943506557054, 2.366660589937964
-1537.5516857506364, 2.442747451053153
-1516.3804071246814, 2.3269800654875503
-1439.6933312128922, 2.3957393898758106
-1424.6381997455464, 2.338396571070116
-1481.0949427480919, 2.3668437124505233
-1378.9351220768203, 2.4520807924328025
-1382.2956424936383, 2.4427647017246255
-1340.8940309584395, 2.32710805576171
-1325.8388994910938, 2.3970249085033895
-1269.3821564885493, 2.338413821741589
-1255.267970737913, 2.4004301995383224
-1239.1374727371858, 2.452665198135022
-1216.9580379861864, 2.4501821668523456
-1227.0395992366412, 2.4427819523960985
-1156.4686704834603, 2.3828013676844786
-1114.1261132315522, 2.338431072413062
-1092.954834605598, 2.3268707502476653
-1077.4292302798976, 2.452368190610962
-1077.664466709076, 2.4497018165532403
-1071.783555979644, 2.4427992030675716
-1057.6693702290077, 2.403944631790231
-958.8700699745541, 2.41853249816871
-958.8700699745541, 2.338448323084535
-980.0413486005091, 2.4535949869239473
-980.0413486005091, 2.4501538700257988
-916.527512722646, 2.4428164537390447
-930.6416984732823, 2.3267528034488585
-860.0707697201015, 2.3825114667898544
-803.614026717557, 2.3384655737560083
-761.2714694656488, 2.4428337044105177
-761.2714694656488, 2.394743148971872
-764.295937840785, 2.3269091837106584
-725.9860050890584, 2.4551229259435967
-720.8108036471585, 2.45116536053388
-775.3856552162852, 2.382740990334323
-663.648351357082, 2.392592505292688
-648.3579834605598, 2.338482824427481
-606.0154262086517, 2.442850955081991
-568.7136495819695, 2.327586243628159
-570.7299618320612, 2.4508875724484027
-550.6443898023099, 2.3822128543923036
-480.2708622484379, 2.3934659894106463
-493.10194020356175, 2.338500075098954
-450.7593829516536, 2.442868205753464
-413.1215542832906, 2.3270100714498883
-387.95125636132343, 2.4520655345499365
-361.92303734470806, 2.392877650941102
-337.8458969465646, 2.338517325770427
-295.50333969465646, 2.4428854564249365
-253.1607824427483, 2.3878136144363484
-242.57514312977128, 2.4525870877386917
-213.64106234096653, 2.4510094249187167
-232.7736252473842, 2.327914610035184
-182.58985368956746, 2.3385345764419
-148.93294920728113, 2.397952695772099
-140.24729643765932, 2.4429027070964096
-99.31615776081435, 2.4526204494451513
-94.37619274809185, 2.4508685967097827
-83.7905534351139, 2.327377617542134
-126.13311068702296, 2.3254611812034685
-35.03245720564428, 2.4000708580718126
-24.07669064396123, 2.3387418257677086
-6.162531806615334, 2.3331934843098407
15.00874681933874, 2.4427612088200714
34.163713195201126, 2.389544419926845
-6.162531806615334, 2.4398360081813686
-6.162531806615334, 2.4375761702184056
-6.162531806615334, 2.4353163322554425
-6.162531806615334, 2.43305649429248
-6.162531806615334, 2.430796656329517
-6.162531806615334, 2.428536818366554
-6.162531806615334, 2.426276980403591
-6.162531806615334, 2.424017142440628
-6.162531806615334, 2.421757304477665
-6.162531806615334, 2.4194974665147018
-6.162531806615334, 2.417237628551739
-6.162531806615334, 2.414977790588776
-6.162531806615334, 2.412717952625813
-6.162531806615334, 2.41045811466285
-6.162531806615334, 2.408198276699887
-6.162531806615334, 2.405938438736924
69.44917757179155, 2.4023956660961674
-6.162531806615334, 2.3968990868850724
-6.162531806615334, 2.3945616383254014
-6.162531806615334, 2.3923794109591463
-6.162531806615334, 2.3855998970702577
-6.162531806615334, 2.3833400591072946
-6.162531806615334, 2.3810802211443316
-6.162531806615334, 2.3788203831813686
-6.162531806615334, 2.3765605452184055
-6.162531806615334, 2.3743007072554425
-6.162531806615334, 2.37204086929248
-6.162531806615334, 2.369781031329517
-6.162531806615334, 2.367521193366554
-6.162531806615334, 2.365261355403591
-6.162531806615334, 2.363001517440628
-6.162531806615334, 2.360741679477665
-6.162531806615334, 2.3584818415147017
-6.162531806615334, 2.356222003551739
-6.162531806615334, 2.353962165588776
-6.162531806615334, 2.351702327625813
-6.162531806615334, 2.34944248966285
-6.162531806615334, 2.347182651699887
-6.162531806615334, 2.344922813736924
-6.162531806615334, 2.342662975773961
-6.162531806615334, 2.335609542131986
57.35130407124689, 2.453448212194123
127.92223282442774, 2.3385690777848462
151.44587574215439, 2.3270728011643356
156.15060432569953, 2.4522329331491695
170.26479007633588, 2.4429372084393557
209.4708616058806, 2.3915701976427766
184.37897582697224, 2.4018654879917407
283.1782760814249, 2.3385863284563193
305.1336761379698, 2.453140486909549
325.52083333333303, 2.4429544591108288
313.57806077510304, 2.327091435431955
340.720725680173, 2.4060876027598357
304.34955470737896, 2.391771912880796
387.8584870653103, 2.4512132180785273
438.43431933842294, 2.338603579127792
419.61540500424144, 2.4532608727823724
452.5485050890584, 2.3815514068403503
480.7768765903311, 2.442971709782302
521.8363260467277, 2.4063986467329146
509.0052480916029, 2.3279696379346904
593.6903625954201, 2.338620829799265
636.0329198473282, 2.4429889604537744
600.7474554707378, 2.4067249125318066
650.1471055979646, 2.381598264036142
646.114481097783, 2.327854108221309
734.8322201017809, 2.4553730179096953
731.8077517266447, 2.4512318689668406
751.1178190448227, 2.400797411202648
748.9464058524172, 2.338638080470738
791.2889631043254, 2.4430062111252475
819.5173346055981, 2.3280087046068547
854.8027989821885, 2.402888779942231
890.0882633587789, 2.4550074034717406
890.0882633587789, 2.451337501325276
904.2024491094153, 2.338655331142211
946.5450063613234, 2.4430234617967206
946.5450063613234, 2.3884645166909007
985.1733042051701, 2.3267818228855557
1046.7557251908402, 2.4529573393765904
1045.344306615776, 2.3754167259024728
1059.4584923664124, 2.338672581813684
1101.8010496183206, 2.4430407124681937
1145.3197890161164, 2.3979258285549787
1146.315020062635, 2.327168121071747
1184.4698518720488, 2.4515829873417245
1130.0294211195933, 2.3753514299724343
1214.7145356234096, 2.3386898324851573
1186.4861641221378, 2.454109631956225
1242.9429071246823, 2.3871861034405764
1257.0570928753186, 2.4430579631396667
1228.828721374046, 2.3979607873020923
1340.5660252332482, 2.4515844342324593
1264.1141857506364, 2.326445972376838
1341.742207379135, 2.3800066876220862
1369.9705788804076, 2.3387164213300307
1414.0774093511454, 2.4538481601308493
1327.6280216284986, 2.387186174724343
1412.3131361323158, 2.44307521381114
1454.655693384224, 2.393503555955548
1507.079811886586, 2.451399766967856
1468.7698791348603, 2.3280808437784692
1426.4273218829521, 2.38010947881326
1525.2266221374048, 2.338724333828103
1553.4549936386766, 2.417704404837635
1567.569179389313, 2.443092464482613
1601.2864009047216, 2.4523072380932165
1638.1401081424938, 2.3274112762657015
1653.3400004893329, 2.3898219687547577
1680.482665394402, 2.338741584499576
1722.825222646311, 2.4431097151540855
1761.6392334605598, 2.451395820356676
1751.0535941475828, 2.3986204574599137
1786.3390585241732, 2.453876682547887
1820.056280039582, 2.328085635631656
1835.7387086514, 2.338758835171049
1849.8528944020354, 2.3934260705014525
1878.0812659033081, 2.4431269658255585
1916.0809967704063, 2.451372485204487
1948.652194656489, 2.3743218072503023
1934.5380089058526, 2.4541937271127
1976.8805661577608, 2.3281850289632975
1990.9947519083962, 2.338776085842522
2033.3373091603062, 2.4431442164970316
2047.4514949109416, 2.382951918999152
2121.3829440809404, 2.4525043060189518
2146.250795165395, 2.4164309101671937
2146.250795165395, 2.3387933365139952
2149.2752635405286, 2.3272435876304423
2188.5933524173033, 2.4431614671685047
2132.136609414758, 2.382858608548912
2246.1358020160496, 2.3898138239447184
2255.450021762421, 2.3333216683887503
2274.8467097823013, 2.4532490151238497
2285.376340421666, 2.4513164420639972
2230.9359096692115, 2.4164589959711624
2301.5068384223923, 2.3388105871854683
2343.8493956743005, 2.4431787178399778
2344.935102270503, 2.327387998438629
2357.963581424936, 2.365514697909438
2416.18459764631, 2.3905926950647998
2421.4774173028, 2.453695589703492
2439.1201494910947, 2.451162936291791
2421.4774173028, 2.3334384592471427
2456.7628816793895, 2.3386971034293316
2499.1054389312976, 2.443195968511451
2482.168416030534, 2.3272993238885356
2555.562181933843, 2.395841027031837
2506.1625318066162, 2.390712994227899
2513.219624681933, 2.4510042497172746
2590.8476463104325, 2.3336302715293487
2612.0189249363866, 2.338845088528414
2654.3614821882948, 2.4432132191829234
2654.3614821882948, 2.42555473328225
2633.733056860443, 2.3273956237083824
2753.1607824427483, 2.395524807152312
2753.1607824427483, 2.3336538499363026
2767.2749681933838, 2.338862339199887
2809.617525445292, 2.4432304698543965
2781.389153944021, 2.32751989439561
2851.960082697202, 2.385883226280511
2922.531011450381, 2.33887958987136
2950.7593829516536, 2.4148581051449614
2964.873568702291, 2.4432477205258696
3050.64438980231, 2.393738353715394
3077.78705470738, 2.338896840542833
3035.44449745547, 2.414904867295731
3120.129611959288, 2.4432649711973426
3148.3579834605607, 2.418017686807644
3134.2437977099235, 2.3938582639772408
3233.043097964377, 2.3389140912143063
3250.181752090146, 2.3751541114152936
3275.385655216285, 2.4432822218688157
3233.043097964377, 2.41800841991801
3360.0707697201015, 2.379343819878171
3388.299141221374, 2.338931341885779
3418.2917859414756, 2.3747325621333406
3430.6416984732823, 2.443299472540289
3543.5551844783713, 2.338948592557252
3557.6693702290086, 2.3796645968270496
3508.269720101782, 2.374639839774173
3585.8977417302794, 2.443316723211762
3698.8112277353684, 2.338965843228725
3679.992313401188, 2.379526702341124
3741.1537849872766, 2.4433339738832345
3755.267970737914, 2.4094099581614494
3855.15297758857, 2.3616922156026403
3854.0672709923656, 2.338983093900198
3896.4098282442756, 2.4433512245547075
3952.866571246819, 2.3577171092685765
4009.3233142493646, 2.339000344571671
4051.6658715012727, 2.4433684752261806
4051.6658715012727, 2.3808680806554734
4037.5516857506354, 2.357801224112949
4164.579357506362, 2.3390175952431442
4206.92191475827, 2.4433857258976537
4180.104961832061, 2.3803846270232545
4263.378657760813, 2.405017553460148
4319.835400763359, 2.3390348459146173
4362.177958015267, 2.4434029765691267
4362.177958015267, 2.3451934069142446
4460.977258269721, 2.355252259193397
4475.091444020356, 2.33905209658609
4517.434001272264, 2.4434202272406
4559.776558524172, 2.3874205710234158
4630.347487277353, 2.3390786854309638
4658.575858778626, 2.4175131403089423
4672.690044529261, 2.443437477912073
4757.375159033079, 2.3641377806773205
4757.375159033079, 2.3335960906182476
4785.60353053435, 2.339086597929036
4827.94608778626, 2.4434547285835455
4857.260165883736, 2.4218631005537614
4926.745388040712, 2.3339023341454523
4940.859573791349, 2.3389824523463005
4954.973759541985, 2.4099821529544814
4983.202131043257, 2.4434719792550186
4940.859573791349, 2.4221015337856944
5096.115617048346, 2.339065070231578
5089.058524173028, 2.333938666488
5039.658874045801, 2.4099915624116486
5138.4581743002545, 2.4433398191520808
5197.737754452926, 2.3400157818242864
5201.972010178117, 2.440414689797145
5201.972010178117, 2.438154851834182
5201.972010178117, 2.4358950138712188
5201.972010178117, 2.4336351759082557
5201.972010178117, 2.4313753379452927
5201.972010178117, 2.42911549998233
5201.972010178117, 2.426855662019367
5201.972010178117, 2.424595824056404
5201.972010178117, 2.422335986093441
5201.972010178117, 2.420076148130478
5201.972010178117, 2.417816310167515
5201.972010178117, 2.4155564722045524
5201.972010178117, 2.4132966342415894
5201.972010178117, 2.4110367962786263
5201.972010178117, 2.4087769583156633
5201.972010178117, 2.4065171203527003
5201.972010178117, 2.4042572823897372
5201.972010178117, 2.401997444426774
5201.972010178117, 2.3997376064638116
5201.972010178117, 2.3974777685008486
5201.972010178117, 2.3952179305378856
5201.972010178117, 2.3929580925749225
5201.972010178117, 2.3906982546119595
5201.972010178117, 2.3884384166489965
5201.972010178117, 2.3861785786860334
5201.972010178117, 2.383918740723071
5201.972010178117, 2.381658902760108
5201.972010178117, 2.379399064797145
5201.972010178117, 2.3771392268341818
5201.972010178117, 2.3748793888712187
5201.972010178117, 2.3726195509082557
5201.972010178117, 2.3703597129452927
5201.972010178117, 2.36809987498233
5201.972010178117, 2.365840037019367
5201.972010178117, 2.363580199056404
5201.972010178117, 2.361320361093441
5201.972010178117, 2.359060523130478
5201.972010178117, 2.356800685167515
5201.972010178117, 2.354540847204552
5201.972010178117, 2.3522810092415893
5201.972010178117, 2.3500211712786263
5201.972010178117, 2.3477613333156633
5201.972010178117, 2.3455014953527002
5201.972010178117, 2.343241657389737
5258.428753180662, 2.333957485402334') %>%
  dplyr::filter(x > -5000 & x < 5000 & !(x == -6.162531806615334) & y < 2.44 & y > 2.34)
p1 <- ggplot(tb1, aes(x = x, y = y, group = x > 0)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black') +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  labs(x = 'Regular Earnings - Cutoff',
       y = 'Log Duration of Family Leave',
       caption =  '\n') + 
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        plot.caption = element_text(size = 9))

tb2 <- read.csv(text = 'x,y
                -5935.0984766132215, 0.26623465473616076
-5932.195259735686, 0.2479570772405863
-5935.0984766132215, 0.23075375985014543
-5932.820567986232, 0.20770602924518589
-5915.7207923655305, 0.27071302468255665
-5918.907459411583, 0.2565746332909988
-5925.461170883652, 0.24424187826255717
-5924.378906603861, 0.2418156366725644
-5912.319390343329, 0.23503462729121616
-5913.436012219305, 0.2117725824952231
-5910.309470966575, 0.26220150444435086
-5910.309470966575, 0.22689635464936675
-5882.170599692003, 0.2737419081922094
-5854.031728417431, 0.2684444940935905
-5882.170599692003, 0.2527375504348396
-5882.170599692003, 0.23822072204903533
-5851.686822477883, 0.23281366867040815
-5878.820734064077, 0.22326887086400143
-5882.170599692003, 0.21849606965156668
-5870.111083431471, 0.2148214221354282
-5854.969690793249, 0.25944411406309
-5862.238899205848, 0.2501574728400783
-5823.547951203311, 0.2657126368722754
-5825.892857142859, 0.24735062043043973
-5823.547951203311, 0.23017250090104008
-5825.892857142859, 0.20785693897384414
-5818.858139324216, 0.2710837741993616
-5818.858139324216, 0.2553039401194522
-5814.168327445121, 0.2427810601379879
-5818.858139324216, 0.23550060820203894
-5818.858139324216, 0.2112413838044486
-5811.823421505573, 0.26203170373802903
-5811.823421505573, 0.22695393110905163
-5797.753985868287, 0.27384036144578316
-5797.753985868287, 0.2689189450108125
-5797.753985868287, 0.25292091442693854
-5727.406807681857, 0.23899060987539905
-5797.753985868287, 0.23341346153846157
-5797.753985868287, 0.22387678920811455
-5750.855867077333, 0.22105005921120383
-5750.855867077333, 0.21909794305426838
-5797.753985868287, 0.21476691380908253
-5783.684550231001, 0.2489865230151375
-5585.630187029205, 0.2810226557116038
-5588.275721935362, 0.2789689312806783
-5585.630187029205, 0.2610235884365866
-5588.275721935362, 0.259015489307658
-5586.712451308997, 0.24176445759728896
-5586.712451308997, 0.20092762659027724
-5572.643015671711, 0.23943057544864776
-5573.725279951502, 0.22151664646752695
-5571.079745045346, 0.2193517059691759
-5481.191684029352, 0.2812821188600556
-5484.7090429386735, 0.27813460573061477
-5481.191684029352, 0.26151540054448563
-5484.7090429386735, 0.25827072713932653
-5488.226401847995, 0.24197072477945974
-5484.7090429386735, 0.19993857642106894
-5478.8467780898045, 0.23843083873957371
-5478.8467780898045, 0.22074207084749256
-5481.191684029352, 0.21799119555143653
-5278.3573202584785, 0.18681514197816912
-5230.647503257682, 0.27928798032366153
-5230.647503257682, 0.25983008958912573
-5242.01127819549, 0.23989903846153848
-5230.647503257682, 0.21992517406905732
-5230.647503257682, 0.20045990173712608
-5136.490510915845, 0.28895854396607407
-5192.768253464989, 0.2862193002780352
-5192.768253464989, 0.28410816342292244
-5192.768253464989, 0.2819970265678097
-5192.768253464989, 0.2756636160024714
-5192.768253464989, 0.2735524791473587
-5192.768253464989, 0.2714413422922459
-5192.768253464989, 0.26933020543713315
-5192.768253464989, 0.2672190685820204
-5192.768253464989, 0.26510793172690766
-5192.768253464989, 0.2629967948717949
-5192.768253464989, 0.25666338430645663
-5192.768253464989, 0.25455224745134386
-5192.768253464989, 0.2524411105962311
-5192.768253464989, 0.2503299737411183
-5192.768253464989, 0.24821883688600557
-5192.768253464989, 0.24610770003089283
-5192.768253464989, 0.24399656317578006
-5192.768253464989, 0.2418854263206673
-5192.768253464989, 0.2376631526104418
-5192.768253464989, 0.23555201575532903
-5192.768253464989, 0.23344087890021628
-5192.768253464989, 0.23132974204510354
-5192.768253464989, 0.22921860518999077
-5192.768253464989, 0.227107468334878
-5192.768253464989, 0.22499633147976525
-5192.768253464989, 0.2228851946246525
-5192.768253464989, 0.21655178405931422
-5192.768253464989, 0.21444064720420147
-5192.768253464989, 0.2123295103490887
-5192.768253464989, 0.21021837349397593
-5192.768253464989, 0.20810723663886319
-5192.768253464989, 0.20599609978375044
-5192.768253464989, 0.20388496292863767
-5192.768253464989, 0.1975515523632994
-5192.768253464989, 0.19544041550818664
-5148.550027176376, 0.19215033209762133
-5134.614586164207, 0.18696388116568846
-5199.802971283632, 0.18574837812789624
-4981.726718905699, 0.2891940840284214
-4981.726718905699, 0.19166305080461715
-4984.540606033157, 0.18764200391308833
-5005.175778301176, 0.18586033235506133
-4953.587847631127, 0.1965483261718202
-4855.101798170125, 0.20907556800629096
-4826.962926895553, 0.2891940840284214
-4826.962926895553, 0.19179390639481006
-4831.183757586739, 0.18771557383379678
-4847.062120663105, 0.18587175625579244
-4756.615748709123, 0.20333536944982733
-4672.199134885408, 0.2891940840284214
-4662.458756367287, 0.19201289378420985
-4693.303288341336, 0.1879674708449182
-4700.338006159979, 0.18589917361754718
-4652.267434399252, 0.19441683400267742
-4672.199134885408, 0.20330919833178873
-4517.435342875262, 0.2891940840284214
-4517.435342875262, 0.1917851826887972
-4529.941507886182, 0.19510988398036597
-4461.157600326118, 0.2192212380992502
-4362.671550865116, 0.2891940840284214
-4362.671550865116, 0.19179390639481006
-4348.60211522783, 0.20828463943185332
-4290.565693224025, 0.2176073524868706
-4207.90775885497, 0.2891940840284214
-4207.90775885497, 0.19179390639481006
-4210.721645982427, 0.21718512511584803
-4151.630016305826, 0.2087876857078665
-4053.1439668448243, 0.2891940840284214
-4053.1439668448243, 0.19952310992220634
-4053.1439668448243, 0.19179390639481006
-3954.657917383822, 0.21388233001937823
-3898.380174834678, 0.2891940840284214
-3898.380174834678, 0.19179390639481006
-3856.17186792282, 0.22150975697662448
-3757.685818461818, 0.228442195354846
-3743.616382824532, 0.2891940840284214
-3743.616382824532, 0.19179390639481006
-3775.7750928526143, 0.2979539311090516
-3696.718264033579, 0.3001496048295747
-3671.705934011737, 0.29634849225963683
-3659.1997690008166, 0.22601991298527446
-3588.8525908143865, 0.2891940840284214
-3588.8525908143865, 0.19179390639481006
-3559.6314552600234, 0.20302131603336426
-3513.815600748861, 0.29857478203411936
-3532.5748482652425, 0.29660438763601416
-3477.576145319488, 0.2263136110877075
-3434.0887988042405, 0.2891940840284214
-3434.0887988042405, 0.19179390639481006
-3476.2971057160985, 0.20321323756564724
-3349.6721849805244, 0.22686756641952427
-3349.6721849805244, 0.20886619906198223
-3356.7069027991674, 0.3003895067449284
-3342.637467161882, 0.29604328371177013
-3279.325006794095, 0.2891940840284214
-3279.325006794095, 0.19179390639481006
-3293.394442431381, 0.29847029142209863
-3223.047264244951, 0.22712117701575535
-3152.700086058521, 0.21658144465975793
-3124.561214783949, 0.2891940840284214
-3124.561214783949, 0.19179390639481006
-3054.2140365975188, 0.21391286299042328
-3024.5118946965817, 0.2970166635201776
-2969.797422773803, 0.2891940840284214
-2969.797422773803, 0.19179390639481006
-2955.727987136517, 0.2076137241272784
-2911.643755473021, 0.29893425411045893
-2856.159673395724, 0.2158874402937193
-2815.033630763657, 0.2891940840284214
-2815.033630763657, 0.19179390639481006
-2788.0672124588586, 0.1866690683674871
-2803.778082253828, 0.2967270041705283
-2758.755888214513, 0.21973572904672306
-2758.755888214513, 0.18144313228048176
-2772.825323851799, 0.21597601946246528
-2660.269838753511, 0.2891940840284214
-2659.18757447372, 0.2540650620232409
-2660.269838753511, 0.19179390639481006
-2653.648927865376, 0.18674637009576775
-2632.130967478939, 0.2990169769989047
-2647.959082570886, 0.2965710679255483
-2674.339274390797, 0.21991041087426633
-2592.2675665066295, 0.18112146985377417
-2560.611336322736, 0.20450870790855735
-2505.5060467433655, 0.2891940840284214
-2575.8532249297955, 0.2541684043867779
-2505.5060467433655, 0.1916194322745528
-2492.609064075853, 0.18706532540418086
-2540.6796358365805, 0.18574837812789624
-2481.3870142223036, 0.29884499536607967
-2482.0569873478885, 0.2961032591906086
-2449.2283041942214, 0.23146932134130932
-2436.1638282453127, 0.18099600278403
-2477.3671754687934, 0.2045566882916281
-2350.7422547332194, 0.2891940840284214
-2350.7422547332194, 0.22537145083831833
-2350.7422547332194, 0.19179390639481006
-2347.6157134804894, 0.18787017729035807
-2364.8116903705054, 0.18589917361754718
-2324.1666540850124, 0.2982024136431767
-2350.7422547332194, 0.29575540141334566
-2276.0660194276243, 0.1810704841060495
-2252.256205272217, 0.20280883147976525
-2195.978462723074, 0.2891940840284214
-2195.978462723074, 0.19179390639481006
-2199.2889181671408, 0.1869371626778608
-2224.117333997646, 0.29933393831737204
-2153.770155811216, 0.22859922206307753
-2136.183361264608, 0.1810303071259397
-2055.2841063502137, 0.23687801906928416
-2041.2146707129277, 0.2891940840284214
-2041.2146707129277, 0.19179390639481006
-2005.0361219313354, 0.1810182304308811
-1956.7980568892117, 0.21267845858960321
-1931.7857268673706, 0.2973454179967734
-1922.7969207657707, 0.29997817015926953
-1886.4508787027817, 0.2891940840284214
-1886.4508787027817, 0.19179390639481006
-1857.2297431484185, 0.23778863976616527
-1837.2078539722806, 0.18111324464524772
-1816.1037005163516, 0.2971451189372876
-1805.5516237883876, 0.3002055819431572
-1758.7436936874165, 0.20226839309902336
-1731.6870866926356, 0.2891940840284214
-1731.6870866926356, 0.19179390639481006
-1703.5482154180645, 0.23790305452579552
-1661.3399085062065, 0.2115356531019182
-1669.1562616380315, 0.1816623450115753
-1647.2704728689205, 0.2977620095767686
-1675.4093441434925, 0.20244555143651533
-1580.9431334360006, 0.29663789774482546
-1576.9232946824905, 0.2891940840284214
-1576.9232946824905, 0.19179390639481006
-1548.7844234079184, 0.23799465343893056
-1576.9232946824905, 0.21156182421995678
-1513.6108343147034, 0.18113677788075386
-1457.9726115672547, 0.2982667382817985
-1450.2983739469164, 0.23054714253283534
-1422.1595026723444, 0.2891940840284214
-1422.1595026723444, 0.19179390639481006
-1397.5379903070934, 0.2997377732082175
-1351.8123244859144, 0.2489865230151375
-1329.466750238461, 0.29728865689000344
-1338.680851224447, 0.18123395719630667
-1267.3957106621983, 0.2891940840284214
-1267.3957106621983, 0.19179390639481006
-1253.3262750249123, 0.22291136574269108
-1155.9224898437024, 0.2974890310749903
-1154.8402255639103, 0.2459245022046227
-1112.6319186520523, 0.2891940840284214
-1112.6319186520523, 0.19179390639481006
-1091.5277651961233, 0.1809981678852161
-1055.271911823117, 0.21973325253677434
-1033.8430790832508, 0.2975381214315953
-957.8681266419071, 0.2891940840284214
-957.8681266419071, 0.25828100864998454
-957.8681266419071, 0.19179390639481006
-929.7292553673351, 0.18087570366937844
-873.4515128181911, 0.2986470918639593
-880.4862306368341, 0.2966214473277726
-858.299812901113, 0.22145316472992566
-803.104334631761, 0.2891940840284214
-803.104334631761, 0.19179390639481006
-758.5511217803551, 0.22473243937287615
-763.9109067850359, 0.18104031119900854
-774.965463357189, 0.22163770466481314
-687.5868630835175, 0.2978473882032855
-648.340542621615, 0.2891940840284214
-648.340542621615, 0.22711619204089087
-648.340542621615, 0.19179390639481006
-676.479413896187, 0.2246124884151993
-568.9487272397864, 0.18162115127175377
-577.993364435185, 0.2955338919781691
-540.8098559652144, 0.24354417670682735
-539.8048962768371, 0.29884499536607967
-493.5767506114689, 0.2891940840284214
-493.5767506114689, 0.19179390639481006
-416.1948546063959, 0.1810998297218177
-397.1006205272224, 0.2970971385542169
-431.6712338074112, 0.24399656317578006
-352.88239423860887, 0.2533832708456203
-338.81295860132286, 0.2891940840284214
-338.81295860132286, 0.19179390639481006
-289.56993387082184, 0.2985822456492637
-254.39634477760774, 0.20953293945010817
-226.25747350303573, 0.2972089404627055
-234.07382663486078, 0.1816753767205575
-184.0491665911777, 0.2891940840284214
-184.0491665911777, 0.19179390639481006
-155.9102953166057, 0.22613913696745025
-92.59783494881867, 0.29867436798475955
-99.63255276746168, 0.29660438763601416
-85.56311713017567, 0.1813395140390623
-127.77142404203369, 0.17957490217279382
-45.364729595073186, 0.23629228452270623
-29.285374581031647, 0.28895854396607407
-23.255616450765956, 0.1919035758418289
-8.181221125102638, 0.1869510863968696
41.06180360539838, 0.2309858826330965
-1.1465033064596355, 0.2862193002780352
-1.1465033064596355, 0.28410816342292244
-1.1465033064596355, 0.2819970265678097
-1.1465033064596355, 0.27988588971269696
-1.1465033064596355, 0.2777747528575842
-1.1465033064596355, 0.2756636160024714
-1.1465033064596355, 0.2735524791473587
-1.1465033064596355, 0.2714413422922459
-1.1465033064596355, 0.26933020543713315
-1.1465033064596355, 0.2672190685820204
-1.1465033064596355, 0.26510793172690766
-1.1465033064596355, 0.2629967948717949
-1.1465033064596355, 0.2608856580166821
-1.1465033064596355, 0.25877452116156935
-1.1465033064596355, 0.25666338430645663
-1.1465033064596355, 0.25455224745134386
-1.1465033064596355, 0.2524411105962311
-1.1465033064596355, 0.2503299737411183
-1.1465033064596355, 0.24821883688600557
-1.1465033064596355, 0.24610770003089283
60.40727760666687, 0.24290500946092064
-1.1465033064596355, 0.24191741324271449
-1.1465033064596355, 0.2394544202450829
-1.1465033064596355, 0.23312100967974464
-1.1465033064596355, 0.2284029386777881
-1.1465033064596355, 0.2260838868293688
-1.1465033064596355, 0.22397274997425604
-1.1465033064596355, 0.22186161311914326
-1.1465033064596355, 0.21975047626403052
-1.1465033064596355, 0.21763933940891778
-1.1465033064596355, 0.215528202553805
-1.1465033064596355, 0.21341706569869223
-1.1465033064596355, 0.2113059288435795
-1.1465033064596355, 0.20919479198846674
-1.1465033064596355, 0.20708365513335397
-1.1465033064596355, 0.2049725182782412
-1.1465033064596355, 0.20286138142312846
-1.1465033064596355, 0.2007502445680157
-1.1465033064596355, 0.19863910771290294
-1.1465033064596355, 0.19652797085779017
-1.1465033064596355, 0.19505657244362068
83.2701105172564, 0.2990169769989047
67.44199542530987, 0.2965710679255483
125.47841742911442, 0.2891940840284214
125.47841742911442, 0.19179390639481006
155.49321345532462, 0.1811262983558165
125.47841742911442, 0.23104185974667907
194.06691616088392, 0.24248518110905162
234.0140637738923, 0.29879016064257025
217.93470875985076, 0.2966653151065802
238.03390252740246, 0.24782627011542677
280.2422094392605, 0.2891940840284214
280.2422094392605, 0.19179390639481006
310.5456092734139, 0.1810627861544799
355.27919950478554, 0.2439485827927093
344.5596294954248, 0.29869419987642876
419.6575262087308, 0.24853289030246864
371.6935410816195, 0.2965110924467099
435.00600144940563, 0.2891940840284214
435.00600144940563, 0.19179390639481006
505.35317963583566, 0.1818715631757801
558.5043809322497, 0.24866132264099133
589.7697934595517, 0.2891940840284214
589.7697934595517, 0.19179390639481006
633.8540251230479, 0.29686454793533107
646.0475360086957, 0.30058724408121995
652.3006185141567, 0.18162887721350746
670.6690483739467, 0.2500660816342293
756.9477933849503, 0.2989352307419724
744.5335854696978, 0.2891940840284214
744.5335854696978, 0.2284160242368074
744.5335854696978, 0.19179390639481006
782.0520805024607, 0.29681763378299525
779.7071745629128, 0.3002227177942539
814.8807636561278, 0.18184597363814237
843.0196349306998, 0.23926821758674172
899.2973774798438, 0.2891940840284214
899.2973774798438, 0.19179390639481006
941.5056843917018, 0.223234142865167
980.0115082411166, 0.18067805816157126
1004.1146729776247, 0.29784121528850444
1039.9917338527039, 0.24049002107887654
1054.0611694899899, 0.2891940840284214
1054.0611694899899, 0.19179390639481006
1147.5224205091035, 0.24453120172999693
1147.2711805870094, 0.2985982391102873
1152.547218950992, 0.29666303032643393
1140.6423118732882, 0.1810723470806303
1124.40834767642, 0.24044601482854497
1208.824961500135, 0.2891940840284214
1208.824961500135, 0.19179390639481006
1260.8818733580938, 0.24323207573885286
1309.9490301431288, 0.29821439604572136
1258.067986230636, 0.1803505850324375
1333.104976296162, 0.2964071349500566
1349.519317872995, 0.2324987186508271
1363.588753510281, 0.2891940840284214
1363.588753510281, 0.19179390639481006
1448.0053673339971, 0.22949277880753788
1462.0748029712831, 0.18184597363814237
1518.3525455204272, 0.2891940840284214
1518.3525455204272, 0.19179390639481006
1529.4599947077595, 0.29767398541534557
1546.4914167949992, 0.2522055705338838
1630.9080306187152, 0.181217463259632
1644.9774662560012, 0.23401664349706522
1634.0345718714443, 0.29917044960468653
1673.1163375305732, 0.2891940840284214
1673.1163375305732, 0.19179390639481006
1689.999660295317, 0.2966886198640717
1743.4635157170032, 0.2378725215547505
1796.2238993568262, 0.2992243831093605
1812.4037503397049, 0.1818715631757801
1827.8801295407193, 0.2891940840284214
1827.8801295407193, 0.19179390639481006
1852.181882005123, 0.29687966902575336
1841.9495651780053, 0.22935818448619658
1940.4356146390073, 0.25299942778105433
1968.5744859135793, 0.1818401578341338
1982.6439215508653, 0.2891940840284214
1982.6439215508653, 0.19179390639481006
2003.2790938188846, 0.29833594634950056
2002.341131443065, 0.29642952579548965
2040.0039283797996, 0.2641409424680973
2024.8522284627234, 0.25301687519308
2137.4077135610114, 0.2891940840284214
2138.5801665307845, 0.24521206621357225
2137.4077135610114, 0.19179390639481006
2139.5722421205937, 0.1810627861544799
2189.933606606879, 0.29605271985377407
2123.3382779237254, 0.26424428483163426
2241.521537276927, 0.29833594634950056
2237.0662159917865, 0.23002627497168163
2246.2607155968544, 0.18673408038361278
2292.1715055711556, 0.2891940840284214
2221.8243273847274, 0.2453400139017609
2292.1715055711556, 0.19179390639481006
2299.2062233897996, 0.295794985229379
2310.461771899627, 0.1812712219604916
2348.4492481202997, 0.23926831451680852
2341.4145303016576, 0.29905405274945934
2320.3103768457277, 0.23008225208526417
2411.7617084880876, 0.18675962182061587
2446.9352975813017, 0.2891940840284214
2446.9352975813017, 0.26534347178925494
2446.9352975813017, 0.19165432709860425
2461.0047332185877, 0.18093477643606104
2545.4213470423038, 0.25585207964726037
2573.5602183168758, 0.18697835143994787
2601.699089591448, 0.2891940840284214
2601.699089591448, 0.19179390639481006
2590.876446793536, 0.2984801335519593
2611.439468109569, 0.18109853417615243
2657.0388697647722, 0.2455895118937288
2714.254574689736, 0.300840231555593
2714.254574689736, 0.2964260363130845
2742.393445964308, 0.18697835143994787
2756.462881601594, 0.2891940840284214
2756.462881601594, 0.19179390639481006
2774.552155992391, 0.1812274074932208
2762.715964107054, 0.2463955823293173
2840.87949542531, 0.21925613292330165
2869.018366699882, 0.30054144462465243
2876.834719831708, 0.296774984553599
2911.22667361174, 0.2891940840284214
2911.22667361174, 0.19179390639481006
2939.365544886312, 0.23516817269076307
3032.2238200924003, 0.29898825108124805
3011.1196666364704, 0.2968901374729688
3038.933858627106, 0.24600435766735584
3065.990465621886, 0.2891940840284214
3065.990465621886, 0.19179390639481006
3023.782158710028, 0.23507221192462158
3137.419908088108, 0.22380789429908987
3166.821421022434, 0.29797449413036764
3174.526111966663, 0.2965282282978066
3122.26820817103, 0.24610770003089283
3220.754257632032, 0.2891940840284214
3220.754257632032, 0.19179390639481006
3248.893128906604, 0.23840466762153512
3306.9295509104086, 0.22248935646431883
3336.4362839830483, 0.29905885078776645
3350.5057196203343, 0.29646577764047644
3375.518049642178, 0.2891940840284214
3375.518049642178, 0.19179390639481006
3445.865227828608, 0.22898306512764346
3396.622203098106, 0.2222854398362682
3516.212406015038, 0.2989471873508018
3508.396052883212, 0.29658306302131604
3530.281841652324, 0.2891940840284214
3530.281841652324, 0.19179390639481006
3544.35127728961, 0.24152775437413992
3663.062140479209, 0.29820240094995365
3685.0456336624684, 0.2891940840284214
3685.0456336624684, 0.19179390639481006
3667.4588391158613, 0.24144160777726292
3743.881455418392, 0.2969145638498048
3742.4056404914045, 0.26967714051472164
3839.8094256726145, 0.2891940840284214
3839.8094256726145, 0.23675047987209133
3839.8094256726145, 0.19179390639481006
3811.6705543980424, 0.29639114148903306
3939.3777394134086, 0.21999899004301232
3994.5732176827605, 0.2891940840284214
3994.5732176827605, 0.19179390639481006
4037.8637888744106, 0.2122335495829472
4022.7120889573325, 0.2201023324065493
4149.337009692907, 0.2891940840284214
4149.337009692907, 0.20882258053191793
4149.337009692907, 0.19179390639481006
4121.198138418335, 0.2123295103490887
4246.740794874117, 0.2375967182338823
4304.100801703053, 0.2891940840284214
4304.100801703053, 0.19179390639481006
4346.309108614911, 0.2178603399612436
4457.9852539858675, 0.240673921648131
4458.864593713199, 0.2891940840284214
4458.864593713199, 0.19179390639481006
4571.420078811487, 0.2418854263206673
4613.628385723345, 0.19179390639481006
4646.457068877013, 0.28668844180139363
4741.335570738709, 0.21343674995841358
4747.288024277561, 0.18675517919255374
4768.392177733491, 0.19179390639481006
4782.461613370777, 0.2891940840284214
4838.739355919921, 0.22098342671384844
4909.086534106351, 0.18697302028627336
4923.155969743637, 0.19168049821664285
4937.225405380923, 0.2891940840284214
4937.225405380923, 0.23824764091330358
4923.155969743637, 0.2210619400679642
5077.919761753783, 0.19173284045272002
5070.885043935139, 0.18692443062849695
5091.989197391069, 0.2891940840284214
5021.642019204639, 0.23823891720729073
5179.219698342242, 0.1924464396045722
5183.440529033427, 0.288330437133148
5183.440529033427, 0.2862193002780352
5183.440529033427, 0.28410816342292244
5183.440529033427, 0.2819970265678097
5183.440529033427, 0.27988588971269696
5183.440529033427, 0.2777747528575842
5183.440529033427, 0.2756636160024714
5183.440529033427, 0.2735524791473587
5183.440529033427, 0.2714413422922459
5183.440529033427, 0.26933020543713315
5183.440529033427, 0.2672190685820204
5183.440529033427, 0.26510793172690766
5183.440529033427, 0.2629967948717949
5183.440529033427, 0.2608856580166821
5183.440529033427, 0.25877452116156935
5183.440529033427, 0.25666338430645663
5183.440529033427, 0.25455224745134386
5183.440529033427, 0.2524411105962311
5183.440529033427, 0.2503299737411183
5183.440529033427, 0.24821883688600557
5183.440529033427, 0.24610770003089283
5183.440529033427, 0.24399656317578006
5183.440529033427, 0.2418854263206673
5183.440529033427, 0.23977428946555457
5183.440529033427, 0.2376631526104418
5183.440529033427, 0.23555201575532903
5183.440529033427, 0.23344087890021628
5183.440529033427, 0.23132974204510354
5183.440529033427, 0.22921860518999077
5183.440529033427, 0.227107468334878
5183.440529033427, 0.22499633147976525
5183.440529033427, 0.2228851946246525
5183.440529033427, 0.22077405776953973
5183.440529033427, 0.21866292091442696
5183.440529033427, 0.21655178405931422
5183.440529033427, 0.21444064720420147
5183.440529033427, 0.2123295103490887
5183.440529033427, 0.21021837349397593
5183.440529033427, 0.20810723663886319
5183.440529033427, 0.20599609978375044
5183.440529033427, 0.20388496292863767
5183.440529033427, 0.2017738260735249
5183.440529033427, 0.19966268921841215
5183.440529033427, 0.1975515523632994
5183.440529033427, 0.19544041550818664
5239.718271582571, 0.18692300898751707
') %>%
  dplyr::filter(x > -5000 & x < 5000 & !(x == -1.1465033064596355) & y > .1921 & y < .288)
p2 <- ggplot(tb2, aes(x = x, y = y, group = x > 0)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black') +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  labs(x = 'Regular Earnings - Cutoff',
       y = 'Proportion Claiming Leave Again',
       caption ="Copyright © 1999-2021 John Wiley & Sons, Inc. All rights reserved.\nStyle of both figures modified from original.") + 
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        plot.caption = element_text(size = 9))

plot_grid(p1,p2)
ggsave('bana_outcomes.pdf',width = 7, height = 3.5, units = 'in', device = cairo_pdf)

# Multiple cuoffs
set.seed(1000)
tib <- tibble(x = runif(100)) %>%
  mutate(Treated = case_when(
    x > .6 ~ 'Treated',
    TRUE ~ 'Untreated'
  ))
p1 <- ggplot(tib, aes(x= x, y = 0)) + geom_point() +
  geom_vline(aes(xintercept = .6)) +
  geom_vline(aes(xintercept = .55), linetype = 'dashed') +
  geom_vline(aes(xintercept = .65), linetype = 'dashed') +
  annotate(geom = 'text', x = .8, y = .2, label = 'Treated', 
           family = 'Garamond', size = 14/.pt) +
  annotate(geom = 'text', x = .4, y = .2, label = 'Untreated', 
           family = 'Garamond', size = 14/.pt) +
  scale_y_continuous(limits = c(-.3, .3)) +
  labs(x = 'Running Variable', y ='',
       title = '(a) One Running Variable') +
  scale_x_continuous(breaks = .6, labels = 'Cutoff') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

set.seed(1000)
tib2 <- tibble(x = runif(100),
               x2 = runif(100)) %>%
  mutate(Treated = case_when(
    x > .6 & x2 > .6 ~ 'Treated',
    TRUE ~ 'Untreated'
  ))

p2 <- ggplot(tib2, aes(x= x, y = x2)) + geom_point() +
  geom_vline(aes(xintercept = .6)) +
  geom_hline(aes(yintercept = .6)) +
  geom_segment(aes(x = .55, xend = Inf, y = .55, yend = .55), linetype = 'dashed') +
  geom_segment(aes(x = .65, xend = Inf, y = .65, yend = .65), linetype = 'dashed') +
  geom_segment(aes(x = .55, xend = .55, y = .55, yend = Inf), linetype = 'dashed') +
  geom_segment(aes(x = .65, xend = .65, y = .65, yend = Inf), linetype = 'dashed') +
  annotate(geom = 'text', x = .8, y = .8, label = 'Treated', 
           family = 'Garamond', size = 14/.pt) +
  annotate(geom = 'text', x = .3, y = .3, label = 'Untreated', 
           family = 'Garamond', size = 14/.pt) +
  annotate(geom = 'text', x = .3, y = .8, label = 'Untreated', 
           family = 'Garamond', size = 14/.pt) +
  annotate(geom = 'text', x = .8, y = .3, label = 'Untreated', 
           family = 'Garamond', size = 14/.pt) +
  labs(x = 'Running Variable 1', y =' Running Variable 2',
       title = '(b) Multiple Running Variables') +
  scale_x_continuous(breaks = .6, labels = 'Cutoff') +
  scale_y_continuous(breaks = .6, labels = 'Cutoff') +
  theme_pubr() +
  theme(text = element_text(family = 'Garamond', size = 14),
        plot.title.position = 'plot')

plot_grid(p1,p2)

ggsave('multi_running.pdf',width = 7, height = 3.5, units = 'in', device = cairo_pdf)
