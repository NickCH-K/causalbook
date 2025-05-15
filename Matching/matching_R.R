{
  library(tidyverse)
  library(extrafont)
  library(modelsummary)
  library(ggpubr)
  library(gghighlight)
  library(cowplot)
  library(Matching)
  library(vtable)
  library(haven)
}

## UCI Credit card example
uci <- read_csv('UCI_Credit_Card.csv') %>%
  mutate(LateSept = PAY_0 > 0,
         LateApril = PAY_6 > 0,
         BillApril = BILL_AMT6/1000) %>%
  dplyr::select(LateSept,LateApril,BillApril, AGE)


uci[10305,]
uci %>%
  mutate(id = row_number()) %>%
  mutate(BillApril = BillApril*1000) %>%
  filter(!LateApril) %>%
  mutate(dif = abs(BillApril - 91630)) %>%
  arrange(dif) %>%
  slice(1:2)

m <- glm(LateApril ~ BillApril, data = uci, family = binomial(link = 'logit'))
msummary(m, stars = TRUE, , gof_omit = 'AIC|BIC|F|Lik')
ind <- coef(m)[1] + coef(m)[2]*91.630
pv <- exp(ind)/(1+exp(ind))
uci <- uci %>%
  mutate(ind = coef(m)[1] + coef(m)[2]*BillApril) %>%
  mutate(pred = exp(ind)/(1+exp(ind)),
         pred2 = predict(m, type = 'response'))
uci %>%
  mutate(id = row_number()) %>%
  filter(!LateApril) %>%
  mutate(dif = abs(pred - pv)) %>%
  arrange(dif) %>%
  slice(1)

sd(uci$BillApril)
uci[27281,]
uci[27719,]

uci[10305,]
quantile(uci$BillApril, (0:10)/10)
uci[27281,]
sd(uci$BillApril*1000)
sd(uci$AGE)
cv <- cov(uci %>% select(BillApril, AGE) %>% mutate(BillApril = BillApril*1000))
x <- uci %>% select(BillApril, AGE) %>% mutate(BillApril = BillApril*1000) %>% slice(10305,27281) %>% as.matrix()
x/sd(uci$BillApril*1000)
x <- x[2,] - x[1,]
x/sd(uci$BillApril*1000)
x/sd(uci$AGE)
t(x) %*% solve(cv) * x
sqrt(t(x) %*% solve(cv) %*% x)

# Graph for subset comparison
epan <- function(x, r=1) (3/4)*(1-r*(x^2)) %>% purrr::map(function(x) max(x,0)) %>% unlist()

set.seed(1000)
sub <- uci %>%
  mutate(id = row_number()) %>%
  mutate(BillApril = BillApril*1000) %>%
  filter(abs(BillApril - uci$BillApril[10305]*1000) < 200) %>%
  mutate(yjit = jitter(as.numeric(LateSept), amount = .5)) %>%
  # Standardize after filtering for better visual
  mutate(BA_std = (BillApril - mean(BillApril))/sd(BillApril)) %>%
  mutate(blank = '')
refba <- sub %>% filter(id == 10305) %>% pull(BA_std)
sub <- sub %>%
  mutate(wt = epan(BA_std - refba, r = 1))


ggplot(sub, aes(x = BillApril, y = yjit, shape = LateApril)) + 
  geom_point() + 
  geom_hline(aes(yintercept = .5), linetype = 'dashed') +
  geom_text(aes(x = BillApril, y = yjit, label = scales::number(id, big.mark = ',')), data = sub %>% filter(id %in% c(10305, 27281, 27719)),
            family = 'Garamond',
            size = 13/.pt, 
            hjust = -.2) +
  labs(x = 'Bill in April',shape = 'Late in April') +
  scale_shape_manual(values = c(1,19), labels = c('Not Late\nin April','Late in\nApril')) + 
  scale_y_continuous(breaks = c(0,1), labels = c('Not Late in\nSeptember','Late in\nSeptember')) + 
  scale_x_continuous(labels = function(x) paste0('NT$',scales::number(x, accuracy = 1, big.mark = ',')))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(.7, .8),
        legend.background = element_rect(color = 'black'))
ggsave('matched_values.pdf', width = 6, height = 5,device=cairo_pdf)


p1 <- ggplot(sub, aes(x = BillApril, y = yjit, shape = LateApril)) + 
  geom_point() + 
  gghighlight(id %in% c(10305, 27281), label_key = blank) +
  geom_hline(aes(yintercept = .5), linetype = 'dashed') +
  geom_text(aes(x = BillApril, y = yjit, label = scales::number(id, big.mark = ',')), data = sub %>% filter(id %in% c(10305, 27281)),
            family = 'Garamond',
            size = 13/.pt, 
            hjust = -.2) +
  labs(x = 'Bill in April',shape = 'Late in April',
       title = '(a) One-to-one Matching') +
  scale_shape_manual(values = c(1,19), labels = c('Not Late\nin April','Late in\nApril')) + 
  scale_y_continuous(breaks = c(0,1), labels = c('Not Late in\nSeptember','Late in\nSeptember')) + 
  scale_x_continuous(labels = function(x) paste0('NT$',scales::number(x, accuracy = 1, big.mark = ',')),
                     breaks = c(91600,91800))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(.7, .8),
        legend.background = element_rect(color = 'black'))

p2 <- ggplot(sub %>% filter(!LateApril | id == 10305), aes(x = BillApril, y = yjit, shape = LateApril, size = wt)) + 
  geom_point() +
  gghighlight(wt > 0, label_key = blank) +
  geom_hline(aes(yintercept = .5), linetype = 'dashed') +
  geom_text(aes(x = BillApril, y = yjit, label = scales::number(id, big.mark = ',')), data = sub %>% filter(id %in% c(10305)),
            family = 'Garamond',
            size = 13/.pt, 
            hjust = -.4) +
  labs(x = 'Bill in April',shape = 'Late in April',
       title = '(b) Kernel Matching') +
  guides(size = 'none',
         shape = 'none') +
  scale_shape_manual(values = c(1,19), labels = c('Not Late\nin April','Late in\nApril')) + 
  scale_y_continuous(breaks = c(0,1), labels = c('Not Late in\nSeptember','Late in\nSeptember')) + 
  scale_x_continuous(labels = function(x) paste0('NT$',scales::number(x, accuracy = 1, big.mark = ',')),
                     breaks = c(91600,91800))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
plot_grid(p1,p2)
ggsave('select_vs_kernel.pdf', width = 7.5, height = 3.5,device=cairo_pdf)


sub2 <- uci %>%
  mutate(id = row_number()) %>%
  mutate(BA_std = (BillApril-mean(BillApril))/sd(BillApril),
         Age_std = (AGE - mean(AGE))/sd(AGE)) %>%
  mutate(blank = '')
sub2 <- sub2 %>%
  arrange(abs(BA_std - sub2$BA_std[10305])) %>%
  slice(1:200) %>%
  arrange(abs(Age_std - sub2$Age_std[10305])) %>%
  slice(1:100) %>%
  arrange(-(id %in% c(11874, 21733))) %>%
  slice(1:20)
pts <- sub2 %>%
  filter(id %in% c(21733, 11874))
pts %>%
  dplyr::select(BA_std, Age_std) %>%
  summarize(BA = BA_std - first(BA_std), AS = Age_std - first(Age_std))
ggplot(sub2, aes(x = BA_std, y = Age_std, shape = LateApril, size = id %in% c(21733, 11874))) + 
  geom_point() +
  gghighlight(id %in% c(21733, 11874), label_key = blank) +
  scale_shape_manual(values = c(1,19), labels = c('Not Late\nin April','Late in\nApril')) +
  scale_size_manual(values = c(1, 3)) +
  annotate(geom = 'segment', x = pts$BA_std[2], xend = pts$BA_std[2], y = pts$Age_std[1], yend = pts$Age_std[2]) +
  annotate(geom = 'segment', x = pts$BA_std[1], xend = pts$BA_std[2], y = pts$Age_std[1], yend = pts$Age_std[1]) +
  annotate(geom = 'segment', x = pts$BA_std[1], xend = pts$BA_std[2], y = pts$Age_std[1], yend = pts$Age_std[2]) +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt, angle = 90,
           x = pts$BA_std[2], y = mean(pts$Age_std), label = 'Age Difference', vjust = -.4) +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = mean(pts$BA_std), y = pts$Age_std[1], label = 'April Bill Difference', vjust = 1.4) +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt, angle = -45,
           x = pts$BA_std[2], y = pts$Age_std[2], label = 'Mahalanobis', vjust = -.35, hjust = -1) +
  guides(size = FALSE) +
  expand_limits(x = .91, y = -.6) +
  labs(x = 'April Bill, Standardized',
       y = 'Age, Standardized') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        legend.position = c(.9, .2),
        legend.background = element_rect(color = 'black'))
ggsave('mahalanobis.pdf', width = 6, height = 5,device=cairo_pdf)


# Epan kernel
ggplot(data.frame(x = (-150:150)/100), aes(x = x)) + 
  geom_function(fun = epan) + 
  labs(x = 'Difference', y = 'Kernel Weight')  +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"))
ggsave('epan.pdf', width = 6, height = 3,device=cairo_pdf)


# Bad matching distribution
set.seed(1000)
tib <- tibble(x = rnorm(1000))
p1 <- tib %>%
  ggplot(aes(x = x)) + geom_density() + 
  geom_vline(aes(xintercept = mean(x)), linetype = 'dashed') +
  annotate(x = mean(tib$x), y = .2, geom = 'label', family = 'Garamond',
           size = 13/.pt, label = paste0('Mean ',scales::number(mean(tib$x), accuracy = .01))) +
  labs(x = 'Matching Variable X',
       y = 'Density',
       title = '(a) Distribution of X in Treated Group') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
set.seed(1000)
tib2 <- tibble(x = c(rnorm(500,-2),rnorm(500,2)))
p2 <- tib2 %>%
  ggplot(aes(x = x)) + geom_density() + 
  geom_vline(aes(xintercept = mean(x)), linetype = 'dashed') +
  annotate(x = mean(tib$x), y = .05, geom = 'label', family = 'Garamond',
           size = 13/.pt, label = paste0('Mean ',scales::number(mean(tib$x), accuracy = .01))) +
  labs(x = 'Matching Variable X',
       y = 'Density',
       title = '(b) Distribution of X in Control Group') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
plot_grid(p1,p2)
ggsave('bad_balance_distribution.pdf', width = 8, height = 3.5,device=cairo_pdf)

tib3 <- tib %>%
  mutate(Group = 'Treated') %>%
  bind_rows(tib2 %>% mutate(Group = 'Control'))
ggplot(tib3, aes(x=x, linetype = Group, color = Group)) +  geom_density(size = 1) + 
  guides(linetype = FALSE, color = FALSE) + 
  scale_linetype_manual(values = c('solid','dashed')) +
  scale_color_manual(values = c('darkgray','black')) +
  annotate(x = 1.4, y = .3, geom = 'text', family = 'Garamond',
           size = 13/.pt, label = 'Treated') +
  annotate(x = 2.5, y = .2, geom = 'text', family = 'Garamond',
           size = 13/.pt, label = 'Control', color = 'darkgray') +
  labs(x = 'Matching Variable X',
       y = 'Density') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
ggsave('overlaid_balance.pdf', width = 6, height = 5,device=cairo_pdf)


## BROOCKMAN STUFF

br <- read_dta('broockman2013.dta')

m <- glm(leg_black ~ medianhhincom + blackpercent + leg_democrat, data = br, family = binomial(link = 'logit'))

br <- br %>%
  mutate(ps = predict(m, type = 'response')) %>%
  mutate(ipw = case_when(
    leg_black == 1 ~ 1/ps,
    leg_black == 0 ~ 1/(1-ps)
  ))

# Common support
p1 <- ggplot(br, aes(x = blackpercent, linetype = factor(leg_black))) + 
  geom_density(size = 1) + 
  guides(linetype = FALSE) + 
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .3, y = 15, label = 'Non-Black Legislator') +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .6, y = 4, label = 'Black Legislator') +
  labs(x = 'Percentage Black in District',
       y = 'Density',
       title = '(a) Percentage Black Common Support') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
p2 <- ggplot(br, aes(x = ps, linetype = factor(leg_black))) + 
  geom_density(size = 1) + 
  guides(linetype = FALSE) + 
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .3, y = 40, label = 'Non-Black Legislator') +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .6, y = 6, label = 'Black Legislator') +
  labs(x = 'Propensity Score',
       y = 'Density',
       title = '(b) Propensity Score Common Support') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
plot_grid(p1,p2)
ggsave('common_support.pdf', width = 8, height = 3.5,device=cairo_pdf)

br <- br %>%
  mutate(cutps = cut(ps, breaks = (0:50)/50))
table(br %>% filter(leg_black==0) %>% pull(cutps))

ggplot(br %>% filter(ps <= .24 ), aes(x = ps, linetype = factor(leg_black))) + 
  geom_density(size = 1) + 
  guides(linetype = FALSE) + 
  expand_limits(x = 1) +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .18, y = 50, label = 'Non-Black Legislator') +
  annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .2, y = 10, label = 'Black Legislator') +
  annotate(geom = 'rect', xmin = .24, xmax = 1, ymin = -1, ymax = 1, 
           color = 'white', fill = 'white') +
  labs(x = 'Propensity Score',
       y = 'Density') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
ggsave('trimmed_common_support.pdf', width = 6, height = 5,device=cairo_pdf)

p1 <- ggplot(br %>% filter(ps >= .02 & ps < .98), aes(x = blackpercent, linetype = factor(leg_black), weight = ipw)) + 
  geom_density(size = 1) + 
  guides(linetype = 'none') + 
  ggplot2::annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .2, y = 1.3, label = 'Non-Black') +
  ggplot2::annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .38, y = 3, label = 'Black Legislator') +
  labs(x = 'Propensity Score',
       y = 'Density',
       title = '(a) Percentage Black after Weighting') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
p2 <- ggplot(br %>% filter(ps >= .02 & ps < .98), aes(x = ps, linetype = factor(leg_black), weight = ipw)) + 
  geom_density(size = 1) + 
  guides(linetype = 'none') + 
  ggplot2::annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .3, y = 5, label = 'Non-Black Legislator') +
  ggplot2::annotate(geom = 'text', family = 'Garamond', size = 13/.pt,
           x = .35, y = 2, label = 'Black Legislator') +
  labs(x = 'Propensity Score',
       y = 'Density',
       title = '(b) Propensity Score After Trimming\nand Inverse Probability Weighting') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"))
plot_grid(p1,p2)
ggsave('ps_after_weight.pdf', width = 8, height = 3.5,device=cairo_pdf)

# Outcome
Y <- br %>%
  pull(responded)
# Treatment
D <- br %>%
  pull(leg_black)
# Matching variables
X <- br %>%
  dplyr::select(medianhhincom, blackpercent, leg_democrat) %>%
  as.matrix()

# Weight = 2, oddly, denotes Mahalanobis distance
M <- Match(Y, D, X, Weight = 2,
           caliper = 1)

btab <- MatchBalance(leg_black ~ medianhhincom + blackpercent + leg_democrat,
             data = br,
             match.out = M,
             print.level = 1)
bt <- read_csv('balance_table_info.csv') %>%
  rename(` ` = X1)
dftoLaTeX(bt, file = 'balance_table.tex', title = 'Broockman (2013) Balance Table Before and After Mahalanobis Matching',
          anchor = 'tab:matching-balance-table')
