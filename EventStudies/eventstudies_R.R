{
  library(tidyverse)
  library(extrafont)
  library(ggpubr)
  library(cowplot)
  library(lubridate)
  library(tidyquant)
}

d <- list()
set.seed(1000)
d[[1]] <- tibble(Date = ymd('2018-01-01') + days(0:199),
                 y = c(runif(150,0,.5), runif(50,0,.5) + 1/sqrt(1:50)),
                 yalt = c(rep(NA_real_,150),runif(50,0,.5))) %>%
  pivot_longer(cols = c('y','yalt')) %>%
  mutate(name = factor(case_when(
    name == 'y' ~ 'What We See',
    name == 'yalt' ~ 'What We Expect'
  ), levels = c('What We See','What We Expect')))

p1 <- ggplot(d[[1]], aes(x = Date, y = value, color = name)) + 
  geom_line() + 
  geom_vline(aes(xintercept = ymd('2018-05-30')), linetype = 'dashed') +
  scale_color_manual(values = c('black','gray')) +
  theme_pubr() + 
  labs(y = 'Outcome', color = NULL,
       title = '(a) Flat Beforehand') + 
  theme(text = element_text(family = 'Garamond', size = 13),
        legend.position = c(.3, .8),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 14))

d[[2]] <- tibble(Date = ymd('2018-01-01') + days(0:199),
             y = c(rnorm(150,0,.5) + (1:150)/75, rnorm(50,0,.5) + (151:200)/75 - 1),
             yalt = c(rep(NA_real_,150),rnorm(50,0,.5) + (151:200)/75)) %>%
  pivot_longer(cols = c('y','yalt')) %>%
  mutate(name = factor(case_when(
    name == 'y' ~ 'What We See',
    name == 'yalt' ~ 'What We Expect'
  ), levels = c('What We See','What We Expect')))

p2 <- ggplot(d[[2]], aes(x = Date, y = value, color = name)) + 
  geom_line() + 
  geom_vline(aes(xintercept = ymd('2018-05-30')), linetype = 'dashed') +
  scale_color_manual(values = c('black','gray')) +
  theme_pubr() + 
  guides(color = FALSE) +
  labs(y = 'Outcome',
       title = '(b) Prior Trend') + 
  theme(text = element_text(family = 'Garamond', size = 13),
        legend.position = c(.2, .8),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 14))

plot_grid(p1,p2)
ggsave('event_study_examples.pdf', width = 8, height = 3.5,device=cairo_pdf)


# Google Analysis
getSymbols("GOOGL", from = '2015-05-01',
           to = "2013-09-01",warnings = FALSE,
           auto.assign = TRUE)
sp500 <- read_csv('SP500_Historical.csv') %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date) %>%
  mutate(SP500_Return = `Close/Last`/lag(`Close/Last`) - 1)
both <- GOOG %>%
  as_tibble() %>%
  mutate(Date = index(GOOG)) %>%
  mutate(Google_Return = GOOG.Close/lag(GOOG.Close) - 1) %>%
  select(Date, Google_Return) %>%
  inner_join(sp500 %>% select(Date, SP500_Return)) %>%
  slice(-1)

write_csv(both, file = 'google_stock_data.csv')

library(tidyverse); library(lubridate)

goog <- read_csv('google_stock_data.csv')

event <- ymd("2015-08-10")

# Create estimation data set
est_data <- goog %>%
  filter(Date >= ymd('2015-05-01') &
           Date <= ymd('2015-07-31'))

# And observation data
obs_data <- goog %>%
  filter(Date >= event - days(4) & 
           Date <= event + days(14))

# Estimate a model predicting stock price with market return
m <- lm(Google_Return ~ SP500_Return, data = est_data)

# Get AR
obs_data <- obs_data %>%
  # Using mean of estimation return
  mutate(AR_mean = Google_Return - mean(est_data$Google_Return),
         # Then comparing to market return
         AR_market = Google_Return - SP500_Return,
         # Then using model fit with estimation data
         risk_predict = predict(m, newdata = obs_data),
         AR_risk = Google_Return - risk_predict)

# Graph the results
ggplot(obs_data, aes(x = Date, y = AR_risk)) + 
  geom_line() + 
  geom_vline(aes(xintercept = event), linetype = 'dashed') + 
  geom_hline(aes(yintercept = 0))

rdata <- goog %>%
  rename(Google = Google_Return, `S&P 500` = SP500_Return) %>%
  pivot_longer(cols = c('Google','S&P 500'))
ggplot(rdata, aes(x = Date, y = value, linetype = name)) + 
  geom_line() +
  geom_text(data = rdata %>% filter(Date == max(Date)),
            mapping = aes(x = Date, y = value, label = name),
            hjust = -.2,
            family = 'Garamond', size = 14/.pt) +
  geom_vline(aes(xintercept = event), linetype = 'dashed') + 
  geom_hline(aes(yintercept = 0)) + 
  guides(linetype = FALSE) + 
  scale_x_date(limits = c(min(rdata$Date),max(rdata$Date) + days(18))) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = 'Date', y = 'Abnormal Return') + 
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('google_sp500_data.pdf', width = 6, height = 5,device=cairo_pdf)  


gdata <- obs_data %>%
  rename(Mean = AR_mean, Market = AR_market, Risk = AR_risk) %>%
  pivot_longer(cols = c('Mean','Market','Risk'))
ggplot(gdata, aes(x = Date, y = value, linetype = name)) + 
  geom_line() + 
  geom_text(data = gdata %>% filter(Date == max(Date)),
            mapping = aes(x = Date, y = value, label = name),
            hjust = -.2,
            family = 'Garamond', size = 14/.pt) +
  geom_vline(aes(xintercept = event), linetype = 'dashed') + 
  geom_hline(aes(yintercept = 0)) + 
  guides(linetype = FALSE) + 
  scale_x_date(limits = c(min(gdata$Date),max(gdata$Date) + days(3))) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = 'Date', y = 'Abnormal Return') + 
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('google_stock_event.pdf', width = 6, height = 5,device=cairo_pdf)

goog <- goog %>%
  mutate(AR_risk = Google_Return - predict(m, newdata = goog))
sd(goog$AR_risk)

# Ambulance study
hosp <- read.csv(text = 'X, Y
                 0.9411764705882355, 0.48863636363636365
1.8235294117647047, 0.4545454545454546
2.882352941176471, 0.4386363636363636
3.9411764705882355, 0.38181818181818183
5, 0.42272727272727273
5.882352941176469, 0.4113636363636364
6.9411764705882355, 0.15227272727272734
8, 0.38636363636363646
8.882352941176471, 0.34090909090909094
9.941176470588236, 0.42727272727272736
10.823529411764707, 0.4136363636363637
11.882352941176471, 0.3772727272727273
12.941176470588236, 0.35
14, 0.49090909090909096
14.882352941176471, 0.44090909090909103
15.941176470588236, 0.3568181818181818
17.000000000000004, 0.5318181818181817
18.058823529411768, 0.4931818181818183
19.117647058823533, 0.44999999999999996
19.823529411764707, 0.4568181818181818
21.058823529411764, 0.44772727272727275
21.941176470588236, 0.5181818181818182
23.000000000000004, 0.4181818181818182
24.058823529411764, 0.4
24.941176470588243, 0.415909090909091
25.823529411764707, 0.36363636363636365
27.05882352941177, 0.44090909090909103
27.941176470588236, 0.38636363636363646
29.000000000000004, 0.4113636363636364
30.058823529411764, 0.49090909090909096
31.117647058823533, 0.3318181818181819
32, 0.3772727272727273
32.88235294117648, 0.44772727272727275
33.94117647058823, 0.49090909090909096
35, 0.4363636363636364
36.05882352941177, 0.3545454545454545
37.117647058823536, 0.42499999999999993
38, 0.4795454545454546
38.882352941176464, 0.46818181818181825
39.94117647058823, 0.42954545454545456
41, 0.5227272727272727
41.88235294117648, 0.4977272727272727
43.117647058823536, 0.4772727272727273
44, 0.5545454545454545
44.88235294117648, 0.5636363636363637
45.941176470588246, 0.55
47, 0.5340909090909091
48.05882352941177, 0.5590909090909091
48.941176470588246, 0.5977272727272728
50.176470588235304, 0.5568181818181819
50.882352941176464, 0.4136363636363637
51.94117647058823, 0.4636363636363636
53, 0.6136363636363636
54.05882352941177, 0.625
55.11764705882352, 0.5545454545454545
56, 0.6022727272727273
57.05882352941177, 0.515909090909091
58.11764705882352, 0.5840909090909091
59, 0.5136363636363637
60.05882352941177, 0.6204545454545455
61.117647058823536, 0.5636363636363637
62, 0.6590909090909092
63.05882352941177, 0.6136363636363636
64.11764705882354, 0.5636363636363637
65, 0.5977272727272728
66.05882352941177, 0.5863636363636364
67.11764705882354, 0.5818181818181818
68.1764705882353, 0.5681818181818181
69.05882352941177, 0.5363636363636364
69.94117647058823, 0.5545454545454545
71, 0.6227272727272728
72.05882352941177, 0.5636363636363637
73.11764705882354, 0.5613636363636363
74, 0.6204545454545455
75.05882352941177, 0.6386363636363637
75.94117647058825, 0.6000000000000001
77, 0.625
78.05882352941177, 0.6590909090909092
79.11764705882354, 0.6272727272727272
80.1764705882353, 0.665909090909091
80.88235294117646, 0.6090909090909091
82.11764705882354, 0.5863636363636364
83, 0.6045454545454545
84.23529411764707, 0.6090909090909091
85.11764705882354, 0.5954545454545455
86.1764705882353, 0.5954545454545455
87.05882352941177, 0.675
88.11764705882354, 0.6454545454545455
89, 0.6590909090909092
89.88235294117646, 0.6795454545454545
90.94117647058825, 0.65
92.17647058823529, 0.6227272727272728
93.23529411764706, 0.6022727272727273
94.11764705882352, 0.6363636363636364
95.00000000000001, 0.6318181818181818
96.05882352941177, 0.6840909090909091
96.94117647058825, 0.6363636363636364
98, 0.6954545454545455
99.05882352941178, 0.6863636363636363
100.11764705882354, 0.6409090909090909
101.00000000000001, 0.6909090909090909
102.23529411764706, 0.7159090909090909
103.11764705882352, 0.6681818181818182
104, 0.7
105.23529411764706, 0.7
106.11764705882354, 0.7568181818181818
107.17647058823529, 0.6363636363636364
108.05882352941177, 0.6977272727272728
109.11764705882352, 0.7318181818181818
110.1764705882353, 0.6704545454545454
111.05882352941177, 0.7318181818181818
111.94117647058825, 0.6795454545454545')
hosp <- hosp %>%
  mutate(X = 1:112) %>%
  mutate(After = X >= 27) %>%
  rename(Weeks = X, AMI = Y) %>%
  mutate(Weeks_Centered = Weeks - 27)

ggplot(hosp, aes(x = Weeks, y = AMI)) + 
  geom_line(alpha = .5, color = 'gray') + geom_point() + 
  geom_smooth(mapping = aes(group = After), method = 'lm', color = 'black', se = FALSE) + 
  geom_vline(aes(xintercept = 27), linetype = 'dashed') + 
  scale_y_continuous(limits = c(0,1)) +
  labs(y = 'AMI Performance') + 
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('ambulances.pdf', width = 6, height = 5,device=cairo_pdf)


library(tidyverse); library(fixest)
set.seed(10)

# Create data with 10 groups and 10 time periods
df <- crossing(id = 1:10, t = 1:10) %>%
  # And an event in period 6 with a one-period positive effect
  mutate(Y = rnorm(n()) + 1*(t == 6))

# Use i() in feols to include time dummies,
# specifying that we want to drop t = 5 as the reference
# and no intercept (-1)
m <- feols(Y ~ -1 + i(t, ref = 5), data = df,
           cluster = 'id')

d <- broom::tidy(m) %>%
  mutate(ci.bot = estimate - std.error,
         ci.top = estimate + std.error) %>%
  mutate(t = c(5,1:4,6:10))
d[1,] <- list('t::5', 0, 0, 0, 0,0,0,5)
d %>%
  arrange(t) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymin = ci.bot, ymax = ci.top), width = .2) + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 5), linetype = 'dashed') + 
  scale_x_continuous(breaks = c(1,3,5,7,9)) +
  scale_y_continuous(breaks = c(-.5, 0, .5, 1, 1.5)) +
  labs(x = 't', y = 'Estimate') + 
  theme_pubr() + 
  theme(text= element_text(family = 'Garamond', size = 13))
ggsave('period_regression.pdf', width = 6, height = 5,device=cairo_pdf)

# ARMA data
d <- data.frame(t = 1:100, Y = rnorm(100)) %>%
  mutate(Y_AR  = Y,
         Y_MA = Y,
         Y_ARMA = Y)
for (i in 2:100) {
  d[i, 'Y_AR'] <- d[i,'Y_AR'] + .8*d[i-1,'Y_AR']
  d[i, 'Y_MA'] <- d[i, 'Y_MA'] + .9*d[i-1, 'Y']
  d[i, 'Y_ARMA'] <- d[i, 'Y_ARMA']+ .8*d[i-1,'Y_ARMA'] + .9*d[i-1, 'Y']
}

names <- c('(a) Totally Random', '(b) MA(1) Process', '(c) AR(1) Process', '(d) ARMA(1,1) Process')
vars <- c('Y','Y_MA','Y_AR','Y_ARMA')
p <- list()
for (i in 1:4) {
  p[[i]] <- ggplot(d, aes_string(x = 't', y = vars[i])) + 
    geom_line() + 
    labs(x = 'Time', y = 'Value', title= names[i]) + 
    theme_pubr() + 
    theme(text= element_text(family = 'Garamond', size = 13))
}
plot_grid(p[[1]],p[[2]],p[[3]],p[[4]], ncol = 2)
ggsave('different_processes.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)
