{
  library(tidyverse)
  library(vtable)
  library(purrr)
  library(cowplot)
  library(Cairo)
  library(extrafont)
  library(haven)
  library(mvtnorm)
  library(ggpubr)
  library(modelsummary)
}

oster <- read_dta('nhanes_summary_cleaned.dta') %>%
  mutate(supplement_vite_single = case_when(
    !supplement_vite_single ~ 'No Vitamin E',
    TRUE ~ 'Took Vitamin E'
  ))

ggplot(oster %>% slice(150:300), aes(x= age, y = heart_health)) + 
  geom_point() + 
  labs(x = 'Age',
       y = 'Heart Health Score') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('scatterplot.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


ggplot(oster, aes(x = vite, linetype = factor(vigorous_exercise_month))) + 
  geom_density(size = 1.5) +
  scale_x_log10() +
  labs(x = 'Vitamin E Taken (Log Scale)',
       y = 'Density') + 
  annotate(geom = 'label', x = 3.5, y = .75, label = 'No Vigorous Exercise\nLast Month', hjust = 1, family = 'Garamond', size = 13/.pt) + 
  annotate(geom = 'label', x = 15, y = .75, label = 'Vigorous Exercise\nLast Month', hjust = 0, family = 'Garamond', size = 13/.pt) + 
  guides(linetype = FALSE) +
  theme_pubr() + 
    theme(text         = element_text(size = 13, family="Garamond"),
          axis.title.x = element_text(size = 13, family="Garamond"),
          axis.title.y = element_text(size = 13, family= "Garamond"),
          panel.grid = element_blank())
ggsave('exercise_vite.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

ggplot(oster %>% 
         select(smoke,supplement_vite_single) %>%
         na.omit() %>%
         mutate(smoke = as.factor(
           case_when(smoke == 0 ~ 'No Smoking',
                     TRUE ~ 'Smoking')
         )) %>%
         group_by(smoke) %>%
         mutate(BigN = n()) %>%
         group_by(smoke, supplement_vite_single) %>% 
         summarize(N = n()/first(BigN)) %>%
         group_by(smoke, supplement_vite_single) %>%
         mutate(sup_label = case_when(
           row_number() == 1 ~ supplement_vite_single,
           TRUE ~ NA_character_
         )), 
       aes(x = smoke, y = N, group = supplement_vite_single)) + 
  geom_col(position = 'dodge', fill = 'white', color = 'black') + 
  geom_text(aes(label = sup_label, y = N + .05),position = position_dodge(0.9), family = 'Garamond', size = 14/.pt) +
  guides(fill = FALSE) +
  labs(x = NULL, y = 'Proportion') +
  scale_fill_manual(values = c('gray','black'))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 16, family = 'Garamond'),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('smoking_vite.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


oster %>%
  filter(year < 2015 & !is.na(year)) %>%
  arrange(year) %>%
  mutate(time = fct_inorder(case_when(
    year <= 1995 ~ 'Before Recommendation',
    year <= 2004 ~ 'During Recommendation',
    TRUE ~ 'After Recommendation'
  ))) %>%
  group_by(time) %>%
  summarize(vite = mean(supplement_vite_single == 'Took Vitamin E')) %>%
  ggplot(aes(x = time, y = vite)) + 
  geom_col(fill = 'white', color = 'black') + 
  geom_text(aes(label = time, y = vite + .005), family = 'Garamond', size = 16/.pt) +
  labs(x = 'Year', y = 'Proportion Taking Vitamin E') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('rec_vite.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

oster %>%
  filter(bmi < 100) %>%
  mutate(bmi_cut = cut(bmi, 8)) %>%
  group_by(bmi_cut) %>%
  summarize(vite = mean(supplement_vite_single == 'Took Vitamin E')) %>%
  mutate(vite = scales::number(vite, accuracy = .001)) %>%
  rename(`BMI Bin` = bmi_cut,
         `Proportion Taking Vitamin E` = vite) %>%
  dftoLaTeX(anchor = 'tab:describingrelationships-cut', file = 'vite_by_cut_bmi.tex', title = 'Proportion Taking Vitamin E by Range of Body Mass Index Values')

oster %>%
  filter(bmi < 100) %>%
  mutate(bmi_cut = cut(bmi, 8)) %>%
  group_by(bmi_cut) %>%
  summarize(vite = mean(supplement_vite_single == 'Took Vitamin E'), bmi = min(bmi)) %>%
  ggplot(aes(x = bmi, y = vite)) + 
  geom_step(color = 'black') + 
  #geom_text(aes(label = bmi_cut, y = vite + .01), family = 'Garamond', size = 14/.pt) +
  labs(x = 'Body Mass Index', y = 'Proportion Taking Vitamin E') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('vite_by_cut_bmi.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

oster %>%
  filter(bmi < 100) %>%
  mutate(vite = (supplement_vite_single == 'Took Vitamin E')*1) %>%
  ggplot(aes(x = bmi, y = vite)) + 
  geom_smooth(color = 'black', se = FALSE) +
  #geom_text(aes(label = bmi_cut, y = vite + .01), family = 'Garamond', size = 14/.pt) +
  labs(x = 'Body Mass Index', y = 'Proportion Taking Vitamin E') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('vite_by_bmi_loess.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

oster %>%
  filter(bmi < 100) %>%
  mutate(vite = (supplement_vite_single == 'Took Vitamin E')*1) %>%
  ggplot(aes(x = bmi, y = vite)) + 
  geom_smooth(method = 'lm', color = 'black', se = FALSE) +
  #geom_text(aes(label = bmi_cut, y = vite + .01), family = 'Garamond', size = 14/.pt) +
  labs(x = 'Body Mass Index', y = 'Proportion Taking Vitamin E') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('vite_by_bmi_lm.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)
lm((supplement_vite_single == 'Took Vitamin E')~bmi, data = oster %>% filter(bmi < 100))
sd(oster$supplement_vite_single == 'Took Vitamin E', na.rm = TRUE)
sd(oster$bmi, na.rm = TRUE)

tb2 <- tibble(X = c(1,3,5,6.5), Y = c(2,6, 3.6, 8)) %>%
  mutate(line1 = 1.5 + X,
         line2 = 2 + .6*X,
         line3 = 1.7324 + .8175*X)
p1 <- ggplot(tb2, aes(x = X, y = Y)) + 
  geom_point(size = 4) + 
  expand_limits(x = c(0,7), y = c(1,9))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        plot.title.position = 'plot') + 
  coord_fixed() +  
  labs(title = 'Let\'s fit a line to four points')
p2 <- ggplot(tb2, aes(x = X, y = Y)) + 
  geom_point(size = 4) + 
  expand_limits(x = c(0,7), y = c(1,9))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        plot.title.position = 'plot') + 
  coord_fixed() + 
  geom_line(aes(x = X, y = line3), size = 1.5) + 
  labs(title = 'Add the OLS line')
p3 <- ggplot(tb2, aes(x = X, y = Y)) + 
  geom_point(size = 4) + 
  expand_limits(x = c(0,7), y = c(1,9))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        plot.title.position = 'plot') + 
  coord_fixed() + 
  geom_segment(aes(x = X, xend = X, y = Y, yend = line3), size = 1.5) + 
  geom_line(aes(x = X, y = line3), size = 1.5) + 
  labs(title = 'Residuals are from point to line')
p4 <- ggplot(tb2, aes(x = X, y = Y)) + 
  geom_point(size = 4) + 
  expand_limits(x = c(0,7), y = c(1,9))+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        plot.title.position = 'plot') +
  geom_segment(aes(x = X, xend = X, y = Y, yend = line3), size = 1.5) + 
  geom_rect(aes(xmin = X, xmax = X + abs(line3 - Y), ymin = Y, ymax = line3), alpha = .5, fill = 'gray') + 
  geom_line(aes(x = X, y = line3), size = 1.5) +
  labs(title = 'Goal: minimize squared residuals')+
  coord_fixed()
plot_grid(p1,p2,p3,p4, nrow = 2)
ggsave('ols_fit.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


set.seed(1234)
tib <- data.frame(x = runif(20), eps = rnorm(20)) %>%
  mutate(y = .5*x + eps)
tib <- tib %>%
  mutate(pred = predict(lm(y~x, data=tib)))
ggplot(tib, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_line(aes(x = x, y = pred), size = 2, linetype = 'solid') + 
  geom_segment(aes(x = x, xend = x, y = y, yend = pred), linetype = 'dashed') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) + 
  labs(x = "X", y = "Y") + 
  geom_rect(aes(xmin = .46, xmax = .85, ymin = -2.42, ymax = -1.8), fill = 'white', color = 'black') +
  geom_segment(aes(x = .48, xend = .58, y= -2.3, yend = -2.3), linetype = 'dashed') + 
  geom_segment(aes(x = .48, xend = .58, y= -2, yend = -2), size = 2, linetype = 'solid') +
  annotate(geom = 'text', x = .6, y = -2, label = 'Conditional Mean\n(from OLS)', hjust = 0, size = 15/.pt, lineheight = .7, family = 'Garamond') + 
  annotate(geom = 'text', x = .6, y = -2.3, label = 'Residual', hjust = 0, size = 15/.pt, family = 'Garamond')
ggsave('ols_resid.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)
  

# HH income control
oster %>%
  filter(bmi < 100) %>%
  mutate(vite = (supplement_vite_single == 'Took Vitamin E')*1) %>%
  lm(vite~bmi, data = .)
oster %>%
  filter(bmi < 100) %>%
  mutate(vite = (supplement_vite_single == 'Took Vitamin E')*1) %>%
  lm(vite~bmi+age + I(gender == 2), data = .)
msummary(list(m1,m2), stars = TRUE)


# 3-D 

set.seed(1015114)
df <- tibble(eps = rnorm(100), nu = rnorm(100), Z = rnorm(100)) %>%
  mutate(X = Z + nu) %>%
  mutate(Y = X + 2*Z + eps)

y_model <- lm(Y~Z, data = df)
x_model <- lm(X~Z, data = df)

df <- df %>%
  mutate(X_res = residuals(x_model),
         Y_res = residuals(y_model))

pmain <- ggplot(df, aes(x = X, y = Y)) + 
  geom_point() +
  geom_smooth(se = FALSE, color = 'black', size = 2, method = 'lm') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

pywithline <- ggplot(df, aes(x = Z, y = Y)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black', size = 2) + 
  scale_x_continuous(limits = c(min(df$Y), max(df$Y))) +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

pxwithline <- ggplot(df, aes(x = Z, y = X)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black', size = 2) + 
  scale_y_continuous(limits = c(min(df$X), max(df$X))) +
  theme_pubr() + 
  coord_flip() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

plot_grid(pmain, pywithline, pxwithline, ncol = 2)
ggsave('control_anim_1.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


pmain <- ggplot(df, aes(x = X_res, y = Y_res)) + 
  geom_point() +
  geom_smooth(se = FALSE, color = 'black', size = 2, method = 'lm') +
  theme_pubr() + 
  scale_y_continuous(limits = c(min(df$Y), max(df$Y))) +
  scale_x_continuous(limits = c(min(df$X), max(df$X))) +
  labs(x = 'X Residual',y = 'Y Residual')+
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())
 
pywithline <- ggplot(df, aes(x = Z, y = Y_res)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black', size = 2) + 
  scale_y_continuous(limits = c(min(df$Y), max(df$Y))) +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) + 
  labs(y = 'Y Residual') +
  geom_segment(aes(x = max(df$Z), xend = max(df$Z), y = max(predict(x_model)),yend = .1), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = min(df$Z), xend = min(df$Z), y = min(predict(x_model)),yend = -.1), arrow = arrow(length = unit(0.03, "npc")))


pxwithline <- ggplot(df, aes(x = Z, y = X_res)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', color = 'black', size = 2) + 
  scale_y_continuous(limits = c(min(df$X), max(df$X))) +
  theme_pubr() + 
  coord_flip() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.line = element_line(size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  labs(y = "X Residual") +
  geom_segment(aes(x = max(df$Z), xend = max(df$Z), y = max(predict(x_model)),yend = .1), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = min(df$Z), xend = min(df$Z), y = min(predict(x_model)),yend = -.1), arrow = arrow(length = unit(0.03, "npc")))


plot_grid(pmain, pywithline, pxwithline, ncol = 2)
ggsave('control_anim_2.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

# Chapter problems
freq_table <- 
  tribble(
    ~Classification, ~Aid, ~freq,
    "Freshman",  "Y",   508,
    "Freshman",  "N",   371,
    "Sophomore",  "Y",   349,
    "Sophomore",  "N",   337,
    "Junior",  "Y",   425,
    "Junior",  "N",   384,
    "Senior",  "Y",   288,
    "Senior",  "N",   338,
    
  ) 

freq_table %>%
  rename(`Financial Aid` = Aid) %>%
  pivot_wider(names_from = Classification, values_from = freq) %>%
  dftoLaTeX(title = 'Financial Aid by Student Standing',
            anchor = 'tab:describingrelationships-finaid',
            file = 'financial_aid_table.tex')

set.seed(20200702)

s_x <- 10000
s_y <- 1.5
rho <- -.5

sigma <-matrix(c(s_x^2, s_x*s_y*rho, s_x*s_y*rho, s_y^2),
               2)

dat <- as_tibble(rmvnorm(n = 500, mean = c(50000, 15), sigma = sigma)) %>%
  rename(Income = V1, Depression = V2)


ggplot(dat, aes(x = Income, y = Depression)) + 
  geom_jitter(alpha = .5) + 
  scale_x_continuous(labels = scales::dollar) +
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('depression_income.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

set.seed(20200703)
dat <- tibble(x = rnorm(100)) %>%
  mutate(y = 1 + 2 * x + 2 * x^2 + rnorm(100))

a <- ggplot(dat, aes(x = x, y = y)) +
  geom_jitter(alpha = .5) + 
  stat_smooth(formula = y ~ x, method = "lm", se = F, color = 'black') +
  labs(x = "Math Exam Scores", y = "Intelligence Scores") + 
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))

b <- ggplot(dat, aes(x = x, y = y)) +
  geom_jitter(alpha = .5) + 
  stat_smooth(method = "loess", se = F, color = 'black') +
  labs(x = "Math Exam Scores", y = "Intelligence Scores") + 
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))

plot_grid(a, b, labels = "AUTO")
ggsave('parabolic.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


set.seed(20200715)

s_x <- 1
s_y <- 1
rho <- .6

sigma <-matrix(c(s_x^2, s_x*s_y*rho, s_x*s_y*rho, s_y^2),
               2)

dat <- as_tibble(rmvnorm(n = 5, mean = c(5, 5), sigma = sigma)) %>%
  rename(get_along = V1, sat = V2) 

mod <- lm(sat ~ get_along, data = dat)

dat <- dat %>%
  mutate(pred_sat = predict(mod)) %>%
  mutate_all(round, 2) %>%
  mutate(id = 1:5) %>%
  select(id, everything())

dat %>%
  select(-id) %>%
  rename(GetAlong = get_along,
         Satisfaction = sat,
         Prediction = pred_sat) %>%
  dftoLaTeX(title = 'Satisfaction and Getting Along with Coworkers',
            anchor = 'fig:describingrelationships-getalong',
            file = 'get_along.tex')
  