{
  library(tidyverse)
  library(vtable)
  library(lubridate)
  library(ggpubr)
  library(extrafont)
  library(cowplot)
  library(modelsummary)
  library(fixest)
}
library(extrafont)

# Snow table
tb <- tibble(`Region Supplier` = c('Non-Lambeth Only (Dirty)','Lambeth + Others (Mix Dirty and Clean)'),
             `Death Rates 1849` = c(134.9, 130.1),
             `Death Rates 1854` = c(146.6, 84.9))
dftoLaTeX(tb, note = 'Death rates are deaths per 10,000 1851 population.',
          align = 'p{.5\\textwidth}p{.25\\textwidth}p{.25\\textwidth}',
          fit.page = '.95\\textwidth',
          anchor = 'tab:differenceindifferences-snow',
          title = 'London Cholera Deaths per 10,000 from Snow (1855)',
          file = 'snow_deaths.tex')


# Kessler and Roth
d <- read_tsv('kessler_roth.tsv') %>%
  pivot_longer(cols = 2:7,
               names_to = 'quarter',
               values_to = 'rate') %>%
  mutate(qtr = case_when(
    quarter == 'Q42010' ~ ymd('2010-10-01'),
    quarter == 'Q12011' ~ ymd('2011-01-15'),
    quarter == 'Q22011' ~ ymd('2011-05-01'),
    quarter == 'Q32011' ~ ymd('2011-08-01'),
    quarter == 'Q42011' ~ ymd('2011-10-01'),
    quarter == 'Q12012' ~ ymd('2012-01-15')
  ),
  st = State == 'California') 

# raw data
p <- ggplot(d, aes(x = qtr, y = rate/100, color = st, shape = st, size = st)) + 
  geom_point(position = position_jitter(seed = 1000)) +
  geom_vline(aes(xintercept = ymd('2011-07-01')), linetype = 'dashed', size = .5)  +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1))+
  scale_x_date(labels = function(x) {
    mo <- month(x)
    paste(month.abb[mo], year(x))
  }) +
  scale_color_manual(values = c('gray','black')) + 
  scale_shape_manual(values = c(1, 19)) +
  scale_size_manual(values = c(2, 5)) +
  theme_pubr() +
  labs(x = "Quarter",
       y = "Organ Donation Rate")+
  guides(color = FALSE, shape = FALSE, size = FALSE) +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
p + 
  labs(caption = 'Jitter has been added to the x-axis to make points easier to see, since data is quarterly.')+
  annotate(geom = 'text', label = 'California', family = 'Garamond', size =  14/.pt,
           x = ymd('2012-01-15'), y = .21, hjust = .5)
ggsave('raw_donations.pdf', width = 6, height = 5,device=cairo_pdf)

# did averages
did <- d %>%
  group_by(st, qtr > ymd('2011-07-01')) %>%
  summarize(m = mean(rate)/100)


p1a <- p +
  geom_segment(aes(x = min(qtr), xend = ymd('2011-07-01'), y = did$m[1], yend = did$m[1]),
               size = 1.5, color = 'darkgray', linetype = 'dashed') + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = max(qtr), y = did$m[2], yend = did$m[2]),
               size = 1.5, color = 'darkgray', linetype = 'dashed') +
  geom_segment(aes(x = min(qtr), xend = ymd('2011-07-01'), y = did$m[3], yend = did$m[3]),
               size = 1.5, color = 'black') + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = max(qtr), y = did$m[4], yend = did$m[4]),
               size = 1.5, color = 'black') 
p1 <- p1a + 
  labs(title = '(a) Before/After, Treated/Untreated Averages')
p2 <- p1a + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = ymd('2011-07-01'),
               y = did$m[1], yend = did$m[2]), size = 1.5, color = 'black') +
  geom_segment(aes(x = ymd('2011-06-01'), xend = ymd('2011-06-28'), 
                   y = .55, yend = mean(did$m[1:2])),
               arrow = arrow(length = unit(0.03, "npc")), size = 1, color = 'black') +
  annotate(geom = 'text', x =  ymd('2011-06-01'), y = .58, size = 14/.pt, family = 'Garamond',
           label = 'Untreated Before/After Difference') + 
  labs(title = '(b) Calculate Before/After Untreated Diff')
p3a <- ggplot(d, aes(x = qtr, y = rate/100 - (did$m[2] - did$m[1]), color = st, shape = st, size = st)) + 
  geom_point(position = position_jitter(seed = 1000)) +
  geom_vline(aes(xintercept = ymd('2011-07-01')), linetype = 'dashed', size = .5) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1))+
  scale_x_date(labels = function(x) {
    mo <- month(x)
    paste(month.abb[mo], year(x))
  }) +
  scale_color_manual(values = c('gray','black')) + 
  scale_shape_manual(values = c(1, 19)) +
  scale_size_manual(values = c(2, 5)) +
  theme_pubr() +
  labs(x = "Quarter",
       y = "Organ Donation Rate")+
  guides(color = FALSE, shape = FALSE, size = FALSE) +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) +
  geom_segment(aes(x = min(qtr), xend = ymd('2011-07-01'), y = did$m[1], yend = did$m[1]),
               size = 1.5, color = 'darkgray', linetype = 'dashed') + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = max(qtr), y = did$m[1], yend = did$m[1]),
               size = 1.5, color = 'darkgray', linetype = 'dashed') +
  geom_segment(aes(x = min(qtr), xend = ymd('2011-07-01'), y = did$m[3], yend = did$m[3]),
               size = 1.5, color = 'black') + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = max(qtr), y = did$m[4]- (did$m[2] - did$m[1]), yend = did$m[4]- (did$m[2] - did$m[1])),
               size = 1.5, color = 'black')
p3 <- p3a +
  geom_segment(aes(x = ymd('2011-09-01'), xend = ymd('2011-09-01'), y = did$m[2] + .04, yend = did$m[1]),
               arrow = arrow(length = unit(0.03, "npc")), size = 1, color = 'black') +
  geom_segment(aes(x = ymd('2011-09-01'), xend = ymd('2011-09-01'), y = did$m[4]- (did$m[2] - did$m[1]) + .05, yend = did$m[4]- (did$m[2] - did$m[1])),
               arrow = arrow(length = unit(0.03, "npc")), size = 1, color = 'black') +
  labs(title = '(c) Remove Before/After Untreated Diff')
  
p4 <- p3a + 
  geom_segment(aes(x = ymd('2011-07-01'), xend = ymd('2011-07-01'),
                   y = did$m[3], yend = did$m[4]- (did$m[2] - did$m[1])), size = 1.5, color = 'black') +
  geom_segment(aes(x = ymd('2011-06-01'), xend = ymd('2011-06-28'), 
                   y = .18, yend = mean(c(did$m[3],did$m[4]- (did$m[2] - did$m[1])))),
               arrow = arrow(length = unit(0.03, "npc")), size = 1, color = 'black') +
  annotate(geom = 'text', x =  ymd('2011-06-01'), y = .16, size = 14/.pt, family = 'Garamond',
           label = 'Remaining Treated Before/After Difference') +
  labs(title = '(d) Remaining Before/After Treated Diff')

plot_grid(p1,p2,p3,p4, ncol = 2)
ggsave('did_steps.pdf',width = 10, height = 6, units = 'in', device = cairo_pdf)

(did$m[4] - did$m[3]) - (did$m[2] - did$m[1])
(did$m[4] - did$m[3])
did

# Prior trends demonstration
set.seed(1000)
tb <- tibble(Time = rep(1:10 ,2),
             Group = c(rep('Treatment',10),
                       rep('Control',10))) %>%
  mutate(After = Time >= 7) %>%
  mutate(YCons = .4*Time + 2*After*(Group == 'Treatment') + (Group == 'Treatment')+ rnorm(20, 0, .5),
         YDiv = (.3+.5*(Group == 'Control'))*Time + 2*After*(Group == 'Treatment') + 3*(Group == 'Treatment')+ rnorm(20, 0, .5))

p1 <- ggplot(tb, aes(x = Time, y = YCons, color = Group)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(aes(xintercept = 7), linetype = 'dashed') + 
  geom_text(data = tb %>% filter(Time == 10),
            aes(x = Time + .1, label = Group, color = Group),
            family = 'Garamond', size = 13/.pt, hjust = 0) +
  annotate(geom = 'label', x = 7, y = 1, label = 'Treatment\nPeriod',
           family = 'Garamond', size = 13/.pt) +
  scale_color_manual(values = c('#676767','black')) + 
  expand_limits(x = 12) +
  labs(y = 'Outcome', 
       title = '(a) Parallel Prior Trends') +
  theme_pubr() +
  guides(color = FALSE) +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p2 <- ggplot(tb, aes(x = Time, y = YDiv, color = Group)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(aes(xintercept = 7), linetype = 'dashed') + 
  geom_text(data = tb %>% filter(Time == 10),
            aes(x = Time + .1, label = Group, color = Group),
            family = 'Garamond', size = 13/.pt, hjust = 0) +
  annotate(geom = 'label', x = 7, y = 2.5, label = 'Treatment\nPeriod',
           family = 'Garamond', size = 13/.pt) +
  scale_color_manual(values = c('#676767','black')) +
  expand_limits(x = 12) +
  labs(y = 'Outcome', 
       title = '(b) Converging Prior Trends') +
  theme_pubr() +
  guides(color = FALSE) +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        axis.text = element_blank())

plot_grid(p1,p2)
ggsave('prior_trends.pdf',width = 10, height = 5, units = 'in', device = cairo_pdf)

# Regression model
m <- feols(I(rate/100)~ Treatment | State + Quarter,
        data = d %>% mutate(Treatment = st & qtr > ymd('2011-07-01')) %>% rename(Quarter = qtr))

knitr::opts_current$set(label = 'differenceindifferences-twfe')
msummary(list('Organ Donation Rate' =m), output = 'did_twfe.tex',
          coef_map = c('TreatmentTRUE' = 'Treatment'),
          gof_omit = 'R2|AIC|BIC|Lik|Std',
          stars = TRUE,
         notes = 'Standard errors clustered at the state level.',
          title = 'Difference-in-differences Estimate of the Effect of Active-Choice Phrasing on Organ Donor Rates') 



od <- read_csv('organ_donation.csv') %>%
  # Use only pre-treatment data
  filter(Quarter %in% c('Q42010','Q12011','Q22011'))

# Create our fake treatment variables
od <- od %>%
  mutate(FakeTreat1 = State == 'California' & 
           Quarter %in% c('Q12011','Q22011'),
         FakeTreat2 = State == 'California' &
           Quarter == 'Q22011')

# Run the same model we did before
# but with our fake treatment
clfe1 <- feols(Rate ~ FakeTreat1 | State + Quarter,
               data = od)
clfe2 <- feols(Rate ~ FakeTreat2 | State + Quarter,
               data = od)

knitr::opts_current$set(label = 'differenceindifferences-placebo')
msummary(list('Second-Period Treatment' = clfe1,
              'Third-Period Treatment' = clfe2), stars = TRUE,
         output = 'did_placebo.tex',
         coef_map = c('FakeTreat1TRUE' = 'Treatment',
                      'FakeTreat2TRUE' = 'Treatment'),
         gof_omit = 'R2|AIC|BIC|Lik|Std',
         notes = 'Standard errors clustered at the state level.',
         title = 'Placebo DID Estimates Using Fake Treatment Periods')

# Long-term effect
# R CODE
library(tidyverse); library(modelsummary)
library(fixest); library(broom)

od <- read_csv('organ_donation.csv')

# Treatment variable
od <- od %>%
  mutate(Treated = State == 'California' & 
           Quarter %in% c('Q32011','Q42011','Q12012')) %>%
  # Create an ordered version of Quarter so we can graph it
  # and make sure we drop the last pre-treatment interaction
  mutate(Quarter = relevel(factor(Quarter), ref = 'Q22011'))

# Interact quarter with being in the treated group
clfe <- feols(Rate ~ I(State == 'California')*Quarter | State,
              data = od)

# Use broom::tidy to get the coefficients and SEs
res <- tidy(clfe) %>%
  # Keep only the interactions
  filter(str_detect(term, ':')) %>%
  # Pull the quarter out of the term
  mutate(Quarter = str_sub(term, -6)) %>%
  # Add in the term we dropped as 0
  add_row(estimate = 0, std.error = 0, Quarter = 'Q22011') %>%
  # and add 95% confidence intervals
  mutate(ci_bottom = estimate - 1.96*std.error,
         ci_top = estimate + 1.96*std.error) %>%
  # And put the quarters in order
  mutate(Quarter = factor(Quarter,
                          levels = c('Q42010','Q12011','Q22011',
                                     'Q32011','Q42011','Q12012')))


# And graph
# "group = 1" is necessary to get ggplot to add the line graph
# when the x-axis is a factor
ggplot(res, aes(x = Quarter, y = estimate, group = 1)) + 
  # Add points for each estimate and connect them
  geom_point() + 
  geom_line() +
  # Add confidence intervals
  geom_linerange(aes(ymin = ci_bottom, ymax = ci_top)) +
  # Add a line so we know where 0 is
  geom_hline(aes(yintercept = 0), linetype = 'dashed') + 
  # Always label!
  labs(caption = '95% Confidence Intervals Shown',
       y = 'Difference-in-Difference Estimate') + 
  ggpubr::theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('dynamic_effect.pdf', width = 6, height = 5,device=cairo_pdf)

## DIDID
# Figure 2
d <- read.csv(text = 'date, y
2007/01/29, 0
2007/02/28, 0
2007/04/01, -0.00010416666666666213
2007/05/01, 0
2007/05/30, -0.00031249999999999334
2007/07/02, -0.0005208333333333245
2007/07/30, -0.0006249999999999936
2007/09/01, -0.0007291666666666627
2007/10/02, -0.0008333333333333248
2007/10/31, -0.0008333333333333248
2007/11/28, -0.0009374999999999939
2007/12/31, -0.0012499999999999942
2008/01/31, -0.0035416666666666582
2008/02/29, -0.004791666666666659
2008/03/30, -0.004583333333333328
2008/05/02, 0
2008/05/31, 0.002291666666666671
2008/07/01, 0.006875000000000003
2008/08/01, 0.006979166666666668
2008/08/31, 0.009687500000000005
2008/09/29, 0.009375000000000005
2008/11/01, 0.00854166666666667
2008/12/01, 0.013125000000000001
2008/12/30, 0.020833333333333336
2009/02/01, 0.025416666666666667
2009/03/04, 0.028229166666666666
2009/04/04, 0.025520833333333333
2009/05/04, 0.022708333333333334
2009/06/02, 0.023645833333333335
2009/07/01, 0.0225
2009/08/03, 0.02354166666666667
2009/08/31, 0.026041666666666664
2009/10/03, 0.026458333333333334
2009/11/03, 0.028854166666666667
2009/12/01, 0.028125
2010/01/03, 0.03083333333333333
2007/01/29, -0.00010416666666666213
2007/02/28, -0.00020833333333332427
2007/04/01, 0
2007/05/01, 0
2007/06/01, 0
2007/07/02, 0
2007/07/30, 0
2007/08/30, 0.00020833333333333814
2007/09/30, 0.00041666666666666935
2007/10/30, 0.00041666666666666935
2007/12/01, 0.0006250000000000006
2007/12/31, 0.0012500000000000011
2008/01/31, 0.0014583333333333393
2008/03/02, 0.0016666666666666705
2008/03/30, 0.0018750000000000017
2008/04/30, 0.0025000000000000022
2008/05/31, 0.0028125000000000025
2008/06/30, 0.0036458333333333343
2008/08/01, 0.003437500000000003
2008/09/02, 0.003437500000000003
2008/09/29, 0.0037500000000000033
2008/11/01, 0.0031250000000000028
2008/12/02, 0.003854166666666669
2008/12/30, 0.004375000000000004
2009/02/01, 0.005208333333333336
2009/03/02, 0.006666666666666668
2009/04/01, 0.007708333333333334
2009/05/04, 0.008750000000000004
2009/06/02, 0.008750000000000004
2009/07/01, 0.01260416666666667
2009/08/03, 0.010937500000000003
2009/08/29, 0.013125000000000001
2009/10/03, 0.013333333333333336
2009/11/01, 0.013125000000000001
2009/11/29, 0.014166666666666668
2010/01/03, 0.014895833333333334') %>%
  mutate(date = ymd(date)) %>%
  mutate(Type = c(rep('ESRR',n()/2),
                  rep('Not\nESRR',n()/2)))

p1 <- ggplot(d, aes(x = date, y = y, linetype = Type, shape = Type)) + 
  geom_line(size = 1) + 
  geom_point() + 
  geom_vline(aes(xintercept = ymd("2008-02-01")), linetype = 'dashed') +
  geom_text(mapping = aes(label = Type), data = d %>% filter(date == max(date)), 
            hjust = -.1, family = 'Garamond', size = 14/.pt) + 
  scale_x_date(labels = function(x) paste0(month.abb[month(x)],' ', str_sub(year(x),3,4)), 
               breaks = ymd('2007-02-01') + years(0:3),
               limits = c(min(d$date), max(d$date) + months(6))) +
  labs(x = 'Month', y = 'Average Modification Rate', caption = '') +
  guides(linetype = 'none', shape = 'none') +
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond',
                            size = 14))

# Figure 3

d2 <- read.csv(text = 'date, y
2007/01/28, -0.017163830287820564
2007/02/28, -0.016370794447494313
2007/04/01, -0.013626539094972953
2007/04/29, -0.012054270253313473
2007/05/30, -0.01399294173006041
2007/07/01, -0.011443808328758548
2007/07/31, -0.009675790134107146
2007/08/31, -0.014931534781585779
2007/10/01, -0.011992157477844892
2007/10/30, -0.009249156928868338
2007/12/02, -0.0059189083209160165
2007/12/30, -0.008444200454866296
2008/02/01, -0.0033578542859383637
2008/03/02, -0.002955689749823548
2008/03/31, -0.0037242569210258064
2008/04/30, 0.00019010273704023195
2008/06/02, -0.007601599874519666
2008/06/30, -0.0036878676182260373
2008/08/01, 0.002763704807466072
2008/09/01, 0.014678691867304514
2008/10/01, 0.02073939298878518
2008/11/01, 0.027386087365696796
2008/12/01, 0.03578825190181162
2008/12/31, 0.03872700180378009
2009/02/02, 0.04186212846051289
2009/03/04, 0.03367892714296917
2009/04/03, 0.035837189240059596
2009/05/04, 0.0407277860559956
2009/06/03, 0.041129950592110415
2009/07/03, 0.043483334640420354
2009/08/03, 0.04895929731001489
2009/09/02, 0.045459022821739456
2009/10/03, 0.043130107442553516
2009/11/04, 0.03865485059995293
2009/12/02, 0.03125150968551485
2010/01/02, 0.026581130891694754
2007/01/28, -0.01072480589757667
2007/02/27, -0.010517763312681366
2007/04/01, -0.01089483177789978
2007/05/01, -0.010687789193004477
2007/06/01, -0.009699631401458728
2007/07/02, -0.008906595561132477
2007/07/31, -0.008114814524351047
2007/08/31, -0.007321778684024796
2007/10/01, -0.00691898674613757
2007/10/30, -0.006517449611795165
2007/12/02, -0.004943298564818455
2007/12/30, -0.005127127284134586
2008/02/01, -0.006089561603011537
2008/03/02, -0.007053250725433308
2008/03/30, -0.007627323347188464
2008/05/02, -0.00741902595874834
2008/06/02, -0.007601599874519666
2008/06/30, -0.0038829895694455496
2008/08/01, -0.004065563485216861
2008/09/03, 0.0004354168300525346
2008/10/01, 0.0023979295741510323
2008/11/01, 0.003190965414477283
2008/12/03, 0.007301074425535237
2008/12/29, 0.009067837816641819
2009/02/02, 0.011032860167829964
2009/03/05, 0.013191749666692787
2009/04/03, 0.015349384362010804
2009/05/04, 0.013800956787702913
2009/06/01, 0.013812250019606294
2009/07/03, 0.016361383420908156
2009/08/05, 0.013642851541055594
2009/09/02, 0.015215120382715067
2009/10/02, 0.013666065406634759
2009/11/04, 0.012703631087757808
2009/12/03, 0.012910673672653104
2010/01/01, 0.009800015685044294') %>%
  mutate(date = ymd(date)) %>%
  mutate(Type = c(rep('ESRR',n()/2),
                  rep('Not\nESRR',n()/2)))


p2 <- ggplot(d2, aes(x = date, y = y, linetype = Type, shape = Type)) + 
  geom_line(size = 1) + 
  geom_point() + 
  geom_vline(aes(xintercept = ymd("2008-02-01")), linetype = 'dashed') +
  geom_text(mapping = aes(label = Type), data = d2 %>% group_by(Type) %>% filter(date == max(date)), 
            hjust = -.1, family = 'Garamond', size = 14/.pt) + 
  scale_x_date(labels = function(x) paste0(month.abb[month(x)],' ', str_sub(year(x),3,4)), 
               breaks = ymd('2007-02-01') + years(0:3),
               limits = c(min(d$date), max(d$date) + months(6))) +
  labs(x = 'Month', y = 'Foreclosure Filing Rate', caption = 'Copyright: Elsevier. Minor changes from original.') +
  guides(linetype = FALSE, shape = FALSE) +
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond',
                            size = 14))

plot_grid(p1,p2)
ggsave('didid.pdf', width = 8, height = 3.5,device=cairo_pdf)
