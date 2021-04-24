{
  library(tidyverse)
  library(vtable)
  library(purrr)
  library(cowplot)
  library(Cairo)
  library(extrafont)
  library(haven)
  library(fGarch)
  library(ggpubr)
}


#### THE DISTRIBUTION

data(Scorecard, package = 'pmdplyr')

# Categorical variable
catvar <- Scorecard %>%
  group_by(unitid) %>%
  summarize(pred_degree_awarded_ipeds = first(pred_degree_awarded_ipeds)) %>%
  mutate(`Primary Degree Type Awarded` = factor(case_when(
    pred_degree_awarded_ipeds == 1 ~ 'Less than Two-Year Degree',
    pred_degree_awarded_ipeds == 2 ~ 'Two-Year Degree',
    pred_degree_awarded_ipeds == 3 ~ 'Four Year or More'
  ),levels = c('Less than Two-Year Degree',
               'Two-Year Degree',
               'Four Year or More'))) %>%
  select(`Primary Degree Type Awarded`)

st(catvar, 
   title = 'Distribution of Kinds of Degrees US Colleges Award',
   anchor = 'tab:describingvariables-frequencytable',
   note = 'Data from College Scorecard',
   out='latex', file = 'frequency_table.tex')

ggplot(catvar %>% 
         group_by(`Primary Degree Type Awarded`) %>%
         summarize(N = n()) %>%
         mutate(`Primary Degree Type Awarded` = factor(`Primary Degree Type Awarded`,levels = 
                                                         c('Less than Two-Year Degree',
                                                           'Two-Year Degree',
                                                           'Four Year or More'))), 
       aes(x = `Primary Degree Type Awarded`, y = N)) + 
  geom_col(fill = 'white', color = 'black') + 
  geom_text(aes(label = `Primary Degree Type Awarded`, 
                x = `Primary Degree Type Awarded`, y = N + 70), 
            vjust = 0,
            family = 'Garamond') +
  theme_pubr() + 
  labs(x = 'Primary Degree Type Awarded',
       y = 'Number of Colleges') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) 
ggsave('college_type_distribution.pdf', width = 6, height = 5, device = cairo_pdf)

# Histogram/density

ratiovec <- function(n,top) {
  rate <- top^(1/(n-1))
  
  v <- c(1)
  
  for (i in 2:n) {
    v[i] <- v[i-1]*rate
  }
  
  return(v)
}

ggplot(Scorecard, aes(x = earnings_med)) + 
  geom_histogram(color='black',fill='white',breaks=ratiovec(6,32)*10000) + 
  scale_x_log10(labels = scales::label_dollar(),
                breaks = ratiovec(6,32)*10000) + 
  labs(x = 'Average Earnings of This College & Cohort\'s Graduates (Log Scale)',
       y = 'Number of College Cohorts') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank()) 
ggsave('college_earnings_histogram.pdf', width = 6, height = 5, device = cairo_pdf)


p <- c(6,15,30) %>%
  map(function(x)
    ggplot(Scorecard, aes(x = earnings_med)) + 
      geom_histogram(color='black',fill='white',breaks=ratiovec(x,32)*10000) + 
      scale_x_log10(labels = scales::label_dollar(),
                    limits = c(10000,200000)) + 
      labs(x = 'Average Earnings (Log Scale)',
           y = 'Number of College Cohorts') + 
      theme_pubr() + 
      theme(text         = element_text(size = 13, family="Garamond"),
            axis.title.x = element_text(size = 13, family="Garamond"),
            axis.title.y = element_text(size = 13, family= "Garamond"),
            panel.grid = element_blank()))

p[[4]] <- ggplot(Scorecard, aes(x = earnings_med)) + 
  geom_density() + 
  scale_x_log10(labels = scales::label_dollar(),
                limits = c(10000,200000)) + 
  labs(x = 'Average Earnings of This College \n& Cohort\'s Graduates (Log Scale)',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())

plot_grid(p[[1]],p[[2]],p[[3]],p[[4]], nrow = 2)
ggsave('histogram_to_density.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

densdata <- with(density(log(Scorecard$earnings_med),na.rm=TRUE),data.frame(x=exp(x),y))
ggplot() + 
  geom_ribbon(data=densdata %>% dplyr::filter(x > 40000 & x < 50000),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_line(data=densdata, mapping=aes(x = x, y = y)) + 
  scale_x_log10(labels = c('$40k','$50k'),
                limits = c(10000,200000),
                breaks=c(40000,50000)) + 
  labs(x = 'Average Earnings of This College & Cohort\'s Graduates (Log Scale)',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.x = element_text(angle = c(0,0,20,0,0,20),
                                   hjust = c(.5,.5,.7,.5,.5,.7)),
        panel.grid = element_blank())
ggsave('shaded_density.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

mean(Scorecard$earnings_med >= 40000 & Scorecard$earnings_med <= 50000, na.rm =TRUE)

## SUMMARIZING THE DISTRIBUTION

names <- c('Very Low Variation','A Little More Variation','Even More Variation','Lots of Variation')

set.seed(1000)
dfs <-  c(.2,.25,.5,.75) %>%
  map(function(x)
    data.frame(x = rnorm(100,sd=x)))

p <- 1:4 %>%
  map(function(x)
    ggplot(dfs[[x]],aes(x=x))+
      geom_density()+
      scale_x_continuous(limits=c(min(dfs[[4]]$x),max(dfs[[4]]$x)))+
      scale_y_continuous(limits=c(0,
                                  max(density(dfs[[1]]$x)$y)))+
      labs(x='Observed Value',y='Density',title=names[x])+
      theme_pubr()+
      theme(text         = element_text(size = 13, family="Garamond"),
            axis.title.x = element_text(size = 13, family="Garamond"),
            axis.title.y = element_text(size = 13, family= "Garamond"),
            panel.grid = element_blank()))


plot_grid(p[[1]],p[[2]],p[[3]],p[[4]], nrow = 2)
ggsave('amount_of_variation.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

a <- data.frame(`Observed Values` = factor(c(2,5,5,6)))

st(a, 
   title = 'Distribution of a Variable',
   anchor = 'tab:describingvariables-minifrequency',
   out='latex', file = 'mini_frequency_table.tex')


ggplot() + 
  geom_ribbon(data=densdata %>% dplyr::filter(x < quantile(Scorecard$earnings_med,.05, na.rm = TRUE)),
              mapping=aes(ymax = y,ymin=0,x=x),
              alpha = .5, color = 'gray') + 
  geom_line(data=densdata, mapping=aes(x = x, y = y)) + 
  scale_x_log10(labels = function(x) scales::dollar(x, accuracy = 1),
                limits = c(10000,200000),
                breaks=c(quantile(Scorecard$earnings_med,.05, na.rm = TRUE))) + 
  annotate(geom='text',x=15000,y=.1,label='5%',family='Garamond', size = 13/.pt) + 
  annotate(geom='text',x=30000,y=.1,label='95%',family='Garamond', size = 13/.pt) + 
  labs(x = 'Average Earnings of This College & Cohort\'s Graduates (Log Scale)',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('fifth_percentile.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

var(Scorecard$earnings_med,na.rm=TRUE)
sd(Scorecard$earnings_med,na.rm=TRUE)
mean(Scorecard$earnings_med,na.rm=TRUE)
(38000-33348.62)/12380.95

logd <- Scorecard %>% mutate(lx = log(earnings_med)) %>% filter(!is.na(lx))
ggplot(logd, aes(x = lx)) + 
  geom_density() + 
  geom_vline(aes(xintercept = mean(lx),ymax=1)) + 
  geom_vline(aes(xintercept = mean(lx) - sd(lx)),linetype = 'dashed') +
  geom_vline(aes(xintercept = mean(lx) + sd(lx)),linetype = 'dashed') +
  scale_x_continuous(labels = scales::dollar(exp(c(mean(logd$lx),mean(logd$lx)-sd(logd$lx),mean(logd$lx)+sd(logd$lx))), accuracy = 1),
                limits = c(log(10000),log(200000)),
                breaks=c(mean(logd$lx),mean(logd$lx)-sd(logd$lx),mean(logd$lx)+sd(logd$lx))) + 
  annotate(geom='label',x=mean(logd$lx)+.01,y=1.25,label='Mean',family='Garamond',hjust=.5) + 
  annotate(geom='label',x=mean(logd$lx)-sd(logd$lx)+.01,y=1.25,label='Mean - 1 SD',family='Garamond',hjust=.5) + 
  annotate(geom='label',x=mean(logd$lx)+sd(logd$lx)+.01,y=1.25,label='Mean + 1 SD',family='Garamond',hjust=.5) + 
  labs(x = 'Average Earnings of This College & Cohort\'s Graduates (Log Scale)',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid = element_blank())
ggsave('standard_deviation.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

mean(logd$lx < mean(logd$lx)) - mean(logd$lx < mean(logd$lx) - sd(logd$lx)) 
mean(logd$lx < mean(logd$lx)+sd(logd$lx)) - mean(logd$lx < mean(logd$lx)) 


logd <- Scorecard %>% mutate(lx = log(earnings_med)) %>% dplyr::filter(!is.na(lx))
ggplot(logd, aes(x = lx)) + 
  geom_density() + 
  geom_vline(aes(xintercept = median(lx))) + 
  geom_vline(aes(xintercept = quantile(lx,.25)),linetype = 'dashed') +
  geom_vline(aes(xintercept = quantile(lx,.75)),linetype = 'dashed') +
  scale_x_continuous(labels = scales::dollar(exp(c(mean(logd$lx),mean(logd$lx)-sd(logd$lx),mean(logd$lx)+sd(logd$lx))),accuracy = 1),
                     limits = c(log(10000),log(200000)),
                     breaks=c(median(logd$lx),quantile(logd$lx,c(.25,.75)))) + 
  annotate(geom='label',x=median(logd$lx),y=1.25,label='Median',family='Garamond',hjust=.5) + 
  annotate(geom='label',x=quantile(logd$lx,.25),y=1.15,label='25th Percentile',family='Garamond',hjust=.7) + 
  annotate(geom='label',x=quantile(logd$lx,.75),y=1.15,label='75th Percentile',family='Garamond',hjust=.3) + 
  geom_segment(aes(x=quantile(lx,.25)+.02,xend=quantile(lx,.75)-.02),
               y=.4,yend=.4,arrow=arrow(ends='both',type='closed',length=unit(.1,'inches')))+
  annotate(geom='label',x=median(logd$lx),y=.4,label='IQR',family='Garamond',hjust=.5)+
  labs(x = 'Average Earnings of This College & Cohort\'s Graduates (Log Scale)',
       y = 'Density') + 
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.x = element_text(angle = 20, hjust = .7),
        panel.grid = element_blank())
ggsave('iqr.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


## Normal distribution matching with bigger sample
nobs <- c(10,50,100,1000)
set.seed(500)
p <- nobs %>%
  map(function(x) with(density(rnorm(x)),data.frame(x,y)) %>%
        mutate(yn = dnorm(x)) %>%
        ggplot(aes(x=x,y=y)) + 
        geom_line(linetype='dashed')+
        geom_line(aes(x=x,y=yn),linetype='solid')+
        labs(x = 'Value of Observation',y ='Density',title = paste(x,'Observations')) + 
        theme_pubr() + 
        theme(text         = element_text(size = 13, family="Garamond"),
              axis.title.x = element_text(size = 13, family="Garamond"),
              axis.title.y = element_text(size = 13, family= "Garamond"),
              panel.grid = element_blank()))
plot_grid(p[[1]],p[[2]],p[[3]],p[[4]], nrow = 2)
ggsave('approach_limit.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


## Income distribution
df <- read_stata(gzfile('usa_00016.dta.gz'))
df_noz <- df %>% filter(inctot > 1)
ggplot(df_noz,aes(x=inctot,weight=perwt)) + 
  geom_density() + 
  labs(x = 'Personal Income',y='Density') +
  scale_x_continuous(labels=scales::dollar)+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())
ggsave('income_distribution.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)


ggplot(df_noz,aes(x=log(inctot),weight=perwt)) + 
  geom_density() + 
  labs(x = 'Log Personal Income',y='Density') +
  scale_x_continuous(labels=scales::number)+
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())
ggsave('log_income_distribution.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

# Normal and log-normal
set.seed(500)
df <- data.frame(x = seq(-4,4,length.out=100000)) %>%
  mutate(y = dnorm(x),
         expy = exp(x))

p1 <- ggplot(df, aes(x = x,y=y)) + geom_line() + 
  labs(x = 'Value',y = 'Density', title = 'Normal Distribution') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(df, aes(x = expy)) + geom_density() + 
  labs(x = 'Value',y = 'Density', title = 'Log-Normal Distribution') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())
plot_grid(p1,p2, nrow = 1)
ggsave('theoretical.pdf',width = 7, height = 3, units = 'in', device = cairo_pdf)

## Chapter Questions

salary_dat <- tibble(Salary = c(85000, 90000, 100000, 120000, 125000, 130000),
                     Frequency = c(5, 4, 1, 2, 3, 2))

dftoLaTeX(salary_dat, title = 'Economics Professor Salaries',
          anchor = 'tab:describingvariables-profsalary',
          file = 'professor_salary.tex')

set.seed(1000)
dat <- tibble(a = rnorm(1000, 10, 3),
              b = runif(1000, 0, 20),
              c = rnorm(1000, 10, 5),
              d = rnorm(1000, 10, 1))

dat %>%
  pivot_longer(cols = a:d,
               names_to = "var",
               values_to = "val") %>%
  ggplot(aes(x = val)) + 
  geom_density() + 
  facet_wrap(~ var) +
  labs(x = "Value", y = "Density") + 
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank())
ggsave('question_variability.pdf', width = 5, height = 4, units = 'in', device = cairo_pdf)


exam_scores <- tibble(scores = rsnorm(1000, mean = 80, sd = 3, xi = 1.5))

ggplot(exam_scores, aes(x = scores)) + 
  geom_density() + 
  labs(y = "Density", x = "Scores") + 
  theme_pubr() +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())
ggsave('question_students.pdf', width = 4, height = 3, units = 'in', device = cairo_pdf)


dat <- tibble(Classification = c("Freshman", "Sophomore", "Junior", "Senior"),
              Count = c(1000, 1200, 900, 1500))
dftoLaTeX(dat, title = 'Student Standing',
          anchor = 'tab:describingvariables-studstanding',
          file = 'student_standing.tex')
