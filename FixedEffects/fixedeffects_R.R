{
library(tidyverse)
library(vtable)
library(cowplot)
library(Cairo)
library(extrafont)
library(gapminder)
library(lfe)
library(Cairo)
library(ggpubr)
library(modelsummary)
}

# Table example of between/within
tb <- tibble(Individual = c('You','You','Me','Me'),
             Year = c('2019','2020','2019','2020'),
             Exercise = c(5,7,4,3))
tb %>%
  dftoLaTeX(file='fixedeffects_within_tab1.tex',
            title='Exercise and Colds',
            anchor='tab:fixedeffects-within1')

tb %>%
  group_by(Individual) %>%
  mutate(MeanExercise = mean(Exercise)) %>%
  mutate(WithinExercise = Exercise - MeanExercise) %>%
  dftoLaTeX(file='fixedeffects_within_tab2.tex',
            title='Exercise with Within Variation',
            anchor = 'tab:fixedeffects-within2')


# Graphically
set.seed(2000)
tb <- tibble(Individual = c(rep('You',8),rep('Me',8))) %>%
  mutate(ExercisePerWeek = 4*runif(16) + 3*(Individual == "You")) %>%
  mutate(ColdsPerYear = 5+ 5*runif(16) + 5*(Individual == 'You') - ExercisePerWeek) %>%
  group_by(Individual) %>%
  mutate(ExMean = mean(ExercisePerWeek),
         ColdsMean = mean(ColdsPerYear))
p1 <- ggplot(tb, 
             aes(x = ExercisePerWeek,y=ColdsPerYear, shape = Individual, color = Individual)) + 
  geom_point() + 
  scale_color_manual(values = c('Me' = 'black', 'You' = '#777777')) + 
  theme_pubr() + 
  theme(legend.position = c(.85,.8),
        legend.background = element_rect(color = 'black'),
        text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) + 
  labs(x = "Exercise Hours Per Week",
       y = "Colds Per Year",
       title = "Raw Data")+
  scale_y_continuous(limits = c(3,11)) + 
  scale_x_continuous(limits = c(0,8))
p2 <- ggplot(tb, 
       aes(x = ExercisePerWeek,y=ColdsPerYear, shape = Individual, color = Individual)) + 
  geom_point() + 
  geom_point(aes(x = ExMean, y = ColdsMean, color = Individual),shape = 3, size = 40) + 
  scale_color_manual(values = c('Me' = 'black', 'You' = '#777777')) + 
  theme_pubr() + 
  guides(shape = FALSE, color = FALSE) + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) + 
  labs(x = "Exercise Hours Per Week",
       y = "Colds Per Year",
       title = "Raw Data with Individual Means")+
  scale_y_continuous(limits = c(3,11)) + 
  scale_x_continuous(limits = c(0,8))
p3 <- ggplot(tb, 
             aes(x = ExMean,y=ColdsMean, shape = Individual, color = Individual)) + 
  geom_point() + 
  geom_point(aes(x = ExMean, y = ColdsMean, color = Individual),shape = 3, size = 40) +
  scale_color_manual(values = c('Me' = 'black', 'You' = '#777777')) + 
  theme_pubr() + 
  guides(shape = FALSE, color = FALSE) + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) + 
  labs(x = "Exercise Hours Per Week",
       y = "Colds Per Year",
       title = "Just Between Variation")+
  scale_y_continuous(limits = c(3,11)) + 
  scale_x_continuous(limits = c(0,8))
p4 <- ggplot(tb  %>% dplyr::filter(Individual == 'Me'), 
       aes(x = ExercisePerWeek - ExMean,y=ColdsPerYear - ColdsMean, shape = Individual, color = Individual)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) +
  scale_color_manual(values = c('Me' = 'black', 'You' = '#777777')) + 
  theme_pubr() + 
  guides(shape = FALSE, color = FALSE) + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) + 
  labs(x = "Exercise Hours Per Week",
       y = "Colds Per Year",
       title = "Just Within Variation, Just for Me")
plot_grid(p1,p2,p3,p4, nrow = 2)
ggsave('between_within_graphs.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

# Walk through FE
set.seed(2000)
tb <- tibble(Individual = factor(c(rep('You',8),rep('Me',8),
                            rep('Shamma',8),rep('Liqing',8)),levels = c('Me','You','Liqing','Shamma')),
             IndNo = sort(rep(1:4,8))) %>%
  mutate(IntensityOfReminders = runif(32)*5 + IndNo) %>%
  mutate(HealthyEatingScore = runif(32)*10 + IntensityOfReminders - 2*IndNo) %>%
  mutate(HealthyEatingScore = case_when(
    HealthyEatingScore < 0 ~ 0,
    TRUE ~ HealthyEatingScore
  )) %>%
  group_by(IndNo) %>%
  mutate(MeanIntensity = mean(IntensityOfReminders),
         MeanScore = mean(HealthyEatingScore)) %>%
  mutate(Intensity.R = IntensityOfReminders - MeanIntensity,
         Score.R = HealthyEatingScore - MeanScore)

ggplot(tb, aes(x = IntensityOfReminders,
               y = HealthyEatingScore, 
               shape = Individual)) + 
  geom_point() + 
  theme_pubr() +
  labs(x = "Intensity of Reminders",
       y = "Healthy Eating Score")+
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('fe_action_raw.pdf', width = 6, height = 5,device=cairo_pdf)
cor(tb$IntensityOfReminders,tb$HealthyEatingScore)

ggplot(tb, aes(x = IntensityOfReminders,
               y = HealthyEatingScore, 
               shape = Individual)) + 
  geom_point() + 
  geom_point(aes(x = MeanIntensity,
                 y = MeanScore),
             shape = 3,
             size = 40) +
  theme_pubr() +
  labs(x = "Intensity of Reminders",
       y = "Healthy Eating Score") +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) 
ggsave('fe_action_means.pdf', width = 6, height = 5, device = cairo_pdf)
cor(tb$MeanIntensity,tb$MeanScore)

shapes <- c(16,17,15,3)
p <- 1:4 %>%
  map(function(x)
    ggplot(tb %>% dplyr::filter(Individual == levels(tb$Individual)[x]),
           aes(x = Intensity.R, y = Score.R)) + 
      geom_point(shape = shapes[x]) + 
      geom_vline(aes(xintercept = 0)) + 
      geom_hline(aes(yintercept = 0)) + 
      theme_pubr() +
      labs(x = 'Within Variation in Intensity',
           y = 'Within Variation in Score',
           title = levels(tb$Individual)[x]) +
      theme(text         = element_text(size = 13, family="Garamond"),
            axis.title.x = element_text(size = 13, family="Garamond"),
            axis.title.y = element_text(size = 13, family= "Garamond")) 
  )
plot_grid(plotlist = p, ncol = 2)
ggsave('fe_action_individual.pdf',width = 7, height = 6, units = 'in', device = cairo_pdf)

ggplot(tb, aes(x = Intensity.R,
               y = Score.R, 
               shape = Individual)) + 
  geom_point() +
  geom_vline(aes(xintercept = 0)) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_pubr() +
  labs(x = "Within Variation in Intensity",
       y = "Within Variation in Score") +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) 
ggsave('fe_action_after.pdf', width = 6, height = 5, device = cairo_pdf)
cor(tb$Intensity.R,tb$Score.R)


# Varying Intercept
# Walk through FE
data(gapminder)
gapminder <- gapminder %>%
  dplyr::filter(country %in% c('India','Brazil'),
         year > 1940)
#Additional data that spans the box

singleline <- coef(lm(lifeExp~log(gdpPercap),data=gapminder))
doubleline <- coef(lm(lifeExp~-1+factor(country)+log(gdpPercap),data=gapminder))

ggplot(gapminder, aes(x = gdpPercap,y=lifeExp,
                      color=country,label=year)) + 
  geom_text() + 
  scale_x_log10() +
  scale_color_manual(values = c('Brazil' = 'black','India' = '#929090')) + 
  annotate('text',x = 1250, y = 65, label = 'India', color = '#929090') + 
  annotate('text',x = 7000, y = 57, label = 'Brazil', color = 'black') + 
  guides(color = FALSE) + 
  theme_pubr() + 
  geom_smooth(aes(color=NULL),method='lm', se = FALSE, color = 'black', linetype = 'dashed') + 
  labs(x = 'GDP per Capita (log scale)',
       y = 'Life Expectancy') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))
ggsave('fe_intercept_shared.pdf', width = 6, height = 5, device = cairo_pdf)

ggplot(gapminder, aes(x = gdpPercap,y=lifeExp,
                      color=country,label=year)) + 
  geom_text() + 
  scale_x_log10() +
  scale_color_manual(values = c('Brazil' = 'black','India' = '#929090')) + 
  annotate('text',x = 1250, y = 65, label = 'India', color = '#929090') + 
  annotate('text',x = 7000, y = 57, label = 'Brazil', color = 'black') + 
  guides(color = FALSE) + 
  theme_pubr() + 
  labs(x = 'GDP per Capita (log scale)',
       y = 'Life Expectancy') +
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"))  +
  stat_function(fun = function(x) doubleline[2]+doubleline[3]*log(x),
                color = '#929090') +
  stat_function(fun = function(x) doubleline[1]+doubleline[3]*log(x),
                color = 'black')
ggsave('fe_intercept_separate.pdf', width = 6, height = 5, device = cairo_pdf)


#Gapminder FE
library(tidyverse)

gapminder <- read_csv('gapminder.csv')
gapminder <- gapminder %>%
  # Put GDP per capita in log format since it's very skewed
  mutate(log_GDPperCap = log(gdpPercap)) %>%
  # Perform each calculation by group
  group_by(country) %>%
  # Get within variation by subtracting out the mean
  mutate(lifeExp_within = lifeExp - mean(lifeExp),
         log_GDPperCap_within = log_GDPperCap - mean(log_GDPperCap)) %>%
  # We no longer need the grouping
  ungroup()

# Analyze the within variation
m1 <-lm(lifeExp_within ~ log_GDPperCap_within, data = gapminder)

# With dummies
m2 <- lm(lifeExp ~ factor(country) + log_GDPperCap, data = gapminder)
knitr::opts_current$set(label = 'fixedeffects-basic-regs')
msummary(list('Life Expectancy (within)' = m1, 'Life Expectancy' = m2), output = 'fixedeffects_basic_regs.tex', 
         coef_map = c('log_GDPperCap' = 'Log GDP per Capita',
                      'log_GDPperCap_within' = 'Log GDP per Capita (within)'),
         gof_omit = 'Adj|AIC|BIC|Lik|F',
         stars = TRUE)
coef(m2)['factor(country)India']
coef(m2)['factor(country)Brazil']



# Multiple FEs
library(tidyverse); library(lfe)

gm <- read_csv('gapminder.csv')

gm <- gm %>%
  # Put GDP per capita in log format since it's very skewed
  mutate(log_GDPperCap = log(gdpPercap))

# Run our two-way fixed effects model (TWFE)
# First the non-fixed effects part of the model
# Then a |, then the fixed effects we want
twfe <- felm(lifeExp ~ log_GDPperCap | country + year,
             data = gm)
export_summs(twfe)



# Multiple FEs
library(tidyverse); library(jtools); library(lfe)

gm <- read_csv('gapminder.csv')

gm <- gm %>%
  # Put GDP per capita in log format since it's very skewed
  mutate(log_GDPperCap = log(gdpPercap))

# | separates each "section" of the formula.
# Clusters go in the fourth section, and we 
# don't need the third (instrumental variables)
# So we skip it with 0.
clfe <- felm(lifeExp ~ log_GDPperCap | country
             | 0 | country,
             data = gm)
export_summs(clfe)
