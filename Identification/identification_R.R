# Output file for Identification Chapter
{
library(Cairo)
library(tidyverse)
library(haven)
library(extrafont)
library(lubridate)
library(vtable)
library(ggrepel)
library(ggpubr)
}

### WHERE'S YOUR VARIATION?

# AVOCADO
# Weekly Sales of Avocados in California, Jan 2015-March 2018 
# \label{fig:identification-avocado}
dfavo <- read_csv('avocado.csv') %>%
  filter(region == "California",
         type == "conventional")

dfavo <- dfavo %>%
  mutate(`Total Volume (Millions)` = `Total Volume`/1000000) %>% 
  arrange(Date)

ggplot(dfavo, aes(y = `Total Volume (Millions)`, x = AveragePrice)) + 
  geom_point(size = 1)+
  theme_pubr()+
  theme(text         = element_text(size = 16, family="Garamond"),
          axis.title.x = element_text(size = 16, family="Garamond"),
          axis.title.y = element_text(size = 16, family= "Garamond")) +
  labs(y = "Total Avocados Sold (Millions)",
       x = "Average Avocado Price",
       title = "",
       caption = "Data from Hass Avocado Board\nc/o https://www.kaggle.com/neuromusic/avocado-prices/")
ggsave('avocado.pdf', width = 6, height = 5, device = cairo_pdf)

ggplot(dfavo %>% mutate(isolate = row_number() %in% 4:5), 
       aes(y = `Total Volume (Millions)`, x = AveragePrice, alpha = isolate)) + 
  geom_point(size = 2)+
  theme_pubr()+
  guides(alpha = FALSE) + 
  scale_alpha_manual(values = c(0,1))+
  theme(text         = element_text(size = 16, family="Garamond"),
        axis.title.x = element_text(size = 16, family="Garamond"),
        axis.title.y = element_text(size = 16, family= "Garamond")) +
  geom_label_repel(aes(label = as.character(Date)), direction = 'y') + 
  labs(y = "Total Avocados Sold (Millions)",
       x = "Average Avocado Price",
       title = "",
       caption = "Data from Hass Avocado Board\nc/o https://www.kaggle.com/neuromusic/avocado-prices/")
ggsave('avocado2.pdf', width = 6, height = 5, device = cairo_pdf)


ggplot(dfavo %>% slice(5:12) %>%
         mutate(yearmonth = factor(year(Date)*100 + month(Date))) %>%
         mutate(yearmonth = ifelse(yearmonth == 201503, "Mar. 2015","Feb. 2015")) %>%
         mutate(yearmolabel = ifelse(row_number() %in% c(1,5),yearmonth,NA)),
       aes(y = `Total Volume (Millions)`, x = AveragePrice)) + 
  geom_point(size = 2)+
  geom_line(aes(linetype = factor(yearmonth))) + 
  theme_pubr()+
  guides(linetype = FALSE) + 
  theme(text         = element_text(size = 16, family="Garamond"),
        axis.title.x = element_text(size = 16, family="Garamond"),
        axis.title.y = element_text(size = 16, family= "Garamond")) +
  geom_label_repel(aes(label = yearmolabel)) + 
  labs(y = "Total Avocados Sold (Millions)",
       x = "Average Avocado Price",
       title = "",
       caption = "Data from Hass Avocado Board\nc/o https://www.kaggle.com/neuromusic/avocado-prices/")
ggsave('avocado3.pdf', width = 6, height = 5, device = cairo_pdf)


### DATA GENERATING PROCESS

set.seed(987987)

df <- tibble(College = runif(5000) < .3) %>%
  mutate(Hair = case_when(
    runif(5000) < .2+.8*.4*(!College) ~ "Brown",
    TRUE ~ "Other Color")) %>%
  mutate(logIncome = .1*(Hair == "Brown") + .2*College + rnorm(5000) + 5)

ggplot(df %>% filter(Hair == "Brown"), aes(x = logIncome, linetype = Hair)) +
  stat_density(geom = 'line', size = 1) +
  stat_density(data = df %>% filter(Hair == "Other Color"), 
               geom = 'line', size = 1) +
  theme_pubr() + 
  labs(x = "Log Income", y = "Density") + 
  theme(text         = element_text(size = 16, family="Garamond"),
        axis.title.x = element_text(size = 16, family="Garamond"),
        axis.title.y = element_text(size = 16, family= "Garamond"),
        legend.position = c(.2,.8),
        legend.background = element_rect())
ggsave('hair1.pdf', width = 6, height = 5, device = cairo_pdf)

# Raw means

df %>%
  group_by(Hair) %>%
  summarize(`Log Income` = round(mean(logIncome),3)) %>%
  dftoLaTeX(title = "Mean Income by Hair Color",
            file = 'hairtable1.tex',
            anchor = 'tab:identification-hair1')

df %>%
  filter(College) %>%
  group_by(Hair) %>%
  summarize(`Log Income` = round(mean(logIncome),3)) %>%
  dftoLaTeX(title = "Mean Income by Hair Color Among College Students",
            file = 'hairtable2.tex',
            anchor = 'tab:identification-hair2')


# Alcohol and Mortality
# Data extracted from published graph using https://apps.automeris.io/wpd/
alc_mort <- read_csv('alcohol_mortality_digitizer.csv') %>%
  # Identify points by sets of three
  mutate(point = floor((row_number()-1)/3)) %>%
  group_by(point) %>%
  # The actual point is the first one
  mutate(datactr = row_number() == 1) %>%
  # make the x axes all identical
  mutate(X = mean(X)) %>%
  # Make the top and bottom equidistant from center
  pmdplyr::mutate_subset(Ycenter = Y, .filter = datactr) %>%
  pmdplyr::mutate_subset(Ydist = mean(abs(Y - Ycenter)), .filter = !datactr)
ggplot(alc_mort %>% filter(datactr), aes(x = X, y = Y, 
                                         ymin = Y - Ydist, ymax = Y + Ydist)) +
  #geom_errorbar(size = 1,width = .5) + 
  geom_point(size = 2) +
  geom_hline(aes(yintercept = 1), size = .3, linetype = 'dashed') + 
  theme_pubr() + 
  labs(x = "Usual Alcohol Consumption (Grams/wk)", y = "Hazard Ratio (1 = no effect)") + 
  theme(text         = element_text(size = 16, family="Garamond"),
        axis.title.x = element_text(size = 16, family="Garamond"),
        axis.title.y = element_text(size = 16, family= "Garamond"))
ggsave('original_alcohol_mortality.pdf', width = 6, height = 5, device = cairo_pdf)

# Alcohol and Maleness
# Data extracted from published graph using https://apps.automeris.io/wpd/
alc_male <- read_csv('alcohol_male_digitizer.csv') %>%
  #X-axis is on round numbers
  mutate(X = round(X,0),
         # Odds ratio for similarity
         Y = Y/(1-Y))
ggplot(alc_male, aes(x = X, y = Y)) + 
  geom_point(size = 2) +
  geom_hline(aes(yintercept = 1), size = .3, linetype = 'dashed') + 
  theme_pubr() + 
  labs(x = "Drinks Per Week", y = "Odds Ratio (1 = no effect)") + 
  theme(text         = element_text(size = 16, family="Garamond"),
        axis.title.x = element_text(size = 16, family="Garamond"),
        axis.title.y = element_text(size = 16, family= "Garamond"))
ggsave('auld_alcohol_male.pdf', width = 6, height = 5, device = cairo_pdf)
