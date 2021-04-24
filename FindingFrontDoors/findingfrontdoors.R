{
  library(tidyverse)
  library(vtable)
  library(Cairo)
  library(extrafont)
  library(directlabels)
  library(ggpubr)
}


# Lottery study
lottery <- read_csv('lottery_data.csv', col_names = c('Years from Winning','Probability of Bankruptcy')) %>%
  mutate(group = c(rep('Large Amount\n($50-$150,000)',10),rep('Small Amount\n(< $10,000)',10))) %>%
  mutate(`Years from Winning` = round(`Years from Winning`))
ggplot(lottery, aes(x = `Years from Winning`, y = `Probability of Bankruptcy`, linetype = group, shape = group)) + 
  geom_line(size = 2) + 
  geom_point(size = 4) + 
  scale_shape_manual(values = c(15,16)) +
  labs(y = 'Prob. of Bankruptcy Relative to Annual Average')+
  geom_vline(aes(xintercept = 0)) +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        panel.grid.major.y = element_line(size = 1)) + 
  guides(linetype = FALSE, shape = FALSE) + 
  annotate(geom = 'text', x = 2.5, y = .006, hjust = 1, label = 'Large Amount\n($50-$150,000)', family = 'Garamond', size = 13/.pt) +
  annotate(geom = 'text', x = 1,5, y = .0025, hjust = .5, label = 'Small Amount\n(< $10,000)', family = 'Garamond', size = 13/.pt)
ggsave('lottery.pdf', width = 7, height = 5, units = 'in', device = cairo_pdf)
