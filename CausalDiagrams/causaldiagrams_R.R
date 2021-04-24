library(tidyverse)
library(Cairo)
library(extrafont)
library(ggpubr)

# Police and crime
library(wooldridge)
data(crime4)

ggplot(crime4, aes(x = polpc, y = crmrte)) + 
  geom_point() + 
  theme_pubr() + 
  scale_x_log10() + 
  scale_y_log10() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.title.x = element_text(size = 13, family="Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond")) +
  labs(x = "Police per Capita (log scale)",
       y = "Crime Rate (log scale)")
ggsave('crime1.pdf', width = 6, height = 5, device=cairo_pdf)
