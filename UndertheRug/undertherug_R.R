{
  library(tidyverse)
  library(extrafont)
  library(ggpubr)
  library(cowplot)
}

xm = .5
a = .9
d <- tibble(x = (10:1000)/100) %>%
  mutate(p = a*(xm^a)/(x^(a+1)))

ggplot(d, aes(x = x, y = p)) + 
  geom_line(size = 1) +
  annotate(x = 7.5,xend = 10, y = 20, yend = 20, geom = 'segment', arrow = arrow()) +
  annotate(x = 10, y = 20, hjust = 1, geom = 'text', family = 'Garamond', size = 13/.pt,
           label = '6.7% of the area under the curve\nis thataway!\n\n.8% past 100\n.1% past 1000!') +
  labs(x = 'X', y = "Density") + 
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('pareto.pdf', width = 6, height = 5,device=cairo_pdf)

(xm/10)^a
(xm/100)^a
(xm/1000)^a
(xm/1)^a  
(1-pnorm(5))*100

rpar <- function(n, xm, a) {
  v <- runif(n)
  xm / v^(1.0/a)
}

set.seed(1000)
sapply(1:10, function(x) mean(rpar(100000, xm, a)))
set.seed(1000)
s <-sapply(1:10, function(x) sd(rpar(100000, xm, a)))
min(s)
max(s)
