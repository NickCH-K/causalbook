{
  library(tidyverse)
  library(extrafont)
  library(ggpubr)
  library(cowplot)
}


set.seed(1000)
tb <- tibble(W = rnorm(100), Z = sample(0:1, 100, replace = TRUE)) %>%
  mutate(X = 2*W - 2*Z + rnorm(100)) %>%
  mutate(Y = 2*W - .5*X + rnorm(100)) %>%
  group_by(Z) %>% 
  mutate(mean_X = mean(X),
         mean_Y = mean(Y))
mns <- tb %>%
  group_by(Z) %>%
  summarize(X = mean(X), Y = mean(Y))

p1 <- ggplot(tb, aes(x = X, y = Y, shape = factor(Z))) + 
  geom_point(color = 'black') +
  scale_shape_manual(values = c(1,19)) + 
  labs(shape = 'Instrument\nValues',
       title = '(a) Raw Data') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.85,.9),
        legend.background = element_rect(size = .5, color = 'black'))

p2 <- ggplot(tb, aes(x = X, y = Y, shape = factor(Z))) + 
  geom_point(color = 'black') +
  geom_vline(aes(xintercept = mns$X[1]), linetype = 'dashed', color = 'black') +
  geom_vline(aes(xintercept = mns$X[2]), linetype = 'dashed', color = 'black') +
  annotate(geom = 'label', x = mns$X[1], y = 2, label = 'X mean for\nZ = 0',
           family = 'Garamond', size = 13/.pt, hjust = 0) +
  annotate(geom = 'label', x = mns$X[2], y = 2, label = 'X mean for\nZ = 1',
           family = 'Garamond', size = 13/.pt, hjust = 1) +
  scale_shape_manual(values = c(1,19)) + 
  guides(shape = FALSE) +
  labs(title = '(b) Predict X with Z') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.9,.9),
        legend.background = element_rect(size = .5, color = 'black'))

p3 <- ggplot(tb, aes(x = mean_X, y = Y, shape = factor(Z))) + 
  geom_point(color = 'black') +
  geom_vline(aes(xintercept = mns$X[1]), linetype = 'dashed', color = 'black') +
  geom_vline(aes(xintercept = mns$X[2]), linetype = 'dashed', color = 'black') +
  annotate(geom = 'label', x = mns$X[1], y = 2, label = 'X mean for\nZ = 0',
           family = 'Garamond', size = 13/.pt, hjust = 0) +
  annotate(geom = 'label', x = mns$X[2], y = 2, label = 'X mean for\nZ = 1',
           family = 'Garamond', size = 13/.pt, hjust = 1) +
  annotate(geom = 'segment', x = -4, xend = mns$X[2]-.1, y = 1, yend = 1, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = -1, xend = mns$X[2]+.1, y = 1, yend = 1, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = -1, xend = mns$X[1]-.1, y = .5, yend = .5, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = 2, xend = mns$X[1]+.1, y = .5, yend = .5, arrow = arrow(length = unit(0.03, "npc"))) +
  scale_shape_manual(values = c(1,19)) + 
  scale_x_continuous(limits = c(min(tb$X),max(tb$X))) +
  guides(shape = FALSE) +
  labs(title = '(c) Only Use Predicted Variation',
       x = 'X') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.9,.9),
        legend.background = element_rect(size = .5, color = 'black'))

p4 <- ggplot(tb, aes(x = mean_X, y = Y, shape = factor(Z))) + 
  geom_point(color = 'black') +
  geom_hline(aes(yintercept = mns$Y[1]), linetype = 'dashed', color = 'black') +
  geom_hline(aes(yintercept = mns$Y[2]), linetype = 'dashed', color = 'black') +
  annotate(geom = 'label', y = mns$Y[1], x = 4, label = 'Y mean for\nZ = 0',
           family = 'Garamond', size = 13/.pt, vjust = 1) +
  annotate(geom = 'label', y = mns$Y[2], x = 4, label = 'Y mean for\nZ = 1',
           family = 'Garamond', size = 13/.pt, vjust = 0) +
  scale_shape_manual(values = c(1,19)) + 
  scale_x_continuous(limits = c(min(tb$X),max(tb$X))) +
  guides(shape = FALSE) +
  labs(title = '(d) Predict Y with Z',
       x = 'X') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.9,.9),
        legend.background = element_rect(size = .5, color = 'black'))

p5 <- ggplot(tb %>% group_by(Z) %>% slice(1), aes(x = mean_X, y = mean_Y, shape = factor(Z))) + 
  geom_point(color = 'black', size = 3) +
  geom_hline(aes(yintercept = mns$Y[1]), linetype = 'dashed', color = 'black') +
  geom_hline(aes(yintercept = mns$Y[2]), linetype = 'dashed', color = 'black') +
  annotate(geom = 'label', y = mns$Y[1], x = 4, label = 'Y mean for\nZ = 0',
           family = 'Garamond', size = 13/.pt, vjust = 1) +
  annotate(geom = 'label', y = mns$Y[2], x = 4, label = 'Y mean for\nZ = 1',
           family = 'Garamond', size = 13/.pt, vjust = 0) +
  annotate(geom = 'segment', x = mns$X[1], xend = mns$X[1], y = mns$Y[1] + 1.1, yend = mns$Y[1]+.3, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = mns$X[1], xend = mns$X[1], y = mns$Y[1] - 1.6, yend = mns$Y[1]-.3, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = mns$X[2], xend = mns$X[2], y = mns$Y[2] + 1.6, yend = mns$Y[2]+.3, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'segment', x = mns$X[2], xend = mns$X[2], y = mns$Y[2] - 1.1, yend = mns$Y[2]-.3, arrow = arrow(length = unit(0.03, "npc"))) +
  scale_shape_manual(values = c(1,19)) + 
  scale_x_continuous(limits = c(min(tb$X),max(tb$X))) +
  scale_y_continuous(limits = c(min(tb$Y),max(tb$Y))) +
  guides(shape = FALSE) +
  labs(title = '(e) Only Use Predicted Variation',
       x = 'X',
       y = 'Y') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.9,.9),
        legend.background = element_rect(size = .5, color = 'black'))

p6 <- ggplot(tb %>% group_by(Z) %>% slice(1), aes(x = mean_X, y = mean_Y)) + 
  geom_line(size = 1) +
  geom_point(aes(shape = factor(Z)), color = 'black', size = 3) +
  annotate(geom = 'text', x = mns$X[1], y = mns$Y[2], label = 'Slope = IV effect',
           family = 'Garamond', size = 15/.pt, hjust = 0) +
  scale_shape_manual(values = c(1,19)) + 
  scale_x_continuous(limits = c(min(tb$X),max(tb$X))) +
  scale_y_continuous(limits = c(min(tb$Y),max(tb$Y))) +
  guides(shape = FALSE) +
  labs(title = '(f) Relate Predicted Y to Predicted X',
       x = 'X',
       y = 'Y') +
  theme_pubr() + 
  theme(text         = element_text(size = 13, family="Garamond"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family= "Garamond"),
        axis.title.y = element_text(size = 13, family= "Garamond"),
        axis.ticks = element_blank(),
        legend.position = c(.9,.9),
        legend.background = element_rect(size = .5, color = 'black'))

plot_grid(p1,p2,p3,p4,p5,p6, ncol = 2)
ggsave('iv_anim.pdf',width = 7, height = 9.5, units = 'in', device = cairo_pdf)
