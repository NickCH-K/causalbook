{
  library(tidyverse)
  library(cowplot)
  library(extrafont)
  library(ggpubr)
  library(vtable)
  library(purrr)
  library(broom)
  library(Synth)
  library(haven)
  library(SortedEffects)
}

#Synthetic control
data(basque)

# dataprep: prepare data for synth
dataprep.out <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 17
    ,controls.identifier   = c(2:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )

# 1. combine highest and second highest 
# schooling category and eliminate highest category
dataprep.out$X1["school.high",] <- 
  dataprep.out$X1["school.high",] + 
  dataprep.out$X1["school.post.high",]
dataprep.out$X1                 <- 
  as.matrix(dataprep.out$X1[
    -which(rownames(dataprep.out$X1)=="school.post.high"),])
dataprep.out$X0["school.high",] <- 
  dataprep.out$X0["school.high",] + 
  dataprep.out$X0["school.post.high",]
dataprep.out$X0                 <- 
  dataprep.out$X0[
    -which(rownames(dataprep.out$X0)=="school.post.high"),]

# 2. make total and compute shares for the schooling catgeories
lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")

dataprep.out$X1[lowest:highest,] <- 
  (100 * dataprep.out$X1[lowest:highest,]) /
  sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-  
  100 * scale(dataprep.out$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out$X0[lowest:highest,])
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out)
dp <- data.frame(Y = c(dataprep.out$Y0plot %*% synth.out$solution.w,
                 dataprep.out$Y1plot),
                 years = rep(1955:1997,2),
                 Treatment = c(rep('Synthetic\nControl',length(1955:1997)),rep('Treated',length(1955:1997))))
ggplot(dp, aes(x = years, linetype =  Treatment, y = Y)) + 
  geom_line(size = 1) + 
  geom_text(data = dp %>% filter(years == 1997),
            mapping = aes(x = years, y = Y, label = Treatment),
            hjust = -.1, family = 'Garamond',size = 13/.pt) +
  geom_vline(aes(xintercept = 1970), linetype = 'dashed') +
  expand_limits(x = 2005) +
  labs(x = 'Year', y = 'Real per-capita GDP\n(1986 USD, thousand)') +
  scale_linetype_manual(values = c('dashed','solid')) + 
  theme_pubr() + 
  guides(linetype = FALSE) +
  theme(text         = element_text(size = 13, family="Garamond"))
ggsave('abadie_conflict.pdf', width = 6, height = 5,device=cairo_pdf)


# Oster Sorted Effects
d <- read_dta('../DescribingRelationships/nhanes_summary_cleaned.dta') %>%
  mutate(treated = 1*(year %in% c(1999,2001,2003))) %>%
  mutate(vite_take = supplement_vite_single == 1) %>%
  select(vite_take, treated, smoke_now, exercise_std, behavior_vitamins) %>% 
  na.omit()

# interaction function
f <- as.formula('vite_take ~ treated + treated*smoke_now + treated*exercise_std + treated*behavior_vitamins')
summary(glm(f, data = d, family = binomial()))
m <- spe(f, d, method = 'ols',var = 'treated', b = 500)
spedat <- data.frame(summary(m))
spedat$qua <- as.numeric(rownames(spedat))
ggplot(spedat, aes(x = qua, y = Est, ymin = X90..ULB, ymax = X90..UUB)) + 
  geom_ribbon(fill = 'gray') + 
  geom_line(size = 1) + 
  geom_hline(aes(yintercept = m$ape$est_bc)) +
  geom_hline(aes(yintercept = m$ape$ubound_est_bc), linetype = 'dashed') +
  geom_hline(aes(yintercept = m$ape$lbound_est_bc), linetype = 'dashed') + 
  expand_limits(x = 1.15) +
  annotate(geom = 'text', x = 1.05, y = tail(spedat$Est,1),
           family = 'Garamond', size = 13/.pt, label = 'Effect Quantile\n(and Conf. Int.)') +
  annotate(geom = 'text', x = 1.05, y = m$ape$est_bc, vjust = 2,
           family = 'Garamond', size = 13/.pt, label = 'Average Effect\n(and Conf. Int.)') +
  labs(x = 'Quantile of Effect',
       y = 'Estimated Effect') +
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('spe.pdf', width = 6, height = 5,device=cairo_pdf)

subp <- subpop(f, d, method = 'ols',var = 'treated', b = 500)
summary(subp)
d <- bind_rows(subp$most %>% mutate(cat = '10% Most Affected'),
               subp$least %>% mutate(cat = '10% Least Affected'))
d %>%
  rename(`Effect:` = cat) %>%
  select(-vite_take, -treated) %>%
  sumtable(group = 'Effect:', summ = c('mean(x)','sd(x)'), summ.names = c('Mean', 'SD'),
           factor.numeric = TRUE,
           digits = 3,
           labels = c('Smoking','Exercise Rating Score','Vitamin Behavior Score'),
           anchor = 'tab:galleryofrogues-spetab',
           out = 'latex',
           file = 'spe_mostleast.tex',
           title = 'Characteristics of Those Least and Most Responsive to Health Recommendation for Vitamin E')
