library(tidyverse)
library(patchwork)

sims = tibble(sample = seq(1:10000))

set.seed(7272)
n = 1004
samples = rbinom(n = 10000, size = n, prob = 0.39)
props = samples/n
sims = sims |>
  mutate(props = props)

ggplot(data = sims) +
  geom_histogram(aes(x = props, y = after_stat(density)), color = 'grey')+
  geom_density(aes(x = props, y = after_stat(density))) +
  geom_hline(yintercept = 0) +
  labs(x = 'Proportion',
       y = 'Density',
       title = '10,000 Random Binomial samples of size 1004') +
  theme_bw()

range.95 = unname(quantile(sims$props, probs = c(0.025, 0.975)))
lower.95 = range.95[1]
upper.95 = range.95[2]

(MOE.est = (upper.95 - lower.95)/2)


## Doubled sample size

sims2 = tibble(sample = seq(1:10000))

n = n*2

samples = rbinom(n = 10000, size = n, prob = 0.39)
props = samples/n
sims2 = sims2 |>
  mutate(props = props)

ggplot(data = sims2) +
  geom_histogram(aes(x = props, y = after_stat(density)), color = 'grey')+
  geom_density(aes(x = props, y = after_stat(density))) +
  geom_hline(yintercept = 0) +
  labs(x = 'Proportion',
       y = 'Density',
       title = '10,000 Random Binomial samples of size 2008') +
  theme_bw()

range.2 = unname(quantile(sims2$props, probs = c(0.025, 0.975)))
lower.2 = range.2[1]
upper.2 = range.2[2]

(MOE.2 = (upper.2 - lower.2)/2)

########################################################################
# PART 2
########################################################################

answer = 1:1004
answer[1:1004*0.39] = 'Satisfied'
answer[(1004*0.39+1):(1004*0.39+1+1004*0.59)] = 'Dissatisfied'
answer[(1004*0.39+1+1004*0.59+1):1005] = 'No Opinion'

gallup.dat = tibble(participant = 1:1004,
                    response = answer)
probs = tibble(iteration = 1:10000)
prob = 1:10000

for (i in 1:10000){
  set.seed(7272+i)
  gallup.dat = gallup.dat |>
    mutate(!!as.character(i) := sample(response, size = length(response), replace = T))
  prob[i] = sum(gallup.dat[[as.character(i)]] == 'Satisfied')/1004
}

probs = probs |>
  mutate(proportions = prob)

ggplot(data = probs) +
  geom_histogram(aes(x = proportions, y = after_stat(density)),
                 color = 'grey')+
  geom_density(aes(x = proportions, y = after_stat(density))) +
  geom_hline(yintercept = 0) +
  labs(x = 'Proportion',
       y = 'Density',
       title = 'Sampling Distribution by Resampling 10,000 Times') +
  theme_bw()

range.resample = unname(quantile(probs$proportions, probs = c(0.025, 0.975)))
lower.resample = range.resample[1]
upper.resample = range.resample[2]

(resample.MOE = (upper.resample - lower.resample)/2)

MOEs = tibble(Sample = c('rbinom size 1004', 'rbinom size 2008', 'resample'),
              MOE = c(MOE.est, MOE.2, resample.MOE))

################################################################################
### PART 3
################################################################################


n = seq(100, 3000, by = 10)
p = seq(0.01, 0.99, by = 0.01)



simulation = function (n, p){
  
  simulations = tibble(sample = seq(1:10000))
  
  samples = rbinom(n = 10000, size = n, prob = p)
  props = samples/n
  
  range.95 = unname(quantile(props, probs = c(0.025, 0.975)))
  lower.95 = range.95[1]
  upper.95 = range.95[2]
  MOE = (upper.95 - lower.95)/2
  
  return(MOE)
}

params = expand.grid(n = n, p = p)


params$MOE = mapply(simulation, params$n, params$p)

params = params |>
  rename("MOE Estimate" = 'MOE')



estimate.plot = ggplot(data = params) + 
  geom_raster(aes(x = n, y = p, fill = `MOE Estimate`)) +
  scale_fill_viridis_c(direction = -1) +
  labs(title = "MOE estimate as a Function of n and p",
    x = "Sample Size (n)",
    y = "Probability (p)",
    fill = "MOE Estimate") +
  theme_minimal()

wilson = function (n, p){
  z = qnorm(0.975)
  
  simulations = tibble(sample = seq(1:10000))
  
  samples = rbinom(n = 10000, size = n, prob = p)
  props = samples/n
  
  MOE = z*((sqrt(n*p*(1-p)+((z^2)/4))/(n+z^2)))

  return(MOE)
}

params = params |>
  mutate(Wilson = mapply(wilson, params$n, params$p))

wilson.plot = ggplot(data = params) + 
  geom_raster(aes(x = n, y = p, fill = Wilson)) +
  scale_fill_viridis_c(direction = -1) +
  labs(title = "Wilson's MOE as a Function of n and p",
       x = "Sample Size (n)",
       y = "Probability (p)",
       fill = "Wilson's MOE") +
  theme_minimal()

estimate.plot + wilson.plot
