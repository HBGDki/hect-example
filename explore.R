library(dplyr)
library(ggplot2)

# recorded overall type I error, power, expected sample size, probability of stopping early due to futility and superiority (separately), as well as mean bias of the relative risk reduction upon termination for futility and superiority (separately)

# load("data/sdat.Rdata")
load("data/sdat2.Rdata")

# it looks like there are 108 scenarios of 500 runs each
# figure out which variables determine the runs:
sdat %>%
  group_by(sim_group) %>%
  summarise_all(funs(n = length(unique(.)))) %>%
  select_if(funs(all(. == 1)))

# these variables define each simulation scenario
scen_vars <- c("s", "p1", "p2", "RRR", "sup", "fut")

# do these fully define a group of simulation runs?
length(which(!(duplicated(sdat[scen_vars]))))

# these variables aren't fully crossed
prod(sapply(sdat[scen_vars], function(x) length(unique(x))))

# get unique values of these variables
lapply(sdat[scen_vars], unique)

# s: 0.002 0.010 0.015
# p1: 0.020 0.035 0.050
# p2: 0.09 0.12 0.15
# RRR:  0.0 0.2 0.4 0.6
# sup: 0.950 0.975 0.990
# fut: 0.2 0.3 0.4


sdat[1, ] %>% data.frame()
#     s   p1   p2 RRR  sup fut
# 0.002 0.02 0.09   0 0.95 0.2

##
##---------------------------------------------------------

b <- sdat
b$cer1 <- lookup[as.character(b$s)]
b$cer2 <- lookup[as.character(b$p1)]
b$cer3 <- lookup[as.character(b$p2)]

table(b$cer1 == b$cer2)
table(b$cer1 == b$cer3)

# CER applies across all variables s, p1, p2

##
##---------------------------------------------------------

# Three settings for the CER (base, worst, and best), four settings for the magnitude of effect (RRR of 0%, 20%, 40%, and 60%, and two stopping rule settings that each employed 3x3 superiority and futility bound combinations.

# For each scenario, we recorded overall type I error (i.e. the probability of detecting a significant effect under the assumption that relative risk reduction RRR is 0%) and power (for RRR= 20%, 40%, and 60% scenarios) and expected sample size. We also recorded the probability of stopping early – overall, for superiority, and for futility. Further, we recorded the distribution of frequentist p-values (Fisher exact test for 2x2 tables) at early stopping for superiority and early stopping for futility. Lastly, we recorded the distribution of relative risk reduction estimates and the absolute bias on these overall, at early stopping for superiority and at early stopping for futility.

# is "early" a surrogate for stopping early for superiority or futility?
tmp <- sdat %>%
  filter(supstop == 1 | futstop == 1) %>%
  select(early, reachmax)

table(tmp$early)
#   0     1
# 874 42632
table(tmp$reachmax)
#     0
# 43506
table(sdat$reachmax)
#     0     1
# 43506 10494

# hist(sdat$RRR_hat1[1:500])



##
##---------------------------------------------------------

# CER: control event rate
# For the stringent outcomes, a base case CER of 1% was assumed, with a span of 0.2% to 1.5% for the worst and best-case scenarios, respectively. For the moderately permissive outcome p1, a base case CER of 5% was assumed, with a span of 2% to 8% for the worst and best-case scenarios, respectively. For the highly permissive outcome p2, a base case CER of 12% was assumed, with a span of 9% to 15% for the worst and best-case scenarios, respectively. Across scenarios, we also examined widely varying magnitudes of effect, expressed as the relative risk reduction of L.plantarum vs placebo. We particularly examined ‘true’ relative risk reduction of 0%, 20%, 40%, 60%.

# Experimental event rate (EER) is used to describe the rate that good events occur with the treatment: # good events / # who received treatment
# Control event rate (CER) is used to describe the rate that good events occur with placebo: # good events / # in control group.
# The relative risk reduction is the difference between the EER and CER (EER-CER) divided by the CER, and usually expressed as a percentage.

# Overall, the Bayesian adaptive design stopping for superiority on s and for futility for s and p2 appeared superior to the design only monitoring for p2. The efficiencies for this design also appeared superior to the efficiencies of the group sequential designs. A superiority probability threshold of either 0.975 or 0.99 kept type I error under control. While superiority thresholds of both 0.3 and 0.4 appeared to perform well, a few odd results were observed with 0.4, and for that reason it may be desirable to opt for 0.3 although this may not be the most clinically meaningful. The simulations also showed that (regardless of the design) a very low control event rate can be detrimental to the likelihood of success of the trial.

# Interim analyses were performed for every 1,000 patients, and the trial was terminated either if one of the stopping rules were enforced or if a total of 12,000 patients had been enrolled.

# - pow1, pow2, pow3: binary variable from which power to detect an effect on the three outcomes, s, p1, and p2, respectively can be obtained under each scenario;
# - Nt: sample size at trial termination;
# - early: binary variable indicating if the trial was stopped early;
# - event1, event2, event3: proportion of events (sepsis) with respect to the three definitions (s, p1, p2);
# - supstop: binary variable indicating if the trial was stopped early due to superiority;
# - futstop: binary variable indicating if the trial was stopped early due to futility;
# - reachmax: binary variable indicating if the trial reached maximum sample size (this is redundant as it is 1-early)
# - RRR_hat1, RRR_hat2, RRR_hat3: relative risk reduction estimates for the three outcomes
# - pvalue1,  pvalue2,  pvalue3: p-value of classical frequentist test for the three outcomes


# - If you were presenting this to a program officer who would be making a final decision about the actual trial design to run based on the information coming out of these simulations, what are the most important things you would want to point out? What are the most important tradeoffs? Would you show the range of options and highlight a single recommended option?
# - Is there more I can read about the synbiotic trial? For example, is this intended for a specific population / geography? etc.
# - I presume the enrollment ratio is 1:1? Is that adjustable?
# - Does the data you provided match what is used in the report? Just checking as I'm trying to replicate some of the plots to make sure I'm understanding things correctly. I have attached some code and a plot, for example, trying to replicate Figure 3.
# - Speaking of the plots, they report "expected average" -- I presume the values shown are means/medians of the data for the 500 runs in each scenario?
# - "Interim analyses were performed for every 1,000 patients" - does this mean the trial starts with 1k patients, then enrolls 1,000 more, etc.? Sorry for my lack of knowledge of clinical trials. If this is the case, does each increment of 1,000 have a fixed time period in between it? And if so, what is this time period for the synbiotic trial?
# - For a given simulation scenario, how can I replicate that in the https://sgolchi.shinyapps.io/brats/ tool? For example, take the first scenario: s=0.002, p1=0.02, p2=0.09, RRR=0, sup=0.95, fut=0.2.


table(sdat$pow1 + sdat$pow2 + sdat$pow3)
#     0     1     2     3
# 12483  9828 24795  6894

table(sdat$early)
#     0     1
# 11368 42632

table(sdat$Nt == 12000)
# FALSE  TRUE
# 42632 11368

table(sdat$supstop + sdat$futstop > 0)
# FALSE  TRUE
# 10494 43506
## why is this one different?

length(which(sdat$supstop == 1 & sdat$Nt == 12000))
# 520
length(which(sdat$futstop == 1 & sdat$Nt == 12000))
# 354
## why are these happening?


table(sdat$event1 < sdat$event2)
# FALSE  TRUE
#    53 53947

table(sdat$event2 < sdat$event3)
#  TRUE
# 54000



# http://clincalc.com/stats/samplesize.aspx

# CER 0.2% (2 / 1000)
# RRR 20% = (EER - CER) / CER; EER = 0.2 * 0.002 + 0.002 = 0.0024 (2.4 / 1000)

# CER 15%
# RRR 20%: EER = 0.2 * 0.15 + 0.15 = 0.18


