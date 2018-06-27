library(dplyr)

sdat <- read.csv("sim05.csv") %>%
  rename(sim_num = X) %>%
  mutate(sim_group = floor(sim_num)) %>%
  as_tibble()

# sdat <- readr::read_csv("sim05.csv") %>%
#   rename(sim_num = X1) %>%
#   mutate(sim_group = floor(sim_num))

save(sdat, file = "data/sdat.Rdata")

##
##---------------------------------------------------------

load("data/sim04_p2_res.Rdata")
dftt$sim_num <- as.numeric(rownames(dftt))
dftt$sim_group = floor(dftt$sim_num)
sdat <- dftt
sdat$Nt <- sdat$Nt / 3
sdat <- as_tibble(sdat)
save(sdat, file = "data/sdat2.Rdata")

