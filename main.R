library(odin)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coda)
library(pracma)

source("R/functions.R")

##########################################################
# load model object
x <- odin::odin("R/odin_model.R")

##########################################################
init_conds_from_file <- 0
save_init_conds <- 1
max_t <- 2000

# mixing matrix - this is an adapted matrix from Mossong et al
mixing <- as.matrix(read.csv("data/mixing_75.csv", header = TRUE), header = TRUE)*365/12

##########################################################
# run model
r <- run_rsv_model(b0 = 0.015421,
                   b1 = 0.39722,
                   prop_detected_1 = 0.424,
                   prop_detected_2 = 0.088,
                   prop_detected_3 = 0.047,
                   prop_detected_4 = 0.020,
                   max_t = max_t,
                   mixing = mixing,
                   obs = obs,
                   init_conds_from_file = 0)

plot(r$I[,5])
q <- r$I[,5]
q <- findpeaks(q)

if (save_init_conds == 1) {
  S0 <- as.vector(t(data.table::last(r$S)))
  E0 <- as.vector(t(data.table::last(r$E)))
  I0 <- as.vector(t(data.table::last(r$I)))
  R0 <- as.vector(t(data.table::last(r$R)))
  init_conds_save <- as.data.frame(cbind(S0, E0, I0, R0))
  saveRDS(init_conds_save, "init_conds.rds")
}

# get second-last peak, and select range of model output accordingly
p_index <- q[(dim(q)[1]-1),2]
index_start <- p_index - (12*12) - 6
index_end <- p_index + 5
index_end - index_start + 1

##################################################################
# Plot infectious class to check solution has converged
plot(r$time, r$I[,3])
lines(r$time, r$I[,3])

# keep the time period of interest
r_Incidence <- as.data.frame(r$Incidence[index_start:index_end,])
r_DetIncidence <- as.data.frame(r$DetIncidence[index_start:index_end,]) %>%
  mutate(DetInc_1_3 = V1 + V2 + V3,
         DetInc_4_6 = V4 + V5 + V6,
         DetInc_7_12 = V7 + V8 + V9 + V10 + V11 + V12,
         DetInc_13_24 = V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24)
r_DetIncidence$time <- 1:156

# reformat outputs for ggplot   
xp1 <- r_DetIncidence %>%
  mutate(time = time - min(time) + 1) %>%
  select(time, DetInc_1_3, DetInc_4_6, DetInc_7_12, DetInc_13_24) %>%
  pivot_longer(!time)
colnames(xp1)[2] <- "age"

g1 <- ggplot() +
  geom_line(data = xp1, aes(x = time, y = value, col = age)) +
  facet_wrap(~age)

g1

