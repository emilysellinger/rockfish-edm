library(rEDM)
library(ggplot2)
library(here)
library(tidyverse)
data("TentMapNoise")

ts <- TentMapNoise[1:100,]
plot(ts, type = "l")


lib <- c(1, 69)
pred <- c(70, 100)

out <- simplex(ts, lib = lib, pred = pred)

plot(out$E, out$rho)
out$rho # E = 3

out2 <- simplex(ts, lib, pred, E = 3, stats_only = FALSE)
out2$model_output

ggplot(data = TentMapNoise, aes(x = Time, y = TentMap)) + 
  geom_line() + geom_point(shape = 1) + xlim(1, 70) + ylab("Value")

# read in data
dat <- read.csv(here("data", "simplex_example_data.csv"))
dat[62:64,3] <- 8
dat[65,3] <- 5

dat %>% filter(Color == 5) %>% mutate(mean = mean(Value))
dat2 <- data.frame(Time = 70, Value = -0.26138, Color = 5)

dat %>% 
  ggplot(aes(Time, Value)) +
  geom_line(data = dat %>% filter(Time < 70), alpha = 0.5) +
  geom_line(data = dat %>% filter(Color == 4), color = "blue") +
  geom_line(data = dat %>% filter(Color == 6), color = "blue") +
  geom_line(data = dat %>% filter(Color == 7), color = "blue") +
  geom_line(data = dat %>% filter(Color == 8), color = "blue") +
  geom_line(data = dat %>% filter(Color == 3), color = "red") +
  geom_point(data = dat %>% filter(Time < 71), shape = 1, size = 4) +
  geom_point(data = dat %>% filter(Color == 2), color = "red", size = 4, alpha = 0.5) +
  geom_point(data = dat %>% filter(Color == 5), color = "green", size = 4) +
  geom_point(data = dat %>% filter(Color == 4), color = "blue", size = 4) +
  geom_point(data = dat %>% filter(Color == 6), color = "blue", size = 4) +
  geom_point(data = dat %>% filter(Color == 7), color = "blue", size = 4) +
  geom_point(data = dat %>% filter(Color == 8), color = "blue", size = 4) +
  geom_point(data = dat %>% filter(Color == 3), color = "red", size = 4) +
  geom_point(data = dat2, color = "green", size = 4, alpha = 0.5) +
  geom_text(aes(73, 0.035), label = "obs")+
  geom_text(aes(73, -0.26), label = "pred")+
  xlim(1,73) + theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank())

# 3d graph
library(rgl)

dat3 <- data.frame(x = runif(20,1,4), y = runif(20,1,5), z = runif(20,1,8))

dat3$col <- c(2,2,4,rep(1,17))
plot3d(x = dat3$x, y=dat3$y, z=dat3$z, xlab = "x", ylab = "x-1", zlab = "x-2",
       size = 5, axes = FALSE, col = dat3$col)
box3d()
rgl.snapshot("3dplot.png", fmt = "png")
