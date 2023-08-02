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
dat <- read.csv(here("proposal_items", "simplex_example_data.csv"))
dat[62:64,3] <- 8
dat[65,3] <- 5

dat %>% filter(Color == 5) %>% mutate(mean = mean(Value))
dat2 <- data.frame(Time = 70, Value = -0.26138, Color = 5)

a <- dat %>% 
  ggplot(aes(Time, Value)) +
  geom_line(data = dat %>% filter(Time < 70), alpha = 0.5) +
  geom_line(data = dat %>% filter(Color == 4), color = "#586028") +
  geom_line(data = dat %>% filter(Color == 6), color = "#586028", size = 1.25) +
  geom_line(data = dat %>% filter(Color == 7), color = "#586028", size = 1.25) +
  geom_line(data = dat %>% filter(Color == 8), color = "#586028", size = 1.25) +
  geom_line(data = dat %>% filter(Color == 3), color = "#00A1B7", size = 1.25) +
  geom_point(data = dat %>% filter(Time < 71), shape = 1, size = 4) +
  geom_point(data = dat %>% filter(Color == 2), color = "#00A1B7", size = 4, alpha = 0.5) +
  geom_point(data = dat %>% filter(Color == 5), color = "#898928", size = 4) +
  geom_point(data = dat %>% filter(Color == 4), color = "#586028", size = 4) +
  geom_point(data = dat %>% filter(Color == 6), color = "#586028", size = 4) +
  geom_point(data = dat %>% filter(Color == 7), color = "#586028", size = 4) +
  geom_point(data = dat %>% filter(Color == 8), color = "#586028", size = 4) +
  geom_point(data = dat %>% filter(Color == 3), color = "#00A1B7", size = 4) +
  geom_point(data = dat2, color = "#898928", size = 4, alpha = 0.8) +
  annotate(geom = "text", x = 73.1, y = 0.035, label = "obs")+
  annotate(geom = "text", x = 73.8, y = -0.26, label = "pred")+
  xlim(1,75) + theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank())


pdf(here('proposal_items/edm_explainer_graph.pdf'), width = 9)
print(a)
dev.off()

# 3d graph
library(rgl)

dat3 <- data.frame(x = runif(20,1,4), y = runif(20,1,5), z = runif(20,1,8))

dat3$col <- c(rep("#9DA7BF", 3), "#898928", 
              rep("#9DA7BF", 3), "#00A1B7", rep("#9DA7BF", 3), "#00A1B7", rep("#9DA7BF", 8))

plot3d(x = dat3$x, y=dat3$y, z=dat3$z, xlab = "x", ylab = "x-1", zlab = "x-2",
       size = 6.5, axes = FALSE, col = dat3$col)
box3d()
rgl.snapshot("3dplot.png", fmt = "png")
