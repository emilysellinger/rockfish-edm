
# Load pacakages ----------------------------------------------------------
library(NatParksPalettes)
library(gridExtra)

# Chapter 2 Figures -------------------------------------------------------
# expanding window figure

ex_df <- tibble(year = seq(1, 20),
                value = runif(20, min = 5, max = 25))
point_pred <- tibble(year = c(11, 12, 13, 14),
                     preds = c(15, 8, 6, 14),
                     fac = as.factor(c(1,1,2,2)))

a <- ggplot() + geom_line(data = ex_df, aes(x = year, y = value)) + 
  labs(x = "Year", y = "Value") + 
  geom_point(data = point_pred, aes(x = year, y = preds, color = fac), size = 3,
             color = c("#00A1B7", "#00A1B7", "#898928", "#898928")) +
  geom_rect(aes(xmin = 1, xmax = 10, ymin = 5, ymax = 25), fill = "#00A1B7", alpha = 0.5) +
  geom_rect(aes(xmin = 1, xmax = 12, ymin = 5, ymax = 25), fill = "#898928", alpha = 0.3) +
  theme(legend.position = "none") +
  theme_minimal()

pdf(here("results/expanding_window_explainer.pdf"))
print(a)
dev.off()