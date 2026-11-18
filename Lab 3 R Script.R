c <- read.csv("/Users/sophiacho/Downloads/Lab 3 NMR 5(Metabolite PCA Data)-3.csv", row.names=NULL)

library(stats)
library(ggplot2)

sample <- c$Sample

num <- c[, !names(c) %in% c("Sample", "Trig_9.02")]

pca <- prcomp(num, scale. = TRUE)

var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

scores <- data.frame(pca$x, Sample = sample)

scores$Type <- ifelse(grepl("Arabica", sample), "Arabica",
                      ifelse(grepl("Robusta", sample), "Robusta",
                             "Unknown"))

library(ggrepel)

ggplot(scores, aes(PC1, PC2, color = Type)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = Sample),
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(x = paste0("PC1 (", var_exp[1], "%)"),
       y = paste0("PC2 (", var_exp[2], "%)"),
       title = "PCA Scores Plot of Coffee Samples") +
  theme_minimal()

library(ggrepel)

loadings <- data.frame(pca$rotation)

loadings$Metabolite <- rownames(loadings)

ggplot(loadings, aes(PC1, PC2)) +
  geom_point(size = 3, color = "darkblue") +
  geom_text_repel(aes(label = Metabolite),
                  box.padding = 0.5,
                  max.overlaps = Inf) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(x = paste0("PC1 (", var_exp[1], "%)"),
       y = paste0("PC2 (", var_exp[2], "%)"),
       title = "PCA Loadings Plot of Coffee Metabolites") +
  theme_minimal()