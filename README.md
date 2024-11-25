---
title: "Analysis Report"
author: "Your Name"
date: "`r Sys.Date()`"
output: 
  pdf_document:
    toc: true
  html_document:
    toc: true
    toc_float: true
---

# Introduction

This report summarizes the analysis, including descriptive statistics, ANOVA, Response Surface Modeling, and Cross-Validation. It also provides various visualizations to interpret the results effectively.

1.  Load Libraries

```{r, message=FALSE}
check_and_install <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

required_packages <- c("dplyr", "ggplot2", "car", "rsm", "caret", "gridExtra", "plotly")

for (pkg in required_packages) {
  check_and_install(pkg)
}

library(dplyr)
library(ggplot2)
library(car)
library(rsm)
library(caret)
library(gridExtra)
library(plotly)
print("All required libraries are installed and loaded.")
```

# 2. Load Dataset

```{r}
data <- read.csv("~/Downloads/dataupdate.csv")
print("Dataset Preview:")
head(data)
```

# 3. Descriptive Statistics

```{r}

summary_stats <- data %>% 
  summarise(across(where(is.numeric), list(
    mean = ~ mean(.),
    sd = ~ sd(.),
    median = ~ median(.),
    min = ~ min(.),
    max = ~ max(.)
  )))

summary_stats
```

# 4. ANOVA Analysis

```{r}
anova_model <- aov(consolidation ~ Bacteria + Cementation + pH + Zeolite + comactive_effort, data = data)
anova_summary <- summary(anova_model)
anova_summary
```

# 5. Response Surface Methodology

```{r}
rsm_model <- rsm(consolidation ~ FO(Bacteria, Cementation, pH, Zeolite, comactive_effort), data = data)
rsm_summary <- summary(rsm_model)
rsm_summary
```

# 6. Cross-Validation

```{r}

control <- trainControl(method = "cv", number = 10, savePredictions = "final")

cv_model <- train(consolidation ~ Bacteria + Cementation + pH + Zeolite + comactive_effort, 
                  data = data, 
                  method = "lm", 
                  trControl = control)

cv_predictions <- cv_model$pred

ggplot(cv_predictions, aes(x = obs, y = pred)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Cross-Validation: Observed vs Predicted",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  theme_minimal()
```

# 7. Visualizations

## Scatterplot Matrix

```{r}
pairs(data %>% select(where(is.numeric)), main = "Scatterplot Matrix")
```

## Boxplot

```{r}
boxplot_plot <- ggplot(data, aes(x = as.factor(Bacteria), y = consolidation)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Consolidation by Bacteria",
    x = "Bacteria",
    y = "Consolidation"
  ) +
  theme_minimal()
boxplot_plot


```

## Contour Plot

```{r}
contour(rsm_model, ~ Bacteria + Cementation, main = "Response Surface: Bacteria and Cementation")
```

# Interaction Plot

```{r}
interaction.plot(data$Bacteria, data$Cementation, data$consolidation, 
                 main = "Interaction Plot: Bacteria and Cementation",
                 xlab = "Bacteria", ylab = "Consolidation")

```

## Combined Visualizations

```{r}
grid.arrange(
  boxplot_plot, 
  ggplot(cv_predictions, aes(x = obs, y = pred)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "Cross-Validation: Observed vs Predicted",
      x = "Observed Values",
      y = "Predicted Values"
    ) +
    theme_minimal(), 
  ncol = 2
)
```

```{r}
fig <- plot_ly(data = data, 
               x = ~Bacteria, 
               y = ~Cementation, 
               z = ~consolidation, 
               type = "scatter3d", 
               mode = "markers", 
               marker = list(size = 5, color = ~consolidation, colorscale = "Viridis", showscale = TRUE)) %>%
  layout(title = "3D Visualization: Bacteria, Cementation, and Consolidation",
         scene = list(
           xaxis = list(title = 'Bacteria'),
           yaxis = list(title = 'Cementation'),
           zaxis = list(title = 'Consolidation')
         ))

fig
```
