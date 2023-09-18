#' # Housing Prices Project (part 1)
#'
#' In this file, you'll visualize and model the relationship between square
#' footage (`area`) and house prices (`price`). The main question: is that
#' relationship best modeled as linear, log-linear, or log-log?

library(tidyverse)
houses <- read_csv("https://raw.githubusercontent.com/cobriant/EC320Lab9/main/Housing.csv")

#' # 1. Linear Model (example: I'll do this one for you)
#'
#' First, visualize the relationship between `price` and `area` with a scatterplot.
#' Then fit the model $price = \beta_0 + \beta_1 area + u$ and interpret the
#' slope coefficient's estimate and statistical significance.

houses %>%
  ggplot(aes(x = area, y = price)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

houses %>%
  lm(price ~ area, data = .) %>%
  broom::tidy()

#' When area increases by one square foot, the house is expected to sell for $462
#' more. That estimate is significant at the 1% level.
#'
#' # 2. Log-log Model
#'
#' Visualize the relationship between `log(price)` and `log(area)` with a scatterplot.
#' Then fit the model $log(price) = \beta_0 + \beta_1 log(area) + u$ and interpret the
#' slope coefficient's estimate and statistical significance.
houses %>%
  ggplot(aes(x = log(area), y = log(price))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

houses %>%
  lm(log(price) ~ log(area), data = .) %>%
  broom::tidy()
#' When area increases by one square foot, the house is expected to sell for $.54
#' more. That estimate is significant at the 1% level.
#'
#' # 3. Log-linear Model
#'
#' Visualize the relationship between `log(price)` and `area` with a scatterplot.
#' Then fit the model $log(price) = \beta_0 + \beta_1 area + u$ and interpret the
#' slope coefficient's estimate and statistical significance.
houses %>%
  ggplot(aes(x = area, log(price))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

houses %>%
  lm(log(price) ~ area, data = .) %>%
  broom::tidy()
#'
#' When area increases by one square foot, the house is expected to sell for $.00009
#' more. That estimate is significant at the 1% level.
#' 
#' # 4. When you review the scatterplots from the previous three questions, which
#' of these three models is the better fit? That is, which of these three
#' transformations makes a linear model seem most plausible?
#' the second one looks the best graphically so... log log method
