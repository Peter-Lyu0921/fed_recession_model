library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

recession_data <- import_data()
recession_data$recession_in_past_one_year <- NA
recession_data$recession_in_past_one_and_half_year <- NA
nber_rec_data <- recession_data$NBER_Rec

for(i in 1:744){
  start_year <- i + 1
  end_year <- i + 12
  
  sum_recession_in_past_one_year <- sum(
    nber_rec_data[start_year : end_year]
  )

  if(sum_recession_in_past_one_year > 0){
    recession_data[i,8] <-1
  } else {
    recession_data[i,8] <-0
  }
}


for(i in 1:738){
  start_year <- i + 1
  end_year <- i + 18
  
  sum_recession_in_past_one_year <- sum(
    nber_rec_data[start_year : end_year]
  )
  
  if(sum_recession_in_past_one_year > 0){
    recession_data[i,9] <-1
  } else {
    recession_data[i,9] <-0
  }
}

one_year_logistic_fit <- glm(recession_in_past_one_year ~ Spread, data = recession_data, family = binomial(link = "probit"))
one_and_half_year_logistic_fit <- glm(recession_in_past_one_and_half_year ~ Spread, data = recession_data, family = binomial(link = "probit"))

recession_data$one_year_prob <- NA
recession_data$one_and_half_year_prob <- NA

recession_data[1:744,]$one_year_prob <- predict(one_year_logistic_fit, type = "response")
recession_data[1:738,]$one_and_half_year_prob <- predict(one_and_half_year_logistic_fit, type = "response")

recession_data$one_year_pred <- 0
recession_data$one_and_half_year_pred <- 0

for(i in 1:744){
  if(recession_data[i,10] > 0.20){
    recession_data[i, 12] <- 1
  } else {
    recession_data[i, 12] <- 0
  }
}

table(recession_data$recession_in_past_one_year, recession_data$one_year_pred)

prob_data <- recession_data%>%
  select(Date, Rec_prob, one_year_prob, one_and_half_year_prob)%>%
  pivot_longer(., cols = c(Rec_prob, one_year_prob, one_and_half_year_prob), names_to = "prob_type", values_to = "prob_number")
ggplot(data = prob_data, aes(x = Date, y = prob_number, col = prob_type)) + geom_line()

fig <- plot_ly(data = recession_data, x = ~Date, y = ~Rec_prob, name = 'Actual Probability', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~one_year_prob, name = 'One Year Probability', mode = 'lines') %>%
  add_trace(y = ~one_and_half_year_prob, name = 'One and Half Year Probability', mode = 'lines')


spread_fig <- plot_ly(data = recession_data, x = ~Date, y = ~Spread, name = "Yield Curve Spread", type = 'scatter', mode = 'lines')

spread_fig <- layout(spread_fig, title = "Yield Curve Spread", 
                     shapes = list(
                       type = "rect",
                       fillcolor = "red",
                       opacity = 0.3,
                       x0 = "1970-01-01", x1 = "2022-01-01", xref = "x",
                       y0 = -3, y1 = 4, yref = "y"
                     ))

spread_fig
