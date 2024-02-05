library(ggplot2)
library(dygraphs)


# Number of sessions per day ----------------------------------------------
sessions_summary

sessions_summary %>%
  ggplot(aes(x = date, y = n, color = Profile)) +
  geom_line(linewidth = 1) +
  labs(x = "", y = "Number of sessions", title = "Test with {ggplot2}") +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "3 days") +
  # facet_wrap(vars(Profile)) +
  theme_dark()


# Make a column plot of the maximum energy charged by date and user profile,
# with every user profile in a different color
sessions_summary %>%
  ggplot(aes(x = date, y = max_kWh, color = Profile)) +
  geom_col()

sessions_summary %>%
  ggplot(aes(x = date, y = max_kWh, color = Profile, fill = Profile)) +
  geom_col(position = "dodge")



# Statistics --------------------------------------------------------------

# Histogram
sessions_profiles2 %>%
  ggplot(aes(x = Energy)) +
  geom_histogram(binwidth = 0.5)

# Density 1 variable
sessions_profiles2 %>%
  ggplot(aes(x = Energy)) +
  geom_histogram(aes(y = stat(density)), binwidth = 1) +
  geom_density(color = "blue", size = 1) +
  facet_wrap(vars(Profile))

# Density 2 variables
sessions_profiles2 %>%
  ggplot(aes(x = ConnectionHours, y = ChargingHours)) +
  geom_point()

sessions_profiles2 %>%
  ggplot(aes(x = ConnectionHours, y = ChargingHours)) +
  stat_density2d(geom = "polygon", aes(fill = stat(nlevel)), bins = 100) +
  xlab("\nConnectionHours") + ylab("ChargingHours\n") +
  theme_light()


# Exercice: make a scatter plot showing the Energy charged according to
# the charging time


# What do you think about the result?


# Make a histogram of the Power variable
sessions_profiles2 %>%
  ggplot(aes(x = Power)) +
  geom_histogram(binwidth = 0.5)





# Interactive plots -------------------------------------------------------

# Plotly
# https://plotly.com/r/
library(plotly)


# Pie chart
USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


# Exercice: calculate the number of sessions from every user profile
# in object `sessions_profiles2` and visualize it as a pie chart




# Convert the `Power` histogram you made before to an interactive
# plot making use of the function `plotly::ggplotly`.






# Dygraphs
# https://rstudio.github.io/dygraphs

profiles_n_sessions %>%
  mutate_if(is.numeric, replace_na, 0) %>% # Fill gaps
  dygraph(main = "Test with {dygraphs}", ylab = "Number of sessions") %>%
  dyOptions(fillGraph = T, stepPlot = T, drawPoints = T, useDataTimezone = T)


# Exercice: use dygraphs to plot the evolution of energy consumed by
# week and user profile



# Exercice: Now plot it with a bar plot



