getwd()


# import modules
library(dplyr)
library(tidyverse)
library(ggplot2)

# read the csv file
data <- read.csv("Data/sales_data_sample.csv")

# summarize the data
colnames(data)

# get the total sales by country
b <- data %>%
  group_by(COUNTRY) %>% # groupby country
  summarize(total_sales = sum(SALES)) %>% # use the summarize to sum
  arrange(desc(total_sales)) # arrange by greatest to least
 
# plot a bar chart of the top ten sales by country
barplot(b$total_sales[1:10],
        main = "Sales by Country (Top Ten)",
        xlab = 'Sales',
        ylab = 'Country',
        las = 1,
        names.arg = b$COUNTRY[1:10],
        col = "darkblue",
        horiz = TRUE)

# create table for sales by customer for a country
usa_c <- data %>%
  group_by(CUSTOMERNAME) %>%
  filter(COUNTRY == 'USA') %>%
  summarize(total_sales = sum(SALES)) %>%
  arrange(desc(total_sales))

# plot a bar chart for the table above
barplot(usa_c$total_sales,
        main = "Sales by Customer",
        las = 2,
        names.arg = usa_c$CUSTOMERNAME,
        col = "green",
        horiz = FALSE)
    
# plot the bar chart using ggplot2 library
usa_bar <- ggplot(
  data=usa_c, aes(x=reorder(CUSTOMERNAME, -total_sales), y=total_sales)) +
    geom_bar(stat="identity", fill='green', width=0.9)

# plot using the theme to adjust the axis as well
# the hjust/vjust are critical to align the label with the grid
usa_bar + theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2))

# group by the year and products to get sales by product each year
usa_year <- data %>%
  filter(COUNTRY == 'USA')

# group by productline and year
usa_year_p <- usa_year %>%
  group_by(PRODUCTLINE, YEAR_ID) %>%
  summarize(total = sum(SALES))

# plot using a stacked bar chart
usa_p_bar <- ggplot(
  data=usa_year_p, aes(x=YEAR_ID, y=total, fill=PRODUCTLINE)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c(
    "Classic Cars" = "#353436",
    "Motorcycles" = "#1b98e0",
    "Planes" = "red",
    "Ships" = "yellow",
    "Trains" = "green",
    "Trucks and Buses" = "darkblue",
    "Vintage Cars" = "orange"
  ))

usa_p_bar




