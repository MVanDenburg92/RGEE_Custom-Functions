library(data.table)
library(tidyr)
library(dplyr)
library(pryr)
library(ggplot2)
library(lubridate)

#Miles code:

mem_used()
gcinfo(FALSE)
gc()

getwd()

data_fold <-  getwd()

data_file <- paste0(data_fold,"/ee-chart.csv")

#Import csv file
# newData_master <- data.table::fread(file = data_file)

newData_df <- read.csv(file = data_file)


#transpose the df

df_pivotwide <- newData_df%>%
  pivot_wider(names_from = system.time_start, values_from = EVI)


summary(newData_df)

str(newData_df)

mdy(newData_df[[1]])


newData_df_convert <- newData_df %>% mutate(dates = mdy(.[[1]])) %>% select(dates, EVI)

p <- ggplot(newData_df_convert, aes(x=dates, y=EVI)) +
  geom_line() +
  xlab("")

p+ scale_x_date(date_labels = "%Y %b %d")
