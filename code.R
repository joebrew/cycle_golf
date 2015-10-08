# Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)

# Link to google spreadsheet
link <- 'https://docs.google.com/spreadsheets/d/1JaJUChykHM7QqYDfgwXDDqU9_nsSIBiKle_ANVTqd_Q/pub?gid=1435508859&single=true&output=csv'

# Read data
df <- read_csv(link)

# Assign points
df$points <- NA
for (i in 1:nrow(df)){
  # Get course times for that day course
  times <- df$time[which(df$day == df$day[i] & 
                           df$course == df$course[i])]
  # Sort times
  times <- sort(times)
  # Get rank
  rank <- which(times == df$time[i])
  # Calculate score
  score <- ifelse(rank == 1, 3, 
                  ifelse(rank == 3, 1, 2))
  # Stick score into df
  df$points[i] <- score
}

# Get overall
overall <- df %>%
  group_by(rider) %>%
  summarise(points = sum(points),
            time = sum(time))

# Get by day
by_day <- df %>%
  group_by(rider, day) %>%
  summarise(points = sum(points),
            time = sum(time))

# Get by day summative
by_day_dt <- data.table(by_day)
by_day_dt[,cpoints:= cumsum(points), by = rider]
by_day <- data.frame(by_day_dt)


# Visualizations########################
par(mfrow = c(1,2))
# POINTS
bp <- barplot(overall$points,
              names.arg = overall$rider,
              main = 'Points')
text(x = bp[,1],
     y = overall$points,
     pos = 1,
     label = overall$points)

# TIME
bp <- barplot(overall$time,
              names.arg = overall$rider,
              main = 'Time')
text(x = bp[,1],
     y = overall$time,
     pos = 1,
     label = overall$time)
par(mfrow = c(1,1))

ggplot(data = by_day, aes(x = day, y = cpoints, group = rider, color = rider)) +
  geom_line() +
  xlab('Points') +
  theme_bw()

