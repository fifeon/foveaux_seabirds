##
## Descriptive results - data summaries
##
## --------------------------------------------------------##

## What birds did we see today and how many? ##

date_today <- Sys.Date()

birds_today <-
  df_birds %>%
  filter(date == date_today, !is.na(species)) %>%
  group_by(species) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_count))

total_birds_today <- sum(birds_today$total_count, na.rm = TRUE)

#Print total
cat("You recorded", total_birds_today, "birds today.\n\n") ;
#Print per-species breakdown
print(birds_today)
