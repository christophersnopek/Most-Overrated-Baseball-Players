
library(fec16)
library(tidyverse)
library(fivethirtyeight)



# I took the house data set and took what mattered to me.
# I then grouped what would make sense for the summarization
# I had to mean the total votes because that was what the end goal was.
# desc is short for descending which is from top to bottom
q1 <- results_house %>%
  select(state, district_id, general_votes) %>%
  drop_na() %>%
  group_by(state, district_id) %>%
  summarize(total_votes = sum(general_votes)) %>%
  summarize(mean_voters = mean(total_votes)) %>%
  arrange(desc(mean_voters))




# I had to assign this first code chunk to use it later.
# I grouped by state because that is what we were judging
# I then needed the total votes for president.
presidential_total <- results_president %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(presidential_votes = sum(general_votes))

# I dropped na to get rid of negligible columns.
# I used group_by and summarize together to sum the general votes by state.
# Inner join was very confusing to me at first but the book simplifies it.
# I then made one more column to satisfy the problem.
q2 <- results_senate %>%
  drop_na(general_votes) %>%
  group_by(state) %>%
  summarize(senate_votes = sum(general_votes)) %>%
  inner_join(presidential_total, q2, by = "state") %>%
  mutate(vote_difference = (presidential_votes - senate_votes))






# I only wanted the democrat and republican data.
# I dropped the columns that would be negligible if they had na.
# I did the classic ggplot
# at first I couldn't get all the colors to work at the bottom, but then i fixed it by adding color = party in the aes function.
# I had to use the scales to make the axises look better which did.
q3 <- results_house %>%
  filter(party == "DEM" | party == "REP") %>%
  drop_na(general_votes, primary_votes) %>%
  ggplot(mapping = aes(x = primary_votes, y = general_votes, color = party)) +
  geom_point() +
  labs(title = "Votes for House Candidates in 2016", subtitle = "Almost all candidates receive more votes in the general election", x = "Number of    Votes in Primary Election", y = "Number of Votes in General Election", caption = "Source: Federal Election Commission") +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("dodgerblue", "lightcoral"), name = "Party", breaks = c("DEM", "REP"), labels = c("Democrat", "Republican")) +
  theme_light()
 
ggsave("plot.png", q3)