
# these were the libraries that I chose to load for my R script.
# I have the usuals and then an unusual package called artyfarty.

library(tidyverse)
library(gganimate)
library(artyfarty)
ss

# This was the first dataset that I loaded into R.
# It was helpful for my barrel ratio.

exit_velocity <- read_csv("Recitation-4/exit_velocity.csv", col_types = cols(
  last_name = col_character(),
  first_name = col_character(),
  player_id = col_double(),
  attempts = col_double(),
  avg_hit_angle = col_double(),
  anglesweetspotpercent = col_double(),
  max_hit_speed = col_double(),
  avg_hit_speed = col_double(),
  fbld = col_double(),
  gb = col_double(),
  max_distance = col_double(),
  avg_distance = col_double(),
  avg_hr_distance = col_double(),
  ev95plus = col_double(),
  `ev95per-swing` = col_double(),
  ev95percent = col_double(),
  barrels = col_double(),
  brl_percent = col_double(),
  brl_pa = col_double()))


# I believe that the readr library was in tidyverse, but it didn't hurt 
# just to make sure.
# I loaded this second dataset to help get some basic stats about the players.

library(readr)
stats <- read_csv("Recitation-4/stats.csv")


# I first joined the two datasets that I had to use info from both of them.
# I then created the variable that I will be focusing on for the rest of
# the project.
# I then sliced the new dataset to find the 10 worst barrel ratios.

barrel_ba <- 
  inner_join(stats, exit_velocity, by = c("last_name", "first_name")) %>%
  select(first_name, last_name, xba, brl_percent) %>%
  mutate(barrel_decimal = brl_percent * .01) %>%
  select(-brl_percent) %>%
  mutate(barrel_ratio = barrel_decimal / xba) %>%
  arrange(barrel_ratio) %>%
  slice(1:10)


# I had to create a column that had the player's full name to help
# distinguish between players.

barrel_ba$full_name <- paste(barrel_ba$first_name, barrel_ba$last_name)


# This is basically the same process that I had earllier.
# I had to create the variable barrel ratio.
# I then found the 10 best players for barrel ratio.

barrel_ba_good <- 
  inner_join(stats, exit_velocity, by = c("last_name", "first_name")) %>%
  select(first_name, last_name, xba, brl_percent) %>%
  mutate(barrel_decimal = brl_percent * .01) %>%
  select(-brl_percent) %>%
  mutate(barrel_ratio = barrel_decimal / xba) %>%
  arrange(desc(barrel_ratio)) %>%
  slice(1:10)


# I once again had to account for the players full name to filter.

barrel_ba_good$full_name <- paste(barrel_ba_good$first_name, barrel_ba_good$last_name)


# I then wanted to relate barrel ratio to the player's salaries.
# I wanted to find which players were overvalued because of their bad 
# barrel ratios.
# I ended up having to manually put in the salaries, because I could not find
# a dataset with all of the player's salaries.

salary_bad_table <- tibble(full_name = c("David Fletcher", "Kolten Wong", "Hanser Alberto",
                "Nicky Lopez", "J.P. Crawford", "Jonathan Villar", 
                "Raimel Tapia", "Victor Robles", "Isiah Kiner Falefa",
                "Jeff McNeil"),
       salary = c(567714, 1250000, 1650000,
                  597500, 2050000, 3550000, 
                  1950000, 557800, 2000000,
                  567714))


# This turned out to be a very similar process that we had earlier.
# These were the players with the 10 best barrel ratios.
# I again had to manually insert the salaries into a new table.

salary_good_table <- tibble(full_name = c("Evan White",
                                    "Joey Gallo",
                                    "Keston Hiura",
                                    "Brandon Lowe",
                                    "Fernando Tatis Jr.",
                                    "Adam Duvall",
                                    "Nick Castellanos",
                                    "Matt Olson",
                                    "Pete Alonso",
                                    "Luis Robert"),
                           salary = c(4000000, 6200000, 582100,
                                      1166666, 24285714, 5000000, 
                                      16000000, 5000000, 555000,
                                      8333333))


# I the joined the original tables with the players salaries by full name.

salary_bad_ratio <- 
  inner_join(salary_bad_table, barrel_ba, by = ("full_name")) 


# I did the same thing for the 10 best players.
salary_good_ratio <- 
  inner_join(salary_good_table, barrel_ba_good, by = ("full_name")) 


# This is the plot that I made to represent the players barrel ratios and 
# their slararies.
# I ended up making their salaries in millions to help with the x axis.
# I realized that I just needed geom_text and not geom_point.
# I decided to use the economist theme because it looked best with my
# shiny app theme.
# I was able to use the plot. arguments to help my axises and titles.

salary_good_plot <- salary_good_ratio %>%
  mutate(salary = salary / 1000000) %>%
  ggplot(mapping = aes(x = salary, y = barrel_ratio)) +
  geom_text(aes(label = full_name), 
            size = 3.5, 
            color = "black", 
            check_overlap = TRUE) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::number_format()) +
  theme_economist() +
  labs(x = "Player Salary", y = "Barrel Ratio", title = "Salary and Barrel Ratio") +
  theme(
    plot.title = element_text(color="blue", size=16, face="bold"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold"))

salary_good_plot


# I then did the same process, but for the 10 worst players.
# I decided to make their salaries in millions as well.
# It was weird, because their x axis turned out to be a lot different 
# than the previous salary chart.
# I wanted to add a transition piect to this graph, but it did not
# end up loading in time.

salary_bad_plot <- salary_bad_ratio %>% 
  mutate(salary = salary / 1000000) %>%
  ggplot(mapping = aes(x = salary, y = barrel_ratio)) +
  geom_text(aes(label = full_name), 
            size = 3.5, 
            color = "black", 
            check_overlap = TRUE) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::number_format()) +
  theme_economist() +
  labs(x = "Player Salary in Millions", y = "Barrel Ratio", title = "Salary and Barrel Ratio") +
  theme(
    plot.title = element_text(color="blue", size=16, face="bold"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold"))

salary_bad_plot
# transition_states(full_name, wrap = FALSE) +
# shadow_mark()


# I then created a bar plot to represent the 10 best players with barrel ratios.
# I had to use the dodge argument to make the x axis suitable to the viewer.
# I then used the stat argument within aes to make the bars go by individual
# barrel ratios.
# I then used the plot. and axis. arguments to better my code titles.

barrel_plot_good <- barrel_ba_good %>%
  ggplot() +
  geom_bar(aes(x = reorder(full_name, barrel_ratio), y = barrel_ratio),
           stat = 'identity') +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Player Name",
       y = "Barrel Ratio",
       title = "Top 10 Best Barrel Ratios") +
  theme_economist() +
  theme(
    plot.title = element_text(color="blue", size=16, face="bold"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold"))
barrel_plot_good


# I then did the exact same thing, but for the 10 worst players.
# I still ended up displaying their bars in ascending orders.
# Maybe I will end up changing that aspect later on.
# I chose to make the axis titles blue to flow with the theme better.
# I also planned on adding a transition on this graph.

barrel_plot <- barrel_ba %>%
  ggplot() +
  geom_bar(aes(x = reorder(full_name, barrel_ratio), y = barrel_ratio),
           stat = 'identity') +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Player Name",
       y = "Barrel Ratio",
       title = "Top 10 Worst Barrel Ratios") +
  theme_economist() +
  theme(
    plot.title = element_text(color="blue", size=16, face="bold"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold"))
barrel_plot
barrel_plot_good

# transition_states(full_name, wrap = FALSE) +
# shadow_mark()


# This was my original graph, that I don't value as much anymore.
# I took data from the exit_velocity dataset.
# I just don't think that this graph furthers my focus much.
# I also used the economist theme, because the blue goes well with the
# shinyapp theme.

plot_1 <- exit_velocity %>%
  ggplot(mapping = aes(x = avg_hit_speed, y = avg_distance)) +
  geom_point() +
  geom_smooth() +
  labs(x = "average exit velocity", 
       y = "average distance",
       title = "Correlation Between Exit Velo and Bomb Potential") +
  theme_economist() + 
  theme(
    plot.title = element_text(color="blue", size=16, face="bold"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )


# plot_2 <- exit_velocity %>%
#   select(last_name, avg_hit_speed) %>%
#   ggplot(mapping = aes(x = last_name)) +
#   geom_bar()
# 
# fit_obj <- stan_glm(data = exit_velocity,
#                     formula = avg_hit_speed ~ 1,
#                     family = gaussian,
#                     refresh = 0,
#                     seed = 9)
# 
# fit_plot <- fit_obj %>%
#   as_tibble() %>%
#   rename(mu = `(Intercept)`) %>%
#   ggplot(aes(x = mu)) +
#   geom_histogram(aes(y = after_stat(count/sum(count))),
#                  bins = 100) +
#   labs(title = "Posterior for Average MLB Exit Velo",
#        subtitle = "Average exit velo around 90 mph",
#        x = "MPH",
#        y = "Probability",
#        caption = "Data source: NHANES") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_classic()
# fit_plot
# 
# ggsave("baseball_velocity.png", fit_plot)


# I ended up reading in one more dataset.

stats_5 <- read_csv("Recitation-4/stats (5).csv")


# I had to once again create a column that went by the player's full name.

stats_5$full_name <- paste(stats_5$first_name, stats_5$last_name)


# For this one I manually coded all of the top 10 best players regarding 
# barrel ratio.
# I just filtered these names with the new dataset that I loaded.
# I ended up making a line graph to see how the top players progressed 
# through time.
# It was tough for me to figure out the scales at first.

yearly_barrel <- stats_5 %>%
  mutate(barrel_ratio = barrel_batted_rate * .01) %>%
  mutate(barrel_average_ratio = barrel_ratio / xba) %>%
  filter(full_name %in% c("Evan White",
                          "Joey Gallo",
                          "Keston Hiura",
                          "Brandon Lowe",
                          "Fernando Tatis Jr.",
                          "Adam Duvall",
                          "Nick Castellanos",
                          "Matt Olson",
                          "Pete Alonso",
                          "Luis Robert")) %>%
  ggplot(mapping = aes(year, barrel_average_ratio, color = full_name)) +
  geom_line(size = 2) +
  labs(x = "Year", y = "Average Barrel Ratio", color = "Player") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2015, 2021, 1), limits = c(2015, 2021)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_economist() +
  theme(
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold"))
  
yearly_barrel

  





