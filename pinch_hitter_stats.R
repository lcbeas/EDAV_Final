
load("pbp2019.rda")
library(tidyverse)

pbp_ordered <- pbp2019 %>%
  mutate(game_date = as.Date(game_date, "%m/%d/%Y")) %>%
  arrange(game_date, home_team, inning, desc(inning_topbot), at_bat_number, pitch_number)


# batter not currently in the field
# restrict to NL games since no info on DH
nl_games <- pbp_ordered %>%
  filter(home_team %in% c("NYM","WSH","PHI","MIA","ATL",
                          "CHC","CIN","STL","PIT","MIL",
                          "ARI","LAD","SF","COL","SD"))

nl_games$ph <- NA
nl_games_ph <- as.list(nl_games)
for(row in 1:nrow(nl_games)){
  if(nl_games_ph$inning[row] == 1 & nl_games_ph$inning_topbot[row] == "Top"){
    nl_games_ph$ph[row] <- 0
  } else{
    if(nl_games_ph$inning_topbot[row] != nl_games_ph$inning_topbot[row-1]){
      fielders <- list(as.character(nl_games_ph$pitcher.1[row-1]), nl_games_ph$fielder_2.1[row-1],
                       nl_games_ph$fielder_3[row-1] ,nl_games_ph$fielder_4[row-1],
                       nl_games_ph$fielder_5[row-1], nl_games_ph$fielder_6[row-1],
                       nl_games_ph$fielder_7[row-1], nl_games_ph$fielder_8[row-1],
                       nl_games_ph$fielder_9[row-1])
    }
    nl_games_ph$ph[row] <- ifelse(as.character(nl_games_ph$batter[row]) %in% fielders,0,1)
  }
}
nl_games_ph <- as.data.frame(nl_games_ph)    

# check
table(nl_games_ph$ph)

nl_games_ph %>%
  filter(ph == 1) %>%
  group_by(batter) %>%
  summarize(pitches = n(),
            approx_pas = pitches / 4) %>%
  arrange(desc(approx_pas)) %>%
  head()


# swing rates
nl_games_ph_swings <- nl_games_ph %>%
  filter(!(description %in% c("bunt_foul_tip","foul_bunt","missed_bunt",
                              "pitchout","hit_by_pitch"))) %>%
  mutate(swing = ifelse(description %in% c("foul","foul_tip","hit_into_play",
                                           "hit_into_play_no_out","hit_into_play_score"),1,0))

# overall
nl_games_ph_swings %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# first pitch
nl_games_ph_swings %>%
  filter(pitch_number == 1) %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# risp
nl_games_ph_swings %>%
  filter(on_2b != "null" | on_3b != "null") %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# risp + first pitch
nl_games_ph_swings %>%
  filter(on_2b != "null" | on_3b != "null", pitch_number == 1) %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# late
nl_games_ph_swings %>%
  filter(inning >= 7) %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# risp late
nl_games_ph_swings %>%
  filter(inning >= 7, on_2b != "null" | on_3b != "null") %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)

# risp, late, first pitch
nl_games_ph_swings %>%
  filter(inning >= 7, on_2b != "null" | on_3b != "null", pitch_number == 1) %>%
  group_by(ph) %>%
  summarize(pitches = n(),
            swing_rate = sum(swing) / pitches)


# create data frame with results
df <- data.frame(situation = c("all","all","first","first",
                               "risp","risp","risp + first","risp + first",
                               "late","late","risp + late","risp + late",
                               "risp + late + first","risp + late + first"),
                 ph = factor(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1)),
                 swing_rate = c(.363,.340,.230,.201,.380,.351,.266,.212,
                                .362,.337,.377,.344,.263,.206))

# cleveland dot plot
theme_dotplot <- theme_bw(15) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())

ggplot(df, aes(x = swing_rate,
               y = fct_reorder2(situation,
                                ph == "1",
                                swing_rate,
                                .desc = FALSE),
               color = ph)) +
  geom_point(size = 3) +
  labs(title = "Swings Rates by Situation", x = "Swing Rate", y = "", color = "Pinch Hitter?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("darkorange", "blue")) +
  theme_dotplot



