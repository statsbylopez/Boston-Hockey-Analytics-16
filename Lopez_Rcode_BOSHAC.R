

### Preamble: Install packages
#
#1. `dplyr`, `tidyr` for data manipulation
#2. `stringr` for pesky characters
#3. `lubridate` for dates
#4. `ggplot2` for visualization
#5. `lme4`, `mgcv`, `rjags` for advanced modeling
#6. `glmnet`, `randomForest`, `caret` for machine learning
#7. `RMarkdown` for reproducibility
#8. `readr`, `googlesheets` for uploading data 
#9. `rvest`, `XML` for scraping data off web
#10. `BradleyTerry2` for team rankings
#
###

### Here's an example:
install.packages("dplyr")

### Read in data and libraries

library(stringr); library(XML); library(BradleyTerry2)
library(dplyr); library(ggplot2); options(dplyr.width = Inf)
url <- c("http://www.hockey-reference.com/leagues/NHL_2016_games.html")
nhl.df <- readHTMLTable(url)
names(nhl.df)
reg.season <- nhl.df$games
reg.season %>%
  head(3)


load(url("http://war-on-ice.com/data/nhlscrapr-20142015.RData"))
grand.data %>% head(3)

load(url("http://war-on-ice.com/data/nhlscrapr-core.RData"))
roster.unique %>% head(2)

### Data manipulation

grand.data %>%
  filter(etype == "GOAL") %>%
  inner_join(roster.unique, by = c("ev.player.1"="player.id")) %>%
  select(gcode, ev.team, ev.player.1, firstlast, 
         distance, ev.team, hometeam, awayteam, seconds) %>%
  sample_n(3) 

brad <- grand.data %>%
  filter(a1 == 3190 | a2 == 3190 | a3 == 3190 |
           h1 == 3190 | h2 == 3190 | h3 == 3190, 
         away.skaters == 6, home.skaters == 6, 
         etype == "SHOT" | etype == "GOAL"| etype == "MISS" | etype == "BLOCK") %>%
  mutate(bos.event = as.numeric((ev.team == "BOS")), 
         corsi.count = 1*bos.event + -1*(1-bos.event),
         cum.corsi = cumsum(corsi.count), 
         index = 1:n())
brad %>% 
  select(season, gcode, period, ev.team, bos.event, corsi.count, cum.corsi, index) %>%
  head(6)

### Visualization

ggplot(data = brad, aes(x = index, y=cum.corsi)) + 
  geom_step() +
  xlab("Shot number (index)") +
  ylab("") + 
  ggtitle("Marchand shot differential, 14-15")


brad.game <- grand.data %>%
  filter(a1 == 3190 | a2 == 3190 | a3 == 3190 |
           h1 == 3190 | h2 == 3190 | h3 == 3190, 
         away.skaters == 6, home.skaters == 6, 
         etype == "SHOT" | etype == "GOAL") %>%
  mutate(bos.event = as.numeric((ev.team == "BOS"))) %>%
  group_by(gcode) %>%
  summarise(corsi.game.for = sum(bos.event), 
            corsi.game.against = sum(!bos.event), 
            corsi.game.diff = corsi.game.for - corsi.game.against)


p <- ggplot(brad.game, aes(corsi.game.for, corsi.game.against)) + 
  geom_jitter(colour = "black") + geom_density2d(colour = "black") + 
  scale_x_continuous(lim = c(0, 16)) + 
  scale_y_continuous(lim = c(0, 16)) +
  xlab("Shots for") +
  ylab("Shots against") +
  geom_abline(intercept = 0, slope = 1, linetype = 2)
p + ggtitle("Marchand game-level shot metrics, 14-15")  

### Example function

player.game <- function(id, team){
  game.data <- grand.data %>%
    filter(a1 == id | a2 == id | a3 == id |
             h1 == id | h2 == id | h3 == id, 
           away.skaters == 6, home.skaters == 6, 
           etype == "SHOT" | etype == "GOAL") %>%
    mutate(team.event = as.numeric((ev.team == team))) %>%
    group_by(gcode) %>%
    summarise(corsi.game.for = sum(team.event), 
              corsi.game.against = sum(!team.event), 
              corsi.game.diff = corsi.game.for - corsi.game.against)
  return(game.data)
}

nielsen.game <- player.game(3690, "NYI")

p +  geom_density2d(data = nielsen.game, aes(corsi.game.for, corsi.game.against), 
                    colour = "blue") + 
  geom_point(data = nielsen.game, aes(corsi.game.for, corsi.game.against), 
             colour = "blue") +
  annotate("text", x = 4, y = 13, label = "Nielsen", colour = "blue") + 
  annotate("text", x = 12, y = 13, label = "Marchand")

names(reg.season)[3] <- "vis.goals"
names(reg.season)[5] <- "home.goals"

reg.season1 <- reg.season %>%
  select(Date, Visitor, vis.goals, Home, home.goals) %>%
  mutate(goals.diff = as.numeric(as.character(home.goals)) - 
           as.numeric(as.character(vis.goals)), 
         home.win = as.numeric(goals.diff > 0))
reg.season1 %>% head(3)


### Statistical modeling using Bradley Terry

homeBT <- BTm(outcome = home.win,
              player1 = data.frame(team = Home, home.ice = 1),
              player2 = data.frame(team = Visitor, home.ice = 0),
              ~ team + home.ice,
              id = "team", data = reg.season1)

abilities <- data.frame(BTabilities(homeBT))
abilities <- abilities %>%
  mutate(ability = ability - mean(ability))
abilities$Team <- word(rownames(BTabilities(homeBT)),-1)
abilities %>% head(3)

abilities$Team <-factor(abilities$Team, 
                        levels= abilities[order(abilities$ability),"Team"]) 
p<- ggplot(abilities, aes(x = Team, y = ability)) + 
  geom_bar(stat = "identity")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Log-odds of beating an average team, 15-16")+ xlab("") + ylab("")

p

div <- c("Metro", "Atlantic", "Central", "Pacific")
div.teams <- c(div[4], div[4], div[2], div[2], div[4], 
               div[1], div[3], div[3], div[1], div[3],
               div[2], div[4], div[2], div[4], div[3], 
               div[2], div[3], div[1], div[1], div[1],
               div[2], div[1], div[1], div[4], div[3], 
               div[2], div[2], div[4], div[1], div[3])
abilities$division <- div.teams

p <- ggplot(abilities, aes(x = Team, y = ability)) + 
  geom_bar(stat = "identity")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Log-odds of beating an average team, 15-16")+ xlab("") + ylab("") + 
  facet_wrap(~division, scales = "free")

p
