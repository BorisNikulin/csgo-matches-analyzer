
# CSGO Matches Analysis

To analyze your matches:

  - Go to your csgo competitive match history and save the html
    webpage\*
  - Run the scraper on the html (complete although strangely slow)
  - Run the R analysis on the resulting sqlite file (wip)

\* This [reddit
post](https://www.reddit.com/r/GlobalOffensive/comments/8mbtjx/how_to_check_your_personal_game_data_for_banned/)
contains a plugin for helping load all the matches. (from 3KliksPhillip)
[video](https://youtu.be/AJyemS9hl50))

### Running the Scrapper

``` bash
$ git clone https://github.com/BorisNikulin/csgo-matches-analyzer.git
$ cd ./csgo-matches-analyzer/data-scraper
$ stack build
$ stack exec scraper-exe -- --help
```

Note: The R analysis assumes the database is called `csgo.db` and is
located inside `data-scraper`.

## Analysis (wip)

``` r
library(readr)
library(DBI)
library(lubridate)
library(dplyr)
library(glue)

vars <- read_csv('secrets/vars.csv',
    col_types = cols(col_character(), col_character())
    )
id1 <- vars$MY_ID
id2 <- vars$FRIEND_ID
p.name <- glue("CASE WHEN p.steam_id = {id1} THEN 'Me' WHEN {id2} THEN 'Friend' END as name")

# Sql with variable Interpolation utility func
# idk why glue_sql doesnt work
# however it quotes interpolated variables which i dont want
si <- . %>% glue() %>% sql()

con <- dbConnect(RSQLite::SQLite(), './data-scraper/csgo.db')
```

``` r
library(ggplot2)

tbl(con, si("
    SELECT {p.name}, pl.start_time
    FROM player p
    INNER JOIN played_in pl ON p.steam_id = pl.pid
    WHERE
        p.steam_id = {id2} OR
        p.steam_id = {id1}
    ")) %>%
    collect() %>%
    mutate(name = as.factor(name)) %>%
    mutate(start_time = ymd_hms(start_time)) %>%
    ggplot(aes(start_time)) +
    geom_histogram() +
    facet_grid(name~.)
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

``` r
tbl(con, si("
    SELECT AVG(pl.kills) as AvgDayKills, pl.start_time, {p.name}
    FROM player p
    INNER JOIN played_in pl ON p.steam_id = pl.pid
    WHERE
        p.steam_id = {id1} OR
        p.steam_id = {id2}
    GROUP BY p.steam_id, pl.start_time
    ")) %>%
    collect() %>%
    mutate(start_time = ymd_hms(start_time)) %>%
    ggplot(aes(start_time, AvgDayKills)) +
    geom_line() +
    geom_smooth() +
    facet_grid(name~.)
```

![](README_files/figure-gfm/a-1.png)<!-- -->

``` r
tbl(con, si("
    SELECT pl1.start_time, (pl1.kills - pl2.kills) AS 'Difference'
    FROM player p1, player p2
    INNER JOIN played_in pl1 ON p1.steam_id = pl1.pid
    INNER JOIN played_in pl2 ON p2.steam_id = pl2.pid
    WHERE
      p1.steam_id = {id1} AND
      p2.steam_id = {id2} AND
      pl1.start_time = pl2.start_time AND
      pl1.map = pl2.map AND
      pl1.duration = pl2.duration
    GROUP BY pl1.start_time
    ")) %>%
    collect() %>%
    mutate(start_time = ymd_hms(start_time)) %>%
    ggplot(aes(start_time, Difference)) +
    geom_line() +
    geom_smooth()
```

![](README_files/figure-gfm/difference-1.png)<!-- -->

``` r
tbl(con, si("
    SELECT {p.name}, pl.kills, m.map, AVG(win.DidWin) as WinRate, m.start_time
    FROM player p
    INNER JOIN played_in pl ON p.steam_id = pl.pid
    INNER JOIN match m ON
        pl.map = m.map AND
        pl.start_time = m.start_time AND
        pl.duration = m.duration
    INNER JOIN (
        SELECT p.steam_id, m.map, m.start_time, m.duration,
            CASE
                WHEN pl.team = 0 AND m.score_team_a = 16 THEN 1
                WHEN pl.team = 1 AND m.score_team_b = 16 THEN 1
                WHEN m.score_team_a = 16 OR m.score_team_b = 16 then 0
            END as DidWin
        FROM player p
        INNER JOIN played_in pl ON p.steam_id = pl.pid
        INNER JOIN match m ON
            pl.map = m.map AND
            pl.start_time = m.start_time AND
            pl.duration = m.duration
    ) win ON
        p.steam_id = win.steam_id AND
        m.map = win.map AND
        m.start_time = win.start_time AND
        m.duration = win.duration
    WHERE
        p.steam_id = {id1} OR
        p.steam_id = {id2} AND
        win.DidWin IS NOT NULL
    GROUP BY p.steam_id, pl.kills, m.map
    ")) %>%
    collect()  %>% #%T>%
    #print(n = Inf) %>%
    ggplot(aes(kills, WinRate)) +
    geom_point(aes(alpha = (map == "Cobblestone"))) +
    geom_smooth() +
    geom_line() +
    facet_grid(name ~ map) +
    guides(alpha = FALSE)
```

![](README_files/figure-gfm/win-ratio-1.png)<!-- -->

``` r
tbl(con, si("
    SELECT {p.name}, pl.kills, AVG(win.DidWin) as WinRate, m.start_time
    FROM player p
    INNER JOIN played_in pl ON p.steam_id = pl.pid
    INNER JOIN match m ON
        pl.map = m.map AND
        pl.start_time = m.start_time AND
        pl.duration = m.duration
    INNER JOIN (
        SELECT p.steam_id, m.map, m.start_time, m.duration,
            CASE
                WHEN pl.team = 0 AND m.score_team_a = 16 THEN 1
                WHEN pl.team = 1 AND m.score_team_b = 16 THEN 1
                WHEN m.score_team_a = 16 OR m.score_team_b = 16 then 0
            END as DidWin
        FROM player p
        INNER JOIN played_in pl ON p.steam_id = pl.pid
        INNER JOIN match m ON
            pl.map = m.map AND
            pl.start_time = m.start_time AND
            pl.duration = m.duration
    ) win ON
        p.steam_id = win.steam_id AND
        m.map = win.map AND
        m.start_time = win.start_time AND
        m.duration = win.duration
    WHERE
        p.steam_id = {id1} OR
        p.steam_id = {id2} AND
        win.DidWin IS NOT NULL
    GROUP BY p.steam_id, pl.kills
    ")) %>%
    collect()  %>% #%T>%
    #print(n = Inf) %>%
    ggplot(aes(kills, WinRate)) +
    geom_point() +
    geom_smooth() +
    geom_line() +
    facet_grid(name~.)
```

![](README_files/figure-gfm/win-ratio-2.png)<!-- -->

``` r
#TODO:
# de duplicate the code
# probably by taking the per map one and combining the averages
# when grouped just by name and kills
#
# figure out how nulls are getting in despite doing win.DidWin IS NOT NULL
```
