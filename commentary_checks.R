# Checks behind the numbers in the commentary on
# Tenenbaum, Ben-Zion and Lev (2025), technical fouls in the NBA.
# Run from the I4R_Forensic_Games_2026 folder, so that pkg/NBA DATA.csv is found.

library(readr)

dd <- as.data.frame(read_csv("pkg/NBA DATA.csv",
                             col_types = cols(GameId = col_character()),
                             show_col_types = FALSE))

home <- dd$TeamId == dd$HostTeamId
margin <- dd$TeamScore - dd$OpponentScore
won <- dd$WL == "W"

# penalised team minus opponent at -5, -3, -1, 0, +1, +3, +5 minutes
windows <- c(m5 = "5 Minutes before - ", m3 = "3 Minutes before - ",
             m1 = "1 Minute before - ", p1 = "1 Minute after - ",
             p3 = "3 Minutes after - ", p5 = "5 Minutes after - ")
diffs <- function(stat) {
  w <- sapply(windows, function(p) dd[[paste0(p, stat)]] - dd[[paste0(p, "Opponent ", stat)]])
  cbind(w[, 1:3], t0 = dd[[paste0("Team", stat)]] - dd[[paste0("Opponent", stat)]], w[, 4:6])
}

# empty windows: zero before the foul (game start), last value carried forward after it
fill_windows <- function(m) {
  for (j in 1:3) m[is.na(m[, j]), j] <- 0
  for (j in 5:7) m[is.na(m[, j]), j] <- m[is.na(m[, j]), j - 1]
  m
}

# 2 x 7 repeated-measures analysis as in Table 2
rm_anova <- function(m, h) {
  steps <- m[, 2:7] - m[, 1:6]
  g <- ifelse(h, 0.5, -0.5)
  mv <- summary(manova(steps ~ g), test = "Wilks", intercept = TRUE)$stats
  loc <- anova(lm(rowMeans(m) ~ h))
  c(loc_F = loc$`F value`[1],
    time_wilks = mv["(Intercept)", "Wilks"], time_F = mv["(Intercept)", "approx F"],
    int_F = mv["g", "approx F"], int_p = mv["g", "Pr(>F)"])
}

lr_stat <- function(t) {
  e <- outer(rowSums(t), colSums(t)) / sum(t)
  2 * sum(t * log(t / e))
}


## Section 2. Table 1 tests

score_tab <- matrix(c(646, 1257, 469, 1677), 2, byrow = TRUE)  # printed counts
final_tab <- matrix(c(873, 1101, 624, 1598), 2, byrow = TRUE)
for (t in list(score_tab, final_tab)) {
  x <- chisq.test(t, correct = FALSE)$statistic
  cat("N", sum(t), " chi2", round(x, 2), " LR", round(lr_stat(t), 2),
      " phi", round(sqrt(x / sum(t)), 3),
      " Yates", round(chisq.test(t)$statistic, 2), "\n")
}

# the same counts from the data
nt <- margin != 0
print(table(home[nt], margin[nt] < 0))
print(table(home, dd$WL))


## Section 2. Table 2 F statistics, complete cases vs filled windows

printed <- list(Score = c(97.90, 287.53, 6.63), AST = c(98.54, 44.96, 8.78),
                REB = c(46.77, 28.86, 6.99), PF = c(52.73, 239.93, 0.59))
t2 <- NULL
for (s in names(printed)) {
  m <- diffs(s)
  cc <- complete.cases(m)
  a <- rm_anova(m[cc, ], home[cc])
  b <- rm_anova(fill_windows(m), home)
  t2 <- rbind(t2, data.frame(stat = s, effect = c("loc", "time", "int"),
                             printed = printed[[s]],
                             complete = round(a[c("loc_F", "time_F", "int_F")], 2),
                             filled = round(b[c("loc_F", "time_F", "int_F")], 2)))
}
print(t2, row.names = FALSE)
cat("rows with an empty window", sum(!complete.cases(diffs("Score"))), "\n")
cat("mean / max deviation, complete cases",
    round(mean(abs(t2$complete / t2$printed - 1)) * 100, 1),
    round(max(abs(t2$complete / t2$printed - 1)) * 100, 1), "\n")
cat("mean / max deviation, filled windows",
    round(mean(abs(t2$filled / t2$printed - 1)) * 100, 1),
    round(max(abs(t2$filled / t2$printed - 1)) * 100, 1), "\n")


## Comment 3.1. Before and after the foul

pts <- diffs("Score")
cc <- complete.cases(pts)
for (h in c(TRUE, FALSE)) {
  cat(if (h) "home" else "away",
      " net scoring 5 min before", round(mean(pts[cc & home == h, "t0"] - pts[cc & home == h, "m5"]), 2),
      " 5 min after", round(mean(pts[cc & home == h, "p5"] - pts[cc & home == h, "t0"]), 2),
      " margin change at +1/+3/+5",
      round(sapply(c("p1", "p3", "p5"), function(w)
        mean(pts[home == h, w] - pts[home == h, "t0"], na.rm = TRUE)), 2), "\n")
}


## Comment 3.2. Home advantage and Hypothesis 1

first_row <- !duplicated(dd$GameId)
home_won <- ifelse(home, won, !won)
cat("games", sum(first_row), " home team won", round(100 * mean(home_won[first_row]), 1), "\n")
cat("penalised team won, ahead: home", round(100 * mean(won[home & margin > 0]), 1),
    " away", round(100 * mean(won[!home & margin > 0]), 1), "\n")
cat("penalised team won, trailing: home", round(100 * mean(won[home & margin < 0]), 1),
    " away", round(100 * mean(won[!home & margin < 0]), 1), "\n")
cat("cases home", sum(home), " away", sum(!home),
    " binomial p", signif(binom.test(sum(home), nrow(dd))$p.value, 2), "\n")


## Comment 3.3. Time effects in Table 2

for (s in c("Score", "PF", "REB")) {
  m <- diffs(s)
  cc <- complete.cases(m)
  cat(s, " time effect 1 - Wilks", round(1 - rm_anova(m[cc, ], home[cc])["time_wilks"], 3), "\n")
}


## Comment 3.4. Reporting errors

# effect sizes implied by the printed location F (1 and 4,194 df)
f <- c(points = 97.90, assists = 98.54, rebounds = 46.77,
       defense = 30.15, offense = 17.28, fouls = 52.73)
print(round(f / (f + 4194), 3))

# values the Results text describes
ast <- diffs("AST"); pf <- diffs("PF")
gap <- function(m, w, h) round(mean(m[home == h, w], na.rm = TRUE), 2)
cat("assists at foul and +5, home", gap(ast, "t0", TRUE), gap(ast, "p5", TRUE),
    " away", gap(ast, "t0", FALSE), gap(ast, "p5", FALSE), "\n")
cat("fouls, away at -5", gap(pf, "m5", FALSE), " home at foul", gap(pf, "t0", TRUE), "\n")
cat("point gap at +5, home", -gap(pts, "p5", TRUE), " away", -gap(pts, "p5", FALSE), "\n")
cat("point difference at foul, SD home", round(sd(pts[home, "t0"]), 2),
    " SE home", round(sd(pts[home, "t0"]) / sqrt(sum(home)), 2), "\n")

cat("tied at foul", sum(margin == 0), " home", sum(margin == 0 & home),
    " away", sum(margin == 0 & !home), "\n")


## Comment 3.5. Supplement shares (changes within each interval)

intervals <- list(c("m5", "m3"), c("m3", "m1"), c("m1", "t0"),
                  c("t0", "p1"), c("p1", "p3"), c("p3", "p5"))
for (s in c("PF", "Score")) {
  m <- diffs(s)
  for (h in c(TRUE, FALSE)) {
    cat(s, if (h) "home" else "away", "\n")
    for (k in intervals) {
      d <- m[home == h, k[2]] - m[home == h, k[1]]
      d <- d[!is.na(d)]
      cat("  ", k[1], "to", k[2], " up", round(100 * mean(d > 0)),
          " down", round(100 * mean(d < 0)), " none", round(100 * mean(d == 0)), "\n")
    }
  }
}
# for fouls, "down" means more fouls against the opponent


## Comment 3.6. Blocks and steals

blk <- diffs("BLK"); stl <- diffs("STL")
cc <- complete.cases(blk) & complete.cases(stl)
cat("n", sum(cc), "\n")
print(round(rm_anova(blk[cc, ], home[cc]), 3))
print(round(rm_anova(stl[cc, ], home[cc]), 3))


## Comment 3.7. Repeated cases

moment <- paste(dd$GameId, dd$TeamId, dd$SecondsFromStart)
groups <- split(seq_len(nrow(dd)), moment)
groups <- groups[lengths(groups) > 1]
cat("same-second groups", length(groups), " rows", sum(lengths(groups)), "\n")
cat("same coach", sum(sapply(groups, function(i) length(unique(dd$ExecutiveId[i])) == 1)),
    " groups with a repeated event number",
    sum(sapply(groups, function(i) anyDuplicated(dd$EventOrder[i]) > 0)), "\n")

table1 <- function(keep) {
  k <- keep & margin != 0
  c(N = sum(keep),
    trail_home = 100 * mean(margin[k & home] < 0), trail_away = 100 * mean(margin[k & !home] < 0),
    won_home = 100 * mean(won[keep & home]), won_away = 100 * mean(won[keep & !home]),
    chi_score = unname(chisq.test(table(home[k], margin[k] > 0), correct = FALSE)$statistic),
    chi_final = unname(chisq.test(table(home[keep], dd$WL[keep]), correct = FALSE)$statistic))
}
print(round(rbind(all = table1(rep(TRUE, nrow(dd))),
                  one_per_moment = table1(!duplicated(moment)),
                  first_per_game_team = table1(!duplicated(paste(dd$GameId, dd$TeamId)))), 2))


## Comment 3.8. Coverage and windows

date <- as.Date(dd$GameDate)
print(aggregate(date ~ Season, data = data.frame(Season = dd$Season, date = date),
                FUN = function(x) paste(range(x), collapse = " to ")))
playoffs <- dd$SeasonType == "Playoffs"
print(table(dd$Season[playoffs]))

late <- dd$Period == 4
for (w in list(c("1 Minute after - ", 60), c("3 Minutes after - ", 180), c("5 Minutes after - ", 300))) {
  past <- late & dd$RemainingSeconds < as.numeric(w[2])
  has_value <- !is.na(dd[[paste0(w[1], "Score")]])
  cat(w[1], " beyond regulation with values", sum(past & has_value),
      " empty", sum(past & !has_value), "\n")
}
# lopsided rows with under a minute left that still have a 1-minute window
i <- which(late & dd$RemainingSeconds < 60 & abs(margin) >= 11 &
             !is.na(dd[["1 Minute after - Score"]]))
print(data.frame(seconds_left = dd$RemainingSeconds[i], margin = abs(margin[i]),
                 points_after = dd[["1 Minute after - Score"]][i] + dd[["1 Minute after - Opponent Score"]][i] -
                   dd$TeamScore[i] - dd$OpponentScore[i]))

# personal identifiers (counts only)
cat("distinct names", length(unique(trimws(dd$ExecutiveName))),
    " distinct ids", length(unique(dd$ExecutiveId)), "\n")
