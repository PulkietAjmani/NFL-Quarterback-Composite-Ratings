2024 NFL Quarterback Composite Ratings

This repo/script builds a composite rating for NFL QBs by combining a mixed-effects model of situational performance (from nflfastR play-by-play) with context and intangible adjustments you provide (cold weather, opponent strength, consensus ranking, leadership, etc.). It outputs a polished GT table and saves it as a PNG.

Author: Pulkiet Ajmani

Data: nflverse (play-by-play + rosters) + your manual inputs

Season: 2024 (regular season)

Highlights

Pulls 2024 regular-season QB dropbacks with nflfastR

Fits a mixed-effects model with QB, offense (team), and defense (opponent) random intercepts using lme4

Extracts per-QB situational coefficients from random effects

Joins roster headshots and team logos

Merges your manual scoring inputs to build a 0–100 final score

Produces a ranked GT table and saves 2024_qb_composite_full_ratings.png
