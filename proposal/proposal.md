Project proposal
================
R-Megaddon: Yihan Shi, Holly Cui, Raffay Rana

``` r
library(tidyverse)
```

``` r
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv', show_col_types = FALSE)
```

## Dataset

The `youtube` dataset contains a list of ads from the 10 brands that had
the most advertisements in Super Bowls from 2000 to 2020, according to
data from superbowl-ads.com, with matching videos found on YouTube. It
was then analyzed by FiveThirtyEight staffers to come up with seven
defining characteristics of a Super Bowl ad represented as boolean value
columns. These include:

`funny`: Did the ad contain humor?

`patriotic`: Did the commercial make a patriotic appeal, either clear or
subtle, or include American imagery? Any glimpses of an American flag or
the words “America” or “United States” counted, as did references to the
armed forces, manufacturing and farming.

`celebrity`: Did the ad feature a celebrity?

`danger`: Did the ad contain any violence, threats of violence,
injuries, fighting or guns? Any allusions to death or hokey injuries
also counted here.

`animals`: Did it include animals? Did an animal — either real or
computer-generated — show up at any point in the ad? Even one-frame
appearances counted.

`use_Sex`: Did it use sex to sell its product? We counted any subtle or
overt suggestions of sex, sexuality, sex appeal or nudity.

Other variables in the dataset include the commercial’s brand, title,
thumbnail, description, year, poster channel’s name, Youtube ID, Youtube
etag, Youtube URL, and audience engagement metrics (likes, dislikes,
comments, favourites). In total, the `youtube` dataset had 247 rows and
25 columns.

## Motivation

We chose this dataset because Super Bowl commercials are often a unique
and interesting topic to explore. More specifically, we are interested
in digging into the several logical variables and how they might be
influencing the views and likes of the ads. Then, we are also interested
in looking at how this relation may have changed over the years and
whether there is some relation between certain years and the kinds of
ads made in those years. The variables in the dataset are also clean and
convenient to be pulled and used, which allows us to delve directly into
data visualization.

## Questions

1.  Are there any trends in how the ads change over the years in terms
    of content, audience preferences and engagement?

2.  How do election year ads differ from non-election year ads in terms
    of content and description?

## Analysis plan

To answer the first question, we will make a time series plot with
`year` on the x-axis and the boolean characteristics on the y-axis. This
can either be done by creating a binary time series plot and then
faceting by characteristics or by wrangling the data such that for each
year we have a column that has all the characteristics of the ad from
that year. This will give us an idea of how the characteristic makeup of
the ads has changed over time. It would then be interesting to look at
how the `like_count`, `dislike_count`, and `view_count` changed over the
years and whether there was any relation between ad characteristic and
audience engagement. We also intend on creating new variables such as
`like/view` and `dislike/view` to account for different viewership of
the ads over the years.

Answering the second question will involve multiple visualizations.
After filtering our data for just election years, we will create word
clouds for ad titles and ad captions to see if there are any particular
words that appear more frequently then others. We will complement this
by adding a bar graph to show the count of the most prominent
characteristics of these ads. Then, we will repeat the process for
non-election years and compare our results to note any differences. We
also intend on either creating new variables or summarizing existing
variables to account for the fact that there are more non-election years
than election years in the dataset.
