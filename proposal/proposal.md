Project proposal
================
megaddon

``` r
library(tidyverse)
```

``` r
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
```

    ## Rows: 247 Columns: 25

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): brand, superbowl_ads_dot_com_url, youtube_url, id, kind, etag, ti...
    ## dbl   (7): year, view_count, like_count, dislike_count, favorite_count, comm...
    ## lgl   (7): funny, show_product_quickly, patriotic, celebrity, danger, animal...
    ## dttm  (1): published_at

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Dataset

The dataset contains a list of ads from the 10 brands that had the most
advertisements in Super Bowls from 2000 to 2020, according to data from
superbowl-ads.com, with matching videos found on YouTube. It was then
analyzed by FiveThirtyEight staffers to come up with seven defining
characteristics of a Super Bowl ad represented as boolean value columns.
These include:

Funny: Did the ad contain humor?

Patriotic: Did the commercial make a patriotic appeal, either clear or
subtle, or include American imagery? Any glimpses of an American flag or
the words “America” or “United States” counted, as did references to the
armed forces, manufacturing and farming.

Celebrity: Did the ad feature a celebrity?

Danger: Did the ad contain any violence, threats of violence, injuries,
fighting or guns? Any allusions to death or hokey injuries also counted
here.

Animals: Did it include animals? Did an animal — either real or
computer-generated — show up at any point in the ad? Even one-frame
appearances counted.

use\_Sex: Did it use sex to sell its product? We counted any subtle or
overt suggestions of sex, sexuality, sex appeal or nudity.

Other variables in the dataset include the commercial’s brand, title,
thumbnail, description, year, poster channel’s name, Youtube ID, Youtube
etag, Youtube URL, and audience engagement metrics (likes, dislikes,
comments, favourites).

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

2.  What are the prominent features of ad titles and descriptions,
    especially during the election years, and do they correspond to
    their categorization?

## Analysis plan

To answer the first question, we need variables that describe the
characteristics that appear in the commercial, including funny,
patriotic, animals, danger, title length, show\_product\_quickly,
use\_sex, view\_count, like\_count, dislike\_count. We will also create
new variables like\_count/view\_count and dislike\_count/view\_count to
show the percentage of like/dislike over all the views.

To answer the second question, we need to extract and count certain
words from the title and description variables. We also need the
variables that describe the characteristics that appear in the
commercial and create a new variables election\_year.
