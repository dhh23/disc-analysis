- [Post count distribution through
  time](#post-count-distribution-through-time)
- [Post count distribution
  (overall/lounge)](#post-count-distribution-overalllounge)
- [Interaction between join date and total / lounge
  posts](#interaction-between-join-date-and-total-lounge-posts)
- [Board rhythm](#board-rhythm)
  - [Are there distinct
    subpopulations?](#are-there-distinct-subpopulations)
- [How long are people active by year
  joined](#how-long-are-people-active-by-year-joined)
- [Different -cels](#different--cels)

``` r
library(ggbeeswarm)
```

    ## Loading required package: ggplot2

``` r
library(gt)
source(here::here("src/common_basis.R"))
```

    ## here() starts at /Users/jiemakel/tyo/disc-analysis

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Post count distribution through time

``` r
incel_posts_c %>%
  count(year=year(time_posted),month=month(time_posted)) %>%
  mutate(month=as.Date(str_c(year,'-',month,'-01'))) %>%
  ggplot(aes(x=month,y=n)) +
  geom_line() +
  scale_y_continuous(labels=scales::number) + 
  xlab("Month") +
  ylab("Posts") +
  theme_hsci_discrete()
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Post count distribution (overall/lounge)

``` r
quantiles <- seq(0,1,by=0.05)
incel_users_c %>%
  select(poster_total_posts) %>%
  collect() %>%
  reframe(
    quantile=quantiles,
    poster_total_posts=quantile(poster_total_posts,quantiles)
  ) %>%
  inner_join(
    incel_posts_c %>%
      count(poster_id) %>%
      select(n) %>%
      collect() %>%
      reframe(
        quantile=quantiles,
        poster_lounge_posts=quantile(n,quantiles)
    ),
    join_by(quantile)
  ) %>%
  gt(rowname_col = "quantile") %>%
  fmt_percent(quantile, drop_trailing_zeros = TRUE) %>%
  fmt_number(columns = c(poster_total_posts,poster_lounge_posts), drop_trailing_zeros = TRUE)
```

<div id="xakypaubkm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xakypaubkm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xakypaubkm thead, #xakypaubkm tbody, #xakypaubkm tfoot, #xakypaubkm tr, #xakypaubkm td, #xakypaubkm th {
  border-style: none;
}
&#10;#xakypaubkm p {
  margin: 0;
  padding: 0;
}
&#10;#xakypaubkm .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xakypaubkm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xakypaubkm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xakypaubkm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xakypaubkm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xakypaubkm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xakypaubkm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xakypaubkm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xakypaubkm .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xakypaubkm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xakypaubkm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xakypaubkm .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xakypaubkm .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xakypaubkm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xakypaubkm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xakypaubkm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xakypaubkm .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xakypaubkm .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xakypaubkm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xakypaubkm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xakypaubkm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xakypaubkm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xakypaubkm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xakypaubkm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xakypaubkm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xakypaubkm .gt_left {
  text-align: left;
}
&#10;#xakypaubkm .gt_center {
  text-align: center;
}
&#10;#xakypaubkm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xakypaubkm .gt_font_normal {
  font-weight: normal;
}
&#10;#xakypaubkm .gt_font_bold {
  font-weight: bold;
}
&#10;#xakypaubkm .gt_font_italic {
  font-style: italic;
}
&#10;#xakypaubkm .gt_super {
  font-size: 65%;
}
&#10;#xakypaubkm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xakypaubkm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xakypaubkm .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xakypaubkm .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xakypaubkm .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xakypaubkm .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xakypaubkm .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="poster_total_posts">poster_total_posts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="poster_lounge_posts">poster_lounge_posts</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_right gt_stub">0%</th>
<td headers="stub_1_1 poster_total_posts" class="gt_row gt_right">0</td>
<td headers="stub_1_1 poster_lounge_posts" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_right gt_stub">5%</th>
<td headers="stub_1_2 poster_total_posts" class="gt_row gt_right">2</td>
<td headers="stub_1_2 poster_lounge_posts" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_right gt_stub">10%</th>
<td headers="stub_1_3 poster_total_posts" class="gt_row gt_right">5</td>
<td headers="stub_1_3 poster_lounge_posts" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_right gt_stub">15%</th>
<td headers="stub_1_4 poster_total_posts" class="gt_row gt_right">8</td>
<td headers="stub_1_4 poster_lounge_posts" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_right gt_stub">20%</th>
<td headers="stub_1_5 poster_total_posts" class="gt_row gt_right">12</td>
<td headers="stub_1_5 poster_lounge_posts" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_right gt_stub">25%</th>
<td headers="stub_1_6 poster_total_posts" class="gt_row gt_right">18</td>
<td headers="stub_1_6 poster_lounge_posts" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_right gt_stub">30%</th>
<td headers="stub_1_7 poster_total_posts" class="gt_row gt_right">26</td>
<td headers="stub_1_7 poster_lounge_posts" class="gt_row gt_right">4</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_right gt_stub">35%</th>
<td headers="stub_1_8 poster_total_posts" class="gt_row gt_right">36</td>
<td headers="stub_1_8 poster_lounge_posts" class="gt_row gt_right">5</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_right gt_stub">40%</th>
<td headers="stub_1_9 poster_total_posts" class="gt_row gt_right">50</td>
<td headers="stub_1_9 poster_lounge_posts" class="gt_row gt_right">6</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_right gt_stub">45%</th>
<td headers="stub_1_10 poster_total_posts" class="gt_row gt_right">69</td>
<td headers="stub_1_10 poster_lounge_posts" class="gt_row gt_right">8</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_right gt_stub">50%</th>
<td headers="stub_1_11 poster_total_posts" class="gt_row gt_right">95.5</td>
<td headers="stub_1_11 poster_lounge_posts" class="gt_row gt_right">12</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_right gt_stub">55%</th>
<td headers="stub_1_12 poster_total_posts" class="gt_row gt_right">126</td>
<td headers="stub_1_12 poster_lounge_posts" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_right gt_stub">60%</th>
<td headers="stub_1_13 poster_total_posts" class="gt_row gt_right">177</td>
<td headers="stub_1_13 poster_lounge_posts" class="gt_row gt_right">22</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_right gt_stub">65%</th>
<td headers="stub_1_14 poster_total_posts" class="gt_row gt_right">247</td>
<td headers="stub_1_14 poster_lounge_posts" class="gt_row gt_right">31</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_right gt_stub">70%</th>
<td headers="stub_1_15 poster_total_posts" class="gt_row gt_right">353</td>
<td headers="stub_1_15 poster_lounge_posts" class="gt_row gt_right">47</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_right gt_stub">75%</th>
<td headers="stub_1_16 poster_total_posts" class="gt_row gt_right">536</td>
<td headers="stub_1_16 poster_lounge_posts" class="gt_row gt_right">74</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_right gt_stub">80%</th>
<td headers="stub_1_17 poster_total_posts" class="gt_row gt_right">802.4</td>
<td headers="stub_1_17 poster_lounge_posts" class="gt_row gt_right">122</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_right gt_stub">85%</th>
<td headers="stub_1_18 poster_total_posts" class="gt_row gt_right">1,337.65</td>
<td headers="stub_1_18 poster_lounge_posts" class="gt_row gt_right">228</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_right gt_stub">90%</th>
<td headers="stub_1_19 poster_total_posts" class="gt_row gt_right">2,373.1</td>
<td headers="stub_1_19 poster_lounge_posts" class="gt_row gt_right">460</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_right gt_stub">95%</th>
<td headers="stub_1_20 poster_total_posts" class="gt_row gt_right">5,339</td>
<td headers="stub_1_20 poster_lounge_posts" class="gt_row gt_right">1,223.65</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_right gt_stub">100%</th>
<td headers="stub_1_21 poster_total_posts" class="gt_row gt_right">319,186</td>
<td headers="stub_1_21 poster_lounge_posts" class="gt_row gt_right">30,485</td></tr>
  </tbody>
  &#10;  
</table>
</div>

- As expected, the distribution is very skewed. Half the users have less
  than 100 posts, while the top 25% have more than 500.

# Interaction between join date and total / lounge posts

``` r
incel_users_c %>%
  filter(poster_joined>"1970-01-01") %>%
  ggplot(aes(x=poster_joined,y=poster_total_posts)) +
  geom_point(size=0.5) +
  geom_smooth(method="lm", formula="y~x") +
  theme_hsci_discrete() +
  scale_y_continuous(labels=scales::number) +
  xlab("user join date") +
  ylab("Total posts") +
  ggtitle("Total posts")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
incel_posts_c %>%
  count(poster_id) %>%
  inner_join(incel_users_c, join_by(poster_id)) %>%
  filter(poster_joined>"1970-01-01") %>%
  ggplot(aes(x=poster_joined,y=n)) +
  geom_point(size=0.5) +
  geom_smooth(method="lm", formula="y~x") +
  theme_hsci_discrete() +
  scale_y_continuous(labels=scales::number) +
  xlab("user join date") +
  ylab("Lounge posts") +
  ggtitle("Lounge posts")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

- The earlier you join, the more likely you are to have more posts, but
  there doesn’t seem to be a discernible pattern for when the real
  “heavy hitters” have joined.

# Board rhythm

``` r
incel_posts_c %>% 
  mutate(hour=hour(time_posted),weekday=weekday(time_posted)) %>%
  count(weekday,hour) %>%
  ggplot(aes(x=hour,y=n,color=as_factor(weekday))) +
  geom_line() +
  theme_hsci_discrete() +
  theme(
    legend.justification = c(1, 0), 
    legend.position = c(0.98, 0.02), 
    legend.background = element_blank(), 
    legend.key = element_blank()) +
  xlab("Hour (UTC)") +
  ylab("Total number of posts") +
  labs(color="Day of the week") +
  ggtitle("Posts by the time of day")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
weekdays <- tribble(~index,~weekday,
                    0,"Mon",
                    1,"Tue",
                    2,"Wed",
                    3,"Thu",
                    4,"Fri",
                    5,"Sat",
                    6,"Sun")
incel_posts_c %>% 
  mutate(weekday=weekday(time_posted)) %>%
  count(weekday) %>%
  ggplot(aes(x=weekday,y=n)) +
  geom_col() +
  theme_hsci_discrete() +
  scale_x_continuous(breaks=weekdays$index, labels=weekdays$weekday) +
  scale_y_continuous(labels=scales::number) +
  xlab("Day of the week") +
  ylab("Total number of posts") +
  ggtitle("Posts by day of the week")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
poster_type <- incel_posts_c %>%
  count(poster_id) %>%
  mutate(poster_type=if_else(n>=100,"top","other")) %>%
  select(poster_id, poster_type)

incel_posts_c %>% 
  mutate(hour=hour(time_posted)) %>%
  inner_join(poster_type, join_by(poster_id)) %>%
  count(poster_type,hour) %>%
  group_by(poster_type) %>%
  mutate(proportion=n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=hour,y=proportion,color=poster_type)) +
  geom_line() +
  theme_hsci_discrete() +
  theme(
    legend.justification = c(1, 0), 
    legend.position = c(0.98, 0.02), 
    legend.background = element_blank(), 
    legend.box.just = "bottom", 
    legend.key = element_blank(), 
    legend.box = "horizontal") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  xlab("Hour (UTC)") +
  ylab("Proportion of posts") +
  labs(color="user type") +
  ggtitle("Proportion of posts by the time of day of top/other users")
```

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

![](incel-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

- There doesn’t seem to be a difference in daily rhythms between top
  users and others.
- Interestingly, no big differences by day of week
- How international is the forum?

## Are there distinct subpopulations?

``` r
incel_posts_c %>% 
  mutate(hour=hour(time_posted)) %>%
  count(poster_id,hour) %>%
  group_by(poster_id) %>%
  filter(sum(n)>=100) %>% # limit to users with enough data to get any pattern
  mutate(proportion=n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=hour,y=proportion)) +
  geom_quasirandom(size=0.25) +
  coord_cartesian(ylim=c(0,0.25)) +
  theme_hsci_discrete() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  xlab("Hour (UTC)") +
  ylab("Proportion of posts") +
  labs(color="user type") +
  ggtitle("Proportion of posts by the time of day for each individual user")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

- There do not seem to be clearly distinct time profiles with large
  groups of users. There may be some variation in UTC night time posting
  behaviour (3-12 UTC)

# How long are people active by year joined

``` r
incel_posts_c %>%
    group_by(poster_id) %>%
    summarise(earliest_post=min(time_posted),latest_post=max(time_posted), .groups="drop") %>%
    mutate(earliest_post_year=year(earliest_post), active_period_days=sql("timestampdiff(day,earliest_post,latest_post)")) %>%
  ggplot(aes(x=earliest_post_year,y=active_period_days)) + 
  geom_quasirandom(size=0.25) +
  theme_hsci_discrete() +
  xlab("Year of earliest post") +
  ylab("Time between earliest and latest post (days)") +
  ggtitle("Time between earliest and latest post by year joined")
```

![](incel-analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

- In 2019, there seem to have been more people joining who stayed on
  longer.

# Different -cels

``` r
cel_post_contents <- incel_posts_c %>% 
  filter(str_detect(post_content,"cels?\\b")) %>% 
  select(post_id, post_content) %>% 
  collect() 

cels_by_post <- cel_post_contents %>%
  mutate(cel=str_extract_all(post_content, "(?<! expand...)[^ \\n]+ cels?\\b|[^ \\n]*cels?\\b(?! said)")) %>%
  unnest(cel) %>% 
  filter(!str_detect(cel, "^@")) %>%
  select(post_id, cel) %>% 
  mutate(cel=cel %>% 
           str_to_lower() %>% 
           str_replace_all("\\W","") %>% 
           str_replace_all("s$",""))
```

``` r
cels <- cels_by_post %>%
  count(cel) %>% 
  arrange(desc(n))
```

``` r
cels %>% 
  write_tsv(here("data/output/jiemakel/cels.tsv"),na="",quote="needed")
```

``` r
cels %>%
  head(n=100) %>%
  gt(rowname_col="cel") %>%
  fmt_integer(n)
```

<div id="tsymkhaech" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tsymkhaech table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#tsymkhaech thead, #tsymkhaech tbody, #tsymkhaech tfoot, #tsymkhaech tr, #tsymkhaech td, #tsymkhaech th {
  border-style: none;
}
&#10;#tsymkhaech p {
  margin: 0;
  padding: 0;
}
&#10;#tsymkhaech .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#tsymkhaech .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#tsymkhaech .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#tsymkhaech .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#tsymkhaech .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#tsymkhaech .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#tsymkhaech .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#tsymkhaech .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#tsymkhaech .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#tsymkhaech .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#tsymkhaech .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#tsymkhaech .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#tsymkhaech .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#tsymkhaech .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#tsymkhaech .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tsymkhaech .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#tsymkhaech .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#tsymkhaech .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#tsymkhaech .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tsymkhaech .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#tsymkhaech .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tsymkhaech .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#tsymkhaech .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tsymkhaech .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tsymkhaech .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tsymkhaech .gt_left {
  text-align: left;
}
&#10;#tsymkhaech .gt_center {
  text-align: center;
}
&#10;#tsymkhaech .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#tsymkhaech .gt_font_normal {
  font-weight: normal;
}
&#10;#tsymkhaech .gt_font_bold {
  font-weight: bold;
}
&#10;#tsymkhaech .gt_font_italic {
  font-style: italic;
}
&#10;#tsymkhaech .gt_super {
  font-size: 65%;
}
&#10;#tsymkhaech .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#tsymkhaech .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#tsymkhaech .gt_indent_1 {
  text-indent: 5px;
}
&#10;#tsymkhaech .gt_indent_2 {
  text-indent: 10px;
}
&#10;#tsymkhaech .gt_indent_3 {
  text-indent: 15px;
}
&#10;#tsymkhaech .gt_indent_4 {
  text-indent: 20px;
}
&#10;#tsymkhaech .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">incel</th>
<td headers="stub_1_1 n" class="gt_row gt_right">116,353</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">fakecel</th>
<td headers="stub_1_2 n" class="gt_row gt_right">14,234</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">truecel</th>
<td headers="stub_1_3 n" class="gt_row gt_right">10,269</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">volcel</th>
<td headers="stub_1_4 n" class="gt_row gt_right">7,820</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">greycel</th>
<td headers="stub_1_5 n" class="gt_row gt_right">7,145</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">brocel</th>
<td headers="stub_1_6 n" class="gt_row gt_right">6,364</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">httpsincel</th>
<td headers="stub_1_7 n" class="gt_row gt_right">5,779</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">graycel</th>
<td headers="stub_1_8 n" class="gt_row gt_right">5,446</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub">trucel</th>
<td headers="stub_1_9 n" class="gt_row gt_right">4,641</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub">ricecel</th>
<td headers="stub_1_10 n" class="gt_row gt_right">4,330</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub">currycel</th>
<td headers="stub_1_11 n" class="gt_row gt_right">3,160</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub">mentalcel</th>
<td headers="stub_1_12 n" class="gt_row gt_right">2,309</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub">oldcel</th>
<td headers="stub_1_13 n" class="gt_row gt_right">2,243</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_left gt_stub">gymcel</th>
<td headers="stub_1_14 n" class="gt_row gt_right">2,185</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_left gt_stub">youngcel</th>
<td headers="stub_1_15 n" class="gt_row gt_right">2,030</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_left gt_stub">blackcel</th>
<td headers="stub_1_16 n" class="gt_row gt_right">1,977</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_left gt_stub">lostcel</th>
<td headers="stub_1_17 n" class="gt_row gt_right">1,977</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_left gt_stub">braincel</th>
<td headers="stub_1_18 n" class="gt_row gt_right">1,628</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_left gt_stub">femcel</th>
<td headers="stub_1_19 n" class="gt_row gt_right">1,626</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_left gt_stub">rincel</th>
<td headers="stub_1_20 n" class="gt_row gt_right">1,588</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_left gt_stub">ritalincel</th>
<td headers="stub_1_21 n" class="gt_row gt_right">1,397</td></tr>
    <tr><th id="stub_1_22" scope="row" class="gt_row gt_left gt_stub">escortcel</th>
<td headers="stub_1_22 n" class="gt_row gt_right">1,329</td></tr>
    <tr><th id="stub_1_23" scope="row" class="gt_row gt_left gt_stub">whitecel</th>
<td headers="stub_1_23 n" class="gt_row gt_right">1,267</td></tr>
    <tr><th id="stub_1_24" scope="row" class="gt_row gt_left gt_stub">fatcel</th>
<td headers="stub_1_24 n" class="gt_row gt_right">1,261</td></tr>
    <tr><th id="stub_1_25" scope="row" class="gt_row gt_left gt_stub">excel</th>
<td headers="stub_1_25 n" class="gt_row gt_right">994</td></tr>
    <tr><th id="stub_1_26" scope="row" class="gt_row gt_left gt_stub">maycel</th>
<td headers="stub_1_26 n" class="gt_row gt_right">983</td></tr>
    <tr><th id="stub_1_27" scope="row" class="gt_row gt_left gt_stub">poorcel</th>
<td headers="stub_1_27 n" class="gt_row gt_right">921</td></tr>
    <tr><th id="stub_1_28" scope="row" class="gt_row gt_left gt_stub">blackops2cel</th>
<td headers="stub_1_28 n" class="gt_row gt_right">830</td></tr>
    <tr><th id="stub_1_29" scope="row" class="gt_row gt_left gt_stub">newcel</th>
<td headers="stub_1_29 n" class="gt_row gt_right">823</td></tr>
    <tr><th id="stub_1_30" scope="row" class="gt_row gt_left gt_stub">fbicel</th>
<td headers="stub_1_30 n" class="gt_row gt_right">788</td></tr>
    <tr><th id="stub_1_31" scope="row" class="gt_row gt_left gt_stub">weebcel</th>
<td headers="stub_1_31 n" class="gt_row gt_right">758</td></tr>
    <tr><th id="stub_1_32" scope="row" class="gt_row gt_left gt_stub">iqcel</th>
<td headers="stub_1_32 n" class="gt_row gt_right">745</td></tr>
    <tr><th id="stub_1_33" scope="row" class="gt_row gt_left gt_stub">richcel</th>
<td headers="stub_1_33 n" class="gt_row gt_right">723</td></tr>
    <tr><th id="stub_1_34" scope="row" class="gt_row gt_left gt_stub">cancel</th>
<td headers="stub_1_34 n" class="gt_row gt_right">661</td></tr>
    <tr><th id="stub_1_35" scope="row" class="gt_row gt_left gt_stub">framecel</th>
<td headers="stub_1_35 n" class="gt_row gt_right">604</td></tr>
    <tr><th id="stub_1_36" scope="row" class="gt_row gt_left gt_stub">bluecel</th>
<td headers="stub_1_36 n" class="gt_row gt_right">570</td></tr>
    <tr><th id="stub_1_37" scope="row" class="gt_row gt_left gt_stub">fellowcel</th>
<td headers="stub_1_37 n" class="gt_row gt_right">561</td></tr>
    <tr><th id="stub_1_38" scope="row" class="gt_row gt_left gt_stub">sandcel</th>
<td headers="stub_1_38 n" class="gt_row gt_right">521</td></tr>
    <tr><th id="stub_1_39" scope="row" class="gt_row gt_left gt_stub">gaycel</th>
<td headers="stub_1_39 n" class="gt_row gt_right">508</td></tr>
    <tr><th id="stub_1_40" scope="row" class="gt_row gt_left gt_stub">ethnicel</th>
<td headers="stub_1_40 n" class="gt_row gt_right">488</td></tr>
    <tr><th id="stub_1_41" scope="row" class="gt_row gt_left gt_stub">stormfrontcel</th>
<td headers="stub_1_41 n" class="gt_row gt_right">459</td></tr>
    <tr><th id="stub_1_42" scope="row" class="gt_row gt_left gt_stub">animecel</th>
<td headers="stub_1_42 n" class="gt_row gt_right">443</td></tr>
    <tr><th id="stub_1_43" scope="row" class="gt_row gt_left gt_stub">2019cel</th>
<td headers="stub_1_43 n" class="gt_row gt_right">439</td></tr>
    <tr><th id="stub_1_44" scope="row" class="gt_row gt_left gt_stub">2017cel</th>
<td headers="stub_1_44 n" class="gt_row gt_right">436</td></tr>
    <tr><th id="stub_1_45" scope="row" class="gt_row gt_left gt_stub">itcel</th>
<td headers="stub_1_45 n" class="gt_row gt_right">420</td></tr>
    <tr><th id="stub_1_46" scope="row" class="gt_row gt_left gt_stub">slavcel</th>
<td headers="stub_1_46 n" class="gt_row gt_right">399</td></tr>
    <tr><th id="stub_1_47" scope="row" class="gt_row gt_left gt_stub">tallcel</th>
<td headers="stub_1_47 n" class="gt_row gt_right">397</td></tr>
    <tr><th id="stub_1_48" scope="row" class="gt_row gt_left gt_stub">shortcel</th>
<td headers="stub_1_48 n" class="gt_row gt_right">373</td></tr>
    <tr><th id="stub_1_49" scope="row" class="gt_row gt_left gt_stub">rbraincel</th>
<td headers="stub_1_49 n" class="gt_row gt_right">367</td></tr>
    <tr><th id="stub_1_50" scope="row" class="gt_row gt_left gt_stub">chadcel</th>
<td headers="stub_1_50 n" class="gt_row gt_right">366</td></tr>
    <tr><th id="stub_1_51" scope="row" class="gt_row gt_left gt_stub">stemcel</th>
<td headers="stub_1_51 n" class="gt_row gt_right">345</td></tr>
    <tr><th id="stub_1_52" scope="row" class="gt_row gt_left gt_stub">2020cel</th>
<td headers="stub_1_52 n" class="gt_row gt_right">336</td></tr>
    <tr><th id="stub_1_53" scope="row" class="gt_row gt_left gt_stub">neetcel</th>
<td headers="stub_1_53 n" class="gt_row gt_right">316</td></tr>
    <tr><th id="stub_1_54" scope="row" class="gt_row gt_left gt_stub">diocel</th>
<td headers="stub_1_54 n" class="gt_row gt_right">313</td></tr>
    <tr><th id="stub_1_55" scope="row" class="gt_row gt_left gt_stub">pedocel</th>
<td headers="stub_1_55 n" class="gt_row gt_right">313</td></tr>
    <tr><th id="stub_1_56" scope="row" class="gt_row gt_left gt_stub">teencel</th>
<td headers="stub_1_56 n" class="gt_row gt_right">313</td></tr>
    <tr><th id="stub_1_57" scope="row" class="gt_row gt_left gt_stub">ukcel</th>
<td headers="stub_1_57 n" class="gt_row gt_right">313</td></tr>
    <tr><th id="stub_1_58" scope="row" class="gt_row gt_left gt_stub">2018cel</th>
<td headers="stub_1_58 n" class="gt_row gt_right">304</td></tr>
    <tr><th id="stub_1_59" scope="row" class="gt_row gt_left gt_stub">cel</th>
<td headers="stub_1_59 n" class="gt_row gt_right">299</td></tr>
    <tr><th id="stub_1_60" scope="row" class="gt_row gt_left gt_stub">kikecel</th>
<td headers="stub_1_60 n" class="gt_row gt_right">297</td></tr>
    <tr><th id="stub_1_61" scope="row" class="gt_row gt_left gt_stub">ethniccel</th>
<td headers="stub_1_61 n" class="gt_row gt_right">282</td></tr>
    <tr><th id="stub_1_62" scope="row" class="gt_row gt_left gt_stub">baldcel</th>
<td headers="stub_1_62 n" class="gt_row gt_right">277</td></tr>
    <tr><th id="stub_1_63" scope="row" class="gt_row gt_left gt_stub">muslimcel</th>
<td headers="stub_1_63 n" class="gt_row gt_right">271</td></tr>
    <tr><th id="stub_1_64" scope="row" class="gt_row gt_left gt_stub">ogrecel</th>
<td headers="stub_1_64 n" class="gt_row gt_right">267</td></tr>
    <tr><th id="stub_1_65" scope="row" class="gt_row gt_left gt_stub">standardcel</th>
<td headers="stub_1_65 n" class="gt_row gt_right">267</td></tr>
    <tr><th id="stub_1_66" scope="row" class="gt_row gt_left gt_stub">junecel</th>
<td headers="stub_1_66 n" class="gt_row gt_right">262</td></tr>
    <tr><th id="stub_1_67" scope="row" class="gt_row gt_left gt_stub">burgercel</th>
<td headers="stub_1_67 n" class="gt_row gt_right">259</td></tr>
    <tr><th id="stub_1_68" scope="row" class="gt_row gt_left gt_stub">wristcel</th>
<td headers="stub_1_68 n" class="gt_row gt_right">252</td></tr>
    <tr><th id="stub_1_69" scope="row" class="gt_row gt_left gt_stub">ksgcel</th>
<td headers="stub_1_69 n" class="gt_row gt_right">251</td></tr>
    <tr><th id="stub_1_70" scope="row" class="gt_row gt_left gt_stub">modcel</th>
<td headers="stub_1_70 n" class="gt_row gt_right">241</td></tr>
    <tr><th id="stub_1_71" scope="row" class="gt_row gt_left gt_stub">locationcel</th>
<td headers="stub_1_71 n" class="gt_row gt_right">233</td></tr>
    <tr><th id="stub_1_72" scope="row" class="gt_row gt_left gt_stub">mayocel</th>
<td headers="stub_1_72 n" class="gt_row gt_right">231</td></tr>
    <tr><th id="stub_1_73" scope="row" class="gt_row gt_left gt_stub">lowiqcel</th>
<td headers="stub_1_73 n" class="gt_row gt_right">229</td></tr>
    <tr><th id="stub_1_74" scope="row" class="gt_row gt_left gt_stub">ppecel</th>
<td headers="stub_1_74 n" class="gt_row gt_right">229</td></tr>
    <tr><th id="stub_1_75" scope="row" class="gt_row gt_left gt_stub">sergeantincel</th>
<td headers="stub_1_75 n" class="gt_row gt_right">228</td></tr>
    <tr><th id="stub_1_76" scope="row" class="gt_row gt_left gt_stub">sfcel</th>
<td headers="stub_1_76 n" class="gt_row gt_right">223</td></tr>
    <tr><th id="stub_1_77" scope="row" class="gt_row gt_left gt_stub">2022cel</th>
<td headers="stub_1_77 n" class="gt_row gt_right">220</td></tr>
    <tr><th id="stub_1_78" scope="row" class="gt_row gt_left gt_stub">cuckcel</th>
<td headers="stub_1_78 n" class="gt_row gt_right">213</td></tr>
    <tr><th id="stub_1_79" scope="row" class="gt_row gt_left gt_stub">voicecel</th>
<td headers="stub_1_79 n" class="gt_row gt_right">208</td></tr>
    <tr><th id="stub_1_80" scope="row" class="gt_row gt_left gt_stub">nonincel</th>
<td headers="stub_1_80 n" class="gt_row gt_right">205</td></tr>
    <tr><th id="stub_1_81" scope="row" class="gt_row gt_left gt_stub">ogcel</th>
<td headers="stub_1_81 n" class="gt_row gt_right">202</td></tr>
    <tr><th id="stub_1_82" scope="row" class="gt_row gt_left gt_stub">jewcel</th>
<td headers="stub_1_82 n" class="gt_row gt_right">201</td></tr>
    <tr><th id="stub_1_83" scope="row" class="gt_row gt_left gt_stub">dickcel</th>
<td headers="stub_1_83 n" class="gt_row gt_right">200</td></tr>
    <tr><th id="stub_1_84" scope="row" class="gt_row gt_left gt_stub">studycel</th>
<td headers="stub_1_84 n" class="gt_row gt_right">198</td></tr>
    <tr><th id="stub_1_85" scope="row" class="gt_row gt_left gt_stub">novembercel</th>
<td headers="stub_1_85 n" class="gt_row gt_right">197</td></tr>
    <tr><th id="stub_1_86" scope="row" class="gt_row gt_left gt_stub">antiincel</th>
<td headers="stub_1_86 n" class="gt_row gt_right">196</td></tr>
    <tr><th id="stub_1_87" scope="row" class="gt_row gt_left gt_stub">nazicel</th>
<td headers="stub_1_87 n" class="gt_row gt_right">194</td></tr>
    <tr><th id="stub_1_88" scope="row" class="gt_row gt_left gt_stub">eskimocel</th>
<td headers="stub_1_88 n" class="gt_row gt_right">185</td></tr>
    <tr><th id="stub_1_89" scope="row" class="gt_row gt_left gt_stub">finncel</th>
<td headers="stub_1_89 n" class="gt_row gt_right">183</td></tr>
    <tr><th id="stub_1_90" scope="row" class="gt_row gt_left gt_stub">rightfulcel</th>
<td headers="stub_1_90 n" class="gt_row gt_right">182</td></tr>
    <tr><th id="stub_1_91" scope="row" class="gt_row gt_left gt_stub">stormcel</th>
<td headers="stub_1_91 n" class="gt_row gt_right">177</td></tr>
    <tr><th id="stub_1_92" scope="row" class="gt_row gt_left gt_stub">augustcel</th>
<td headers="stub_1_92 n" class="gt_row gt_right">174</td></tr>
    <tr><th id="stub_1_93" scope="row" class="gt_row gt_left gt_stub">octobercel</th>
<td headers="stub_1_93 n" class="gt_row gt_right">174</td></tr>
    <tr><th id="stub_1_94" scope="row" class="gt_row gt_left gt_stub">usacel</th>
<td headers="stub_1_94 n" class="gt_row gt_right">174</td></tr>
    <tr><th id="stub_1_95" scope="row" class="gt_row gt_left gt_stub">femdomcel</th>
<td headers="stub_1_95 n" class="gt_row gt_right">173</td></tr>
    <tr><th id="stub_1_96" scope="row" class="gt_row gt_left gt_stub">bagelcel</th>
<td headers="stub_1_96 n" class="gt_row gt_right">170</td></tr>
    <tr><th id="stub_1_97" scope="row" class="gt_row gt_left gt_stub">nearcel</th>
<td headers="stub_1_97 n" class="gt_row gt_right">168</td></tr>
    <tr><th id="stub_1_98" scope="row" class="gt_row gt_left gt_stub">arabcel</th>
<td headers="stub_1_98 n" class="gt_row gt_right">161</td></tr>
    <tr><th id="stub_1_99" scope="row" class="gt_row gt_left gt_stub">autistcel</th>
<td headers="stub_1_99 n" class="gt_row gt_right">160</td></tr>
    <tr><th id="stub_1_100" scope="row" class="gt_row gt_left gt_stub">chatcel</th>
<td headers="stub_1_100 n" class="gt_row gt_right">159</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
cels2 <- cels_by_post %>%
  distinct() %>%
  group_by(post_id) %>%
  filter(n()>1) %>%
  arrange(cel) %>%
  summarise(cel=str_flatten(cel, collapse=", "), .groups="drop") %>%
  count(cel) %>% 
  arrange(desc(n))
```

``` r
cels2 %>% 
  write_tsv(here("data/output/jiemakel/cels2.tsv"),na="",quote="needed")
```

``` r
cels2 %>%
  head(n=100) %>%
gt(rowname_col="cel") %>%
  fmt_integer(n)
```

<div id="jcckpiwwge" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jcckpiwwge table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jcckpiwwge thead, #jcckpiwwge tbody, #jcckpiwwge tfoot, #jcckpiwwge tr, #jcckpiwwge td, #jcckpiwwge th {
  border-style: none;
}
&#10;#jcckpiwwge p {
  margin: 0;
  padding: 0;
}
&#10;#jcckpiwwge .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jcckpiwwge .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#jcckpiwwge .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#jcckpiwwge .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#jcckpiwwge .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#jcckpiwwge .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jcckpiwwge .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jcckpiwwge .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#jcckpiwwge .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jcckpiwwge .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#jcckpiwwge .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#jcckpiwwge .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jcckpiwwge .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jcckpiwwge .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#jcckpiwwge .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jcckpiwwge .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#jcckpiwwge .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jcckpiwwge .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jcckpiwwge .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jcckpiwwge .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jcckpiwwge .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jcckpiwwge .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jcckpiwwge .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jcckpiwwge .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jcckpiwwge .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jcckpiwwge .gt_left {
  text-align: left;
}
&#10;#jcckpiwwge .gt_center {
  text-align: center;
}
&#10;#jcckpiwwge .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jcckpiwwge .gt_font_normal {
  font-weight: normal;
}
&#10;#jcckpiwwge .gt_font_bold {
  font-weight: bold;
}
&#10;#jcckpiwwge .gt_font_italic {
  font-style: italic;
}
&#10;#jcckpiwwge .gt_super {
  font-size: 65%;
}
&#10;#jcckpiwwge .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jcckpiwwge .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jcckpiwwge .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jcckpiwwge .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jcckpiwwge .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jcckpiwwge .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jcckpiwwge .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">fakecel, incel</th>
<td headers="stub_1_1 n" class="gt_row gt_right">1,414</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">incel, truecel</th>
<td headers="stub_1_2 n" class="gt_row gt_right">929</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">incel, volcel</th>
<td headers="stub_1_3 n" class="gt_row gt_right">525</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">incel, trucel</th>
<td headers="stub_1_4 n" class="gt_row gt_right">365</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">fakecel, truecel</th>
<td headers="stub_1_5 n" class="gt_row gt_right">357</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">httpsincel, incel</th>
<td headers="stub_1_6 n" class="gt_row gt_right">355</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">brocel, incel</th>
<td headers="stub_1_7 n" class="gt_row gt_right">350</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">incel, mentalcel</th>
<td headers="stub_1_8 n" class="gt_row gt_right">281</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub">greycel, incel</th>
<td headers="stub_1_9 n" class="gt_row gt_right">232</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub">braincel, incel</th>
<td headers="stub_1_10 n" class="gt_row gt_right">230</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub">fatcel, volcel</th>
<td headers="stub_1_11 n" class="gt_row gt_right">226</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub">incel, rincel</th>
<td headers="stub_1_12 n" class="gt_row gt_right">222</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub">femcel, incel</th>
<td headers="stub_1_13 n" class="gt_row gt_right">211</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_left gt_stub">incel, ricecel</th>
<td headers="stub_1_14 n" class="gt_row gt_right">172</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_left gt_stub">graycel, incel</th>
<td headers="stub_1_15 n" class="gt_row gt_right">170</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_left gt_stub">incel, oldcel</th>
<td headers="stub_1_16 n" class="gt_row gt_right">166</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_left gt_stub">escortcel, incel</th>
<td headers="stub_1_17 n" class="gt_row gt_right">155</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_left gt_stub">incel, youngcel</th>
<td headers="stub_1_18 n" class="gt_row gt_right">151</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_left gt_stub">fakecel, incel, truecel</th>
<td headers="stub_1_19 n" class="gt_row gt_right">147</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_left gt_stub">graycel, greycel</th>
<td headers="stub_1_20 n" class="gt_row gt_right">127</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_left gt_stub">currycel, incel</th>
<td headers="stub_1_21 n" class="gt_row gt_right">126</td></tr>
    <tr><th id="stub_1_22" scope="row" class="gt_row gt_left gt_stub">blackcel, incel</th>
<td headers="stub_1_22 n" class="gt_row gt_right">122</td></tr>
    <tr><th id="stub_1_23" scope="row" class="gt_row gt_left gt_stub">fakecel, trucel</th>
<td headers="stub_1_23 n" class="gt_row gt_right">122</td></tr>
    <tr><th id="stub_1_24" scope="row" class="gt_row gt_left gt_stub">gymcel, incel</th>
<td headers="stub_1_24 n" class="gt_row gt_right">122</td></tr>
    <tr><th id="stub_1_25" scope="row" class="gt_row gt_left gt_stub">incel, whitecel</th>
<td headers="stub_1_25 n" class="gt_row gt_right">108</td></tr>
    <tr><th id="stub_1_26" scope="row" class="gt_row gt_left gt_stub">bluecel, greycel</th>
<td headers="stub_1_26 n" class="gt_row gt_right">93</td></tr>
    <tr><th id="stub_1_27" scope="row" class="gt_row gt_left gt_stub">currycel, ricecel</th>
<td headers="stub_1_27 n" class="gt_row gt_right">87</td></tr>
    <tr><th id="stub_1_28" scope="row" class="gt_row gt_left gt_stub">fakecel, volcel</th>
<td headers="stub_1_28 n" class="gt_row gt_right">85</td></tr>
    <tr><th id="stub_1_29" scope="row" class="gt_row gt_left gt_stub">trucel, truecel</th>
<td headers="stub_1_29 n" class="gt_row gt_right">79</td></tr>
    <tr><th id="stub_1_30" scope="row" class="gt_row gt_left gt_stub">incel, nonincel</th>
<td headers="stub_1_30 n" class="gt_row gt_right">76</td></tr>
    <tr><th id="stub_1_31" scope="row" class="gt_row gt_left gt_stub">fakecel, mentalcel</th>
<td headers="stub_1_31 n" class="gt_row gt_right">75</td></tr>
    <tr><th id="stub_1_32" scope="row" class="gt_row gt_left gt_stub">incel, lostcel</th>
<td headers="stub_1_32 n" class="gt_row gt_right">73</td></tr>
    <tr><th id="stub_1_33" scope="row" class="gt_row gt_left gt_stub">antiincel, incel</th>
<td headers="stub_1_33 n" class="gt_row gt_right">71</td></tr>
    <tr><th id="stub_1_34" scope="row" class="gt_row gt_left gt_stub">currycel, trucel</th>
<td headers="stub_1_34 n" class="gt_row gt_right">70</td></tr>
    <tr><th id="stub_1_35" scope="row" class="gt_row gt_left gt_stub">fakecel, youngcel</th>
<td headers="stub_1_35 n" class="gt_row gt_right">69</td></tr>
    <tr><th id="stub_1_36" scope="row" class="gt_row gt_left gt_stub">blackops2cel, incel</th>
<td headers="stub_1_36 n" class="gt_row gt_right">66</td></tr>
    <tr><th id="stub_1_37" scope="row" class="gt_row gt_left gt_stub">oldcel, youngcel</th>
<td headers="stub_1_37 n" class="gt_row gt_right">64</td></tr>
    <tr><th id="stub_1_38" scope="row" class="gt_row gt_left gt_stub">incel, poorcel</th>
<td headers="stub_1_38 n" class="gt_row gt_right">56</td></tr>
    <tr><th id="stub_1_39" scope="row" class="gt_row gt_left gt_stub">femcel, volcel</th>
<td headers="stub_1_39 n" class="gt_row gt_right">55</td></tr>
    <tr><th id="stub_1_40" scope="row" class="gt_row gt_left gt_stub">fakecel, graycel</th>
<td headers="stub_1_40 n" class="gt_row gt_right">54</td></tr>
    <tr><th id="stub_1_41" scope="row" class="gt_row gt_left gt_stub">fakecel, greycel</th>
<td headers="stub_1_41 n" class="gt_row gt_right">53</td></tr>
    <tr><th id="stub_1_42" scope="row" class="gt_row gt_left gt_stub">fatcel, incel</th>
<td headers="stub_1_42 n" class="gt_row gt_right">53</td></tr>
    <tr><th id="stub_1_43" scope="row" class="gt_row gt_left gt_stub">excel, incel</th>
<td headers="stub_1_43 n" class="gt_row gt_right">52</td></tr>
    <tr><th id="stub_1_44" scope="row" class="gt_row gt_left gt_stub">fakecel, whitecel</th>
<td headers="stub_1_44 n" class="gt_row gt_right">49</td></tr>
    <tr><th id="stub_1_45" scope="row" class="gt_row gt_left gt_stub">incel, iqcel</th>
<td headers="stub_1_45 n" class="gt_row gt_right">49</td></tr>
    <tr><th id="stub_1_46" scope="row" class="gt_row gt_left gt_stub">incel, richcel</th>
<td headers="stub_1_46 n" class="gt_row gt_right">48</td></tr>
    <tr><th id="stub_1_47" scope="row" class="gt_row gt_left gt_stub">aaaaaaaaaaacel, cheesecel, daydreamincel, diocel, incel, itsover4cel, manicel, rightfulcel, singleplayercel</th>
<td headers="stub_1_47 n" class="gt_row gt_right">47</td></tr>
    <tr><th id="stub_1_48" scope="row" class="gt_row gt_left gt_stub">mentalcel, truecel</th>
<td headers="stub_1_48 n" class="gt_row gt_right">47</td></tr>
    <tr><th id="stub_1_49" scope="row" class="gt_row gt_left gt_stub">incel, itcel</th>
<td headers="stub_1_49 n" class="gt_row gt_right">44</td></tr>
    <tr><th id="stub_1_50" scope="row" class="gt_row gt_left gt_stub">mentalcel, volcel</th>
<td headers="stub_1_50 n" class="gt_row gt_right">44</td></tr>
    <tr><th id="stub_1_51" scope="row" class="gt_row gt_left gt_stub">gaycel, maycel</th>
<td headers="stub_1_51 n" class="gt_row gt_right">43</td></tr>
    <tr><th id="stub_1_52" scope="row" class="gt_row gt_left gt_stub">incel, rbraincel</th>
<td headers="stub_1_52 n" class="gt_row gt_right">43</td></tr>
    <tr><th id="stub_1_53" scope="row" class="gt_row gt_left gt_stub">lostcel, trucel</th>
<td headers="stub_1_53 n" class="gt_row gt_right">43</td></tr>
    <tr><th id="stub_1_54" scope="row" class="gt_row gt_left gt_stub">incel, shortcel</th>
<td headers="stub_1_54 n" class="gt_row gt_right">42</td></tr>
    <tr><th id="stub_1_55" scope="row" class="gt_row gt_left gt_stub">incel, trucel, truecel</th>
<td headers="stub_1_55 n" class="gt_row gt_right">42</td></tr>
    <tr><th id="stub_1_56" scope="row" class="gt_row gt_left gt_stub">fakecel, incel, volcel</th>
<td headers="stub_1_56 n" class="gt_row gt_right">41</td></tr>
    <tr><th id="stub_1_57" scope="row" class="gt_row gt_left gt_stub">incel, ritalincel</th>
<td headers="stub_1_57 n" class="gt_row gt_right">41</td></tr>
    <tr><th id="stub_1_58" scope="row" class="gt_row gt_left gt_stub">truecel, volcel</th>
<td headers="stub_1_58 n" class="gt_row gt_right">41</td></tr>
    <tr><th id="stub_1_59" scope="row" class="gt_row gt_left gt_stub">ethnicel, incel</th>
<td headers="stub_1_59 n" class="gt_row gt_right">40</td></tr>
    <tr><th id="stub_1_60" scope="row" class="gt_row gt_left gt_stub">fbicel, incel</th>
<td headers="stub_1_60 n" class="gt_row gt_right">40</td></tr>
    <tr><th id="stub_1_61" scope="row" class="gt_row gt_left gt_stub">junecel, maycel</th>
<td headers="stub_1_61 n" class="gt_row gt_right">38</td></tr>
    <tr><th id="stub_1_62" scope="row" class="gt_row gt_left gt_stub">incel, maycel</th>
<td headers="stub_1_62 n" class="gt_row gt_right">37</td></tr>
    <tr><th id="stub_1_63" scope="row" class="gt_row gt_left gt_stub">incel, newcel</th>
<td headers="stub_1_63 n" class="gt_row gt_right">37</td></tr>
    <tr><th id="stub_1_64" scope="row" class="gt_row gt_left gt_stub">gaycel, greycel</th>
<td headers="stub_1_64 n" class="gt_row gt_right">35</td></tr>
    <tr><th id="stub_1_65" scope="row" class="gt_row gt_left gt_stub">incel, stormfrontcel</th>
<td headers="stub_1_65 n" class="gt_row gt_right">34</td></tr>
    <tr><th id="stub_1_66" scope="row" class="gt_row gt_left gt_stub">cel, incel</th>
<td headers="stub_1_66 n" class="gt_row gt_right">33</td></tr>
    <tr><th id="stub_1_67" scope="row" class="gt_row gt_left gt_stub">framecel, incel</th>
<td headers="stub_1_67 n" class="gt_row gt_right">33</td></tr>
    <tr><th id="stub_1_68" scope="row" class="gt_row gt_left gt_stub">standardcel, volcel</th>
<td headers="stub_1_68 n" class="gt_row gt_right">33</td></tr>
    <tr><th id="stub_1_69" scope="row" class="gt_row gt_left gt_stub">rbraincel, rincel</th>
<td headers="stub_1_69 n" class="gt_row gt_right">32</td></tr>
    <tr><th id="stub_1_70" scope="row" class="gt_row gt_left gt_stub">fakecel, incel, mentalcel</th>
<td headers="stub_1_70 n" class="gt_row gt_right">31</td></tr>
    <tr><th id="stub_1_71" scope="row" class="gt_row gt_left gt_stub">escortcel, fakecel</th>
<td headers="stub_1_71 n" class="gt_row gt_right">30</td></tr>
    <tr><th id="stub_1_72" scope="row" class="gt_row gt_left gt_stub">fellowcel, incel</th>
<td headers="stub_1_72 n" class="gt_row gt_right">30</td></tr>
    <tr><th id="stub_1_73" scope="row" class="gt_row gt_left gt_stub">gaycel, incel</th>
<td headers="stub_1_73 n" class="gt_row gt_right">30</td></tr>
    <tr><th id="stub_1_74" scope="row" class="gt_row gt_left gt_stub">incel, nearcel</th>
<td headers="stub_1_74 n" class="gt_row gt_right">30</td></tr>
    <tr><th id="stub_1_75" scope="row" class="gt_row gt_left gt_stub">incel, neetcel</th>
<td headers="stub_1_75 n" class="gt_row gt_right">30</td></tr>
    <tr><th id="stub_1_76" scope="row" class="gt_row gt_left gt_stub">bluecel, graycel</th>
<td headers="stub_1_76 n" class="gt_row gt_right">29</td></tr>
    <tr><th id="stub_1_77" scope="row" class="gt_row gt_left gt_stub">brocel, fakecel</th>
<td headers="stub_1_77 n" class="gt_row gt_right">29</td></tr>
    <tr><th id="stub_1_78" scope="row" class="gt_row gt_left gt_stub">incel, mentalcel, truecel</th>
<td headers="stub_1_78 n" class="gt_row gt_right">29</td></tr>
    <tr><th id="stub_1_79" scope="row" class="gt_row gt_left gt_stub">incel, pedocel</th>
<td headers="stub_1_79 n" class="gt_row gt_right">29</td></tr>
    <tr><th id="stub_1_80" scope="row" class="gt_row gt_left gt_stub">braincel, rincel</th>
<td headers="stub_1_80 n" class="gt_row gt_right">28</td></tr>
    <tr><th id="stub_1_81" scope="row" class="gt_row gt_left gt_stub">fakecel, httpsincel</th>
<td headers="stub_1_81 n" class="gt_row gt_right">28</td></tr>
    <tr><th id="stub_1_82" scope="row" class="gt_row gt_left gt_stub">incel, locationcel</th>
<td headers="stub_1_82 n" class="gt_row gt_right">28</td></tr>
    <tr><th id="stub_1_83" scope="row" class="gt_row gt_left gt_stub">incel, standardcel</th>
<td headers="stub_1_83 n" class="gt_row gt_right">28</td></tr>
    <tr><th id="stub_1_84" scope="row" class="gt_row gt_left gt_stub">brocel, trucel</th>
<td headers="stub_1_84 n" class="gt_row gt_right">27</td></tr>
    <tr><th id="stub_1_85" scope="row" class="gt_row gt_left gt_stub">chadcel, incel</th>
<td headers="stub_1_85 n" class="gt_row gt_right">27</td></tr>
    <tr><th id="stub_1_86" scope="row" class="gt_row gt_left gt_stub">incel, teencel</th>
<td headers="stub_1_86 n" class="gt_row gt_right">27</td></tr>
    <tr><th id="stub_1_87" scope="row" class="gt_row gt_left gt_stub">cancel, incel</th>
<td headers="stub_1_87 n" class="gt_row gt_right">26</td></tr>
    <tr><th id="stub_1_88" scope="row" class="gt_row gt_left gt_stub">fakecel, incel, trucel</th>
<td headers="stub_1_88 n" class="gt_row gt_right">26</td></tr>
    <tr><th id="stub_1_89" scope="row" class="gt_row gt_left gt_stub">oldcel, truecel</th>
<td headers="stub_1_89 n" class="gt_row gt_right">26</td></tr>
    <tr><th id="stub_1_90" scope="row" class="gt_row gt_left gt_stub">brocel, graycel</th>
<td headers="stub_1_90 n" class="gt_row gt_right">25</td></tr>
    <tr><th id="stub_1_91" scope="row" class="gt_row gt_left gt_stub">incel, tallcel</th>
<td headers="stub_1_91 n" class="gt_row gt_right">25</td></tr>
    <tr><th id="stub_1_92" scope="row" class="gt_row gt_left gt_stub">fakecel, fatcel</th>
<td headers="stub_1_92 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_93" scope="row" class="gt_row gt_left gt_stub">fakecel, ricecel</th>
<td headers="stub_1_93 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_94" scope="row" class="gt_row gt_left gt_stub">greycel, truecel</th>
<td headers="stub_1_94 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_95" scope="row" class="gt_row gt_left gt_stub">incel, kikecel</th>
<td headers="stub_1_95 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_96" scope="row" class="gt_row gt_left gt_stub">incel, ogrecel</th>
<td headers="stub_1_96 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_97" scope="row" class="gt_row gt_left gt_stub">incel, ukcel</th>
<td headers="stub_1_97 n" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_98" scope="row" class="gt_row gt_left gt_stub">cuckcel, incel</th>
<td headers="stub_1_98 n" class="gt_row gt_right">23</td></tr>
    <tr><th id="stub_1_99" scope="row" class="gt_row gt_left gt_stub">currycel, truecel</th>
<td headers="stub_1_99 n" class="gt_row gt_right">22</td></tr>
    <tr><th id="stub_1_100" scope="row" class="gt_row gt_left gt_stub">ethniccel, incel</th>
<td headers="stub_1_100 n" class="gt_row gt_right">22</td></tr>
  </tbody>
  &#10;  
</table>
</div>
