COVID-19 Data from NYT for Counties of Interest
================

Updated: Sun Jun 7 15:06:02 2020

Get the Data
------------

The NYT is super generous with their data. Go get it.

``` r
library(tidyverse)
library(magrittr)
curl::curl_download(
  url      = 'https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv',
  destfile = './county-cases.csv'
)
df <- read_csv('county-cases.csv') 
```

Are you curious what it looks like? It looks like this.

``` r
df %>%
  sample_n(15) %>%
  pander::pandoc.table(caption = 'NYT COVID Tracking Data')
```

<table style="width:88%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="18%" />
<col width="18%" />
<col width="11%" />
<col width="11%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">date</th>
<th align="center">county</th>
<th align="center">state</th>
<th align="center">fips</th>
<th align="center">cases</th>
<th align="center">deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2020-05-11</td>
<td align="center">Grand</td>
<td align="center">Colorado</td>
<td align="center">08049</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-03-14</td>
<td align="center">Sullivan</td>
<td align="center">Tennessee</td>
<td align="center">47163</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-19</td>
<td align="center">Shasta</td>
<td align="center">California</td>
<td align="center">06089</td>
<td align="center">26</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">2020-04-23</td>
<td align="center">Marion</td>
<td align="center">Texas</td>
<td align="center">48315</td>
<td align="center">6</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-05-02</td>
<td align="center">Butler</td>
<td align="center">Missouri</td>
<td align="center">29023</td>
<td align="center">26</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-06-04</td>
<td align="center">Sevier</td>
<td align="center">Tennessee</td>
<td align="center">47155</td>
<td align="center">97</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">2020-04-16</td>
<td align="center">Auglaize</td>
<td align="center">Ohio</td>
<td align="center">39011</td>
<td align="center">19</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-03-21</td>
<td align="center">Lake</td>
<td align="center">Illinois</td>
<td align="center">17097</td>
<td align="center">63</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-05-25</td>
<td align="center">Broadwater</td>
<td align="center">Montana</td>
<td align="center">30007</td>
<td align="center">4</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-05</td>
<td align="center">Franklin</td>
<td align="center">Georgia</td>
<td align="center">13119</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-16</td>
<td align="center">Lincoln</td>
<td align="center">Idaho</td>
<td align="center">16063</td>
<td align="center">15</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-24</td>
<td align="center">Beltrami</td>
<td align="center">Minnesota</td>
<td align="center">27007</td>
<td align="center">12</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-20</td>
<td align="center">Bourbon</td>
<td align="center">Kentucky</td>
<td align="center">21017</td>
<td align="center">8</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-04-09</td>
<td align="center">Calcasieu</td>
<td align="center">Louisiana</td>
<td align="center">22019</td>
<td align="center">209</td>
<td align="center">4</td>
</tr>
<tr class="odd">
<td align="center">2020-05-18</td>
<td align="center">Meade</td>
<td align="center">Kansas</td>
<td align="center">20119</td>
<td align="center">22</td>
<td align="center">0</td>
</tr>
</tbody>
</table>

Tidy the data
-------------

Tidy the data by filtering on the counties of interest, and gathering values of interest into a single column.

``` r
county_list = c(
  'Alameda, California', 
  #'Contra Costa, California', 
  #'Bernalillo, New Mexico', 
  #'Mecklenburg, North Carolina',
  'Mercer, New Jersey'
)
df %<>%
  # filter on the county_list
  mutate(county = paste(county, state, sep = ', ')) %>%
  filter(county %in% county_list) %>% 
  # we don't need everything, keep only the region, day, and counts
  select(county, date, cases, deaths) %>%
  # tidy the data by putting all observations in rows
  gather(key = type, value = number, -date, -county) %>%
  # group by county and type (deaths or cases) to get a delta column for new cases in a day
  group_by(county, type) %>%
  arrange(date) %>%
  mutate(new = number - lag(number, default = 0)) %>%
  ungroup()
```

Curious what it looks like now? It looks like this.

``` r
df %>%
  arrange(date) %>%
  sample_n(15) %>%
  pander::pandoc.table(caption = 'Tracking Data Ready for Plotting')
```

<table style="width:81%;">
<caption>Tracking Data Ready for Plotting</caption>
<colgroup>
<col width="30%" />
<col width="18%" />
<col width="12%" />
<col width="12%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">county</th>
<th align="center">date</th>
<th align="center">type</th>
<th align="center">number</th>
<th align="center">new</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-27</td>
<td align="center">deaths</td>
<td align="center">194</td>
<td align="center">6</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-06-03</td>
<td align="center">cases</td>
<td align="center">7033</td>
<td align="center">29</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-10</td>
<td align="center">cases</td>
<td align="center">25</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-07</td>
<td align="center">deaths</td>
<td align="center">313</td>
<td align="center">15</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-01</td>
<td align="center">deaths</td>
<td align="center">239</td>
<td align="center">8</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-30</td>
<td align="center">cases</td>
<td align="center">304</td>
<td align="center">13</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-27</td>
<td align="center">deaths</td>
<td align="center">458</td>
<td align="center">6</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-28</td>
<td align="center">cases</td>
<td align="center">3605</td>
<td align="center">172</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-05</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-28</td>
<td align="center">cases</td>
<td align="center">3118</td>
<td align="center">48</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-15</td>
<td align="center">cases</td>
<td align="center">1856</td>
<td align="center">125</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-08</td>
<td align="center">deaths</td>
<td align="center">16</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-16</td>
<td align="center">cases</td>
<td align="center">40</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-22</td>
<td align="center">deaths</td>
<td align="center">443</td>
<td align="center">5</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-23</td>
<td align="center">deaths</td>
<td align="center">447</td>
<td align="center">4</td>
</tr>
</tbody>
</table>

Plot the data
-------------

This is what the straight-up cumulative data looks like.

``` r
# time for plotting
library(ggplot2)
ggplot(df, aes(x = date, y = number, fill = type)) +
  geom_area(alpha    = 0.5,
            position = 'identity',
            show.legend = FALSE) +
  scale_y_continuous(name   = '',
                     labels = scales::comma) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-cumulative-1.png)

This looks at the daily increase.

``` r
ggplot(df, aes(x = date, y = new, color = type)) +
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_continuous(name   = 'Daily Increase') +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-daily-1.png)

Because of the noise in the daily data, maybe a weekly increase is a better idea.

``` r
df %>% 
  dplyr::group_by(lubridate::week(date), type, county) %>% 
  dplyr::summarize(new = sum(new), date = min(date)) %>%
  ggplot(aes(x = date, y = new, color = type)) +
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_continuous(name   = 'Weekly Increase') +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-weekly-1.png)
