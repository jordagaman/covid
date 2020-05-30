COVID-19 Data from NYT for Counties of Interest
================

Updated: Sat May 30 13:46:14 2020

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

<table style="width:99%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="23%" />
<col width="23%" />
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
<td align="center">2020-03-07</td>
<td align="center">Unknown</td>
<td align="center">Rhode Island</td>
<td align="center">NA</td>
<td align="center">3</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-20</td>
<td align="center">Perquimans</td>
<td align="center">North Carolina</td>
<td align="center">37143</td>
<td align="center">22</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">2020-05-16</td>
<td align="center">Cumberland</td>
<td align="center">Maine</td>
<td align="center">23005</td>
<td align="center">821</td>
<td align="center">35</td>
</tr>
<tr class="even">
<td align="center">2020-05-28</td>
<td align="center">Schoharie</td>
<td align="center">New York</td>
<td align="center">36095</td>
<td align="center">49</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-26</td>
<td align="center">Lake</td>
<td align="center">South Dakota</td>
<td align="center">46079</td>
<td align="center">4</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-03-14</td>
<td align="center">Collier</td>
<td align="center">Florida</td>
<td align="center">12021</td>
<td align="center">3</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-05-05</td>
<td align="center">Spokane</td>
<td align="center">Washington</td>
<td align="center">53063</td>
<td align="center">379</td>
<td align="center">23</td>
</tr>
<tr class="even">
<td align="center">2020-05-28</td>
<td align="center">Garfield</td>
<td align="center">Colorado</td>
<td align="center">08045</td>
<td align="center">135</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">2020-04-20</td>
<td align="center">Pitt</td>
<td align="center">North Carolina</td>
<td align="center">37147</td>
<td align="center">93</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-05-06</td>
<td align="center">Craighead</td>
<td align="center">Arkansas</td>
<td align="center">05031</td>
<td align="center">88</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-26</td>
<td align="center">Orange</td>
<td align="center">Texas</td>
<td align="center">48361</td>
<td align="center">64</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-09</td>
<td align="center">St. Louis city</td>
<td align="center">Missouri</td>
<td align="center">29510</td>
<td align="center">1484</td>
<td align="center">86</td>
</tr>
<tr class="odd">
<td align="center">2020-03-30</td>
<td align="center">Hardin</td>
<td align="center">Texas</td>
<td align="center">48199</td>
<td align="center">9</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-04-22</td>
<td align="center">Newton</td>
<td align="center">Georgia</td>
<td align="center">13217</td>
<td align="center">146</td>
<td align="center">5</td>
</tr>
<tr class="odd">
<td align="center">2020-04-28</td>
<td align="center">Caroline</td>
<td align="center">Maryland</td>
<td align="center">24011</td>
<td align="center">69</td>
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
<td align="center">2020-04-04</td>
<td align="center">cases</td>
<td align="center">586</td>
<td align="center">102</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-07</td>
<td align="center">cases</td>
<td align="center">655</td>
<td align="center">46</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-06</td>
<td align="center">cases</td>
<td align="center">1884</td>
<td align="center">54</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-08</td>
<td align="center">cases</td>
<td align="center">3</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-10</td>
<td align="center">cases</td>
<td align="center">5317</td>
<td align="center">84</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-29</td>
<td align="center">deaths</td>
<td align="center">466</td>
<td align="center">4</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-18</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-16</td>
<td align="center">cases</td>
<td align="center">5719</td>
<td align="center">46</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-24</td>
<td align="center">deaths</td>
<td align="center">2</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-02</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-14</td>
<td align="center">cases</td>
<td align="center">5612</td>
<td align="center">113</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-29</td>
<td align="center">cases</td>
<td align="center">202</td>
<td align="center">34</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-28</td>
<td align="center">cases</td>
<td align="center">3605</td>
<td align="center">172</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-27</td>
<td align="center">cases</td>
<td align="center">3070</td>
<td align="center">63</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-16</td>
<td align="center">deaths</td>
<td align="center">40</td>
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
