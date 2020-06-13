COVID-19 Data from NYT for Counties of Interest
================

Updated: Sat Jun 13 10:06:17 2020

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
  sample_n(5) %>%
  pander::pandoc.table(caption = 'NYT COVID Tracking Data')
```

<table style="width:90%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="20%" />
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
<td align="center">2020-04-17</td>
<td align="center">Humboldt</td>
<td align="center">California</td>
<td align="center">06023</td>
<td align="center">52</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-06-03</td>
<td align="center">Bannock</td>
<td align="center">Idaho</td>
<td align="center">16005</td>
<td align="center">37</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-05-08</td>
<td align="center">Pinellas</td>
<td align="center">Florida</td>
<td align="center">12103</td>
<td align="center">828</td>
<td align="center">59</td>
</tr>
<tr class="even">
<td align="center">2020-05-12</td>
<td align="center">Sevier</td>
<td align="center">Tennessee</td>
<td align="center">47155</td>
<td align="center">65</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">2020-04-22</td>
<td align="center">Presque Isle</td>
<td align="center">Michigan</td>
<td align="center">26141</td>
<td align="center">6</td>
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
  # fips is NA when the data is at the state level
  filter(!is.na(fips)) %>%
  left_join(select(usmap::countypop, fips, population = pop_2015), 
            by = 'fips') %>%
  filter(county %in% county_list) %>% 
  # we don't need everything, keep only the region, day, and counts
  select(county, date, cases, deaths, population) %>%
  # tidy the data by putting all observations in rows
  gather(key = type, value = number, -date, -county, -population) %>%
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
  sample_n(5) %>%
  pander::pandoc.table(caption = 'Tracking Data Ready for Plotting')
```

<table style="width:99%;">
<caption>Tracking Data Ready for Plotting</caption>
<colgroup>
<col width="30%" />
<col width="18%" />
<col width="18%" />
<col width="12%" />
<col width="12%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">county</th>
<th align="center">date</th>
<th align="center">population</th>
<th align="center">type</th>
<th align="center">number</th>
<th align="center">new</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-31</td>
<td align="center">371398</td>
<td align="center">cases</td>
<td align="center">6933</td>
<td align="center">83</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-17</td>
<td align="center">1638215</td>
<td align="center">deaths</td>
<td align="center">85</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-18</td>
<td align="center">1638215</td>
<td align="center">deaths</td>
<td align="center">41</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-14</td>
<td align="center">371398</td>
<td align="center">cases</td>
<td align="center">1731</td>
<td align="center">85</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-16</td>
<td align="center">1638215</td>
<td align="center">deaths</td>
<td align="center">85</td>
<td align="center">1</td>
</tr>
</tbody>
</table>

Plot the data
-------------

This is what the straight-up cumulative data looks like.

``` r
# time for plotting
library(ggplot2)
ggplot(df, aes(x = date, y = number/population*1e5, fill = type)) +
  geom_area(alpha    = 0.5,
            position = 'identity',
            show.legend = FALSE) +
  scale_y_log10(name   = 'Per 100,000 People',
                labels = scales::comma) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  geom_hline(yintercept = 10^(-2:3), size = 0.5, color = 'white') +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-cumulative-1.png)

This looks at the daily increase.

``` r
ggplot(df, aes(x = date, y = new/population*1e5, color = type)) +
  geom_hline(yintercept = 10^(-2:3), size = 0.1) +
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_log10(name   = 'Daily Cases Per 100,000 People', 
                breaks = 10^(-2:3),
                limits = c(0.01,1000),
                labels = scales::comma) +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-daily-1.png)

Because of the noise in the daily data, maybe a weekly increase is a better idea.

``` r
df %>% 
  dplyr::group_by(lubridate::week(date), type, county, population) %>% 
  dplyr::summarize(new = sum(new), date = min(date)) %>%
  ggplot(aes(x = date, y = new/population*1e5, color = type)) +
  geom_hline(yintercept = 10^(-2:3), size = 0.1) +
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_log10(name   = 'Weekly Cases Per 100,000 People', 
                breaks = 10^(-2:3),
                limits = c(0.01,1000),
                labels = scales::comma) +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-weekly-1.png)
