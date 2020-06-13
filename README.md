COVID-19 Data from NYT for Counties of Interest
================

Updated: Sat Jun 13 09:47:32 2020

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

<table style="width:96%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="20%" />
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
<td align="center">Spokane</td>
<td align="center">Washington</td>
<td align="center">53063</td>
<td align="center">4</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-08</td>
<td align="center">Harding</td>
<td align="center">New Mexico</td>
<td align="center">35021</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-14</td>
<td align="center">Androscoggin</td>
<td align="center">Maine</td>
<td align="center">23001</td>
<td align="center">29</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-26</td>
<td align="center">Chowan</td>
<td align="center">North Carolina</td>
<td align="center">37041</td>
<td align="center">7</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-06-03</td>
<td align="center">Audubon</td>
<td align="center">Iowa</td>
<td align="center">19009</td>
<td align="center">13</td>
<td align="center">1</td>
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
<td align="center">2020-04-06</td>
<td align="center">371398</td>
<td align="center">cases</td>
<td align="center">740</td>
<td align="center">86</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-06-06</td>
<td align="center">1638215</td>
<td align="center">deaths</td>
<td align="center">104</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-31</td>
<td align="center">371398</td>
<td align="center">cases</td>
<td align="center">268</td>
<td align="center">19</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-06-10</td>
<td align="center">1638215</td>
<td align="center">deaths</td>
<td align="center">110</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-25</td>
<td align="center">1638215</td>
<td align="center">cases</td>
<td align="center">2895</td>
<td align="center">27</td>
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
  scale_y_continuous(name   = 'Per 100,000 People',
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
ggplot(df, aes(x = date, y = new/population*1e5, color = type)) +
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_continuous(name   = 'Daily Cases Per 100,000 People') +
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
  geom_step(show.legend = FALSE) +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_continuous(name   = 'Weekly Cases Per 100,000 People') +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-weekly-1.png)
