COVID-19 Data from NYT for Counties of Interest
================

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

<table style="width:90%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="18%" />
<col width="20%" />
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
<td align="center">2020-04-30</td>
<td align="center">Duval</td>
<td align="center">Texas</td>
<td align="center">48131</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-29</td>
<td align="center">Johnson</td>
<td align="center">Kansas</td>
<td align="center">20091</td>
<td align="center">454</td>
<td align="center">40</td>
</tr>
<tr class="odd">
<td align="center">2020-04-15</td>
<td align="center">Aroostook</td>
<td align="center">Maine</td>
<td align="center">23003</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-17</td>
<td align="center">Tunica</td>
<td align="center">Mississippi</td>
<td align="center">28143</td>
<td align="center">28</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-05-11</td>
<td align="center">Johnson</td>
<td align="center">Missouri</td>
<td align="center">29101</td>
<td align="center">63</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-22</td>
<td align="center">Leon</td>
<td align="center">Texas</td>
<td align="center">48289</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-30</td>
<td align="center">Lincoln</td>
<td align="center">Washington</td>
<td align="center">53043</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-19</td>
<td align="center">Atkinson</td>
<td align="center">Georgia</td>
<td align="center">13003</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-11</td>
<td align="center">Woodbury</td>
<td align="center">Iowa</td>
<td align="center">19193</td>
<td align="center">18</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-15</td>
<td align="center">Washington</td>
<td align="center">Oregon</td>
<td align="center">41067</td>
<td align="center">620</td>
<td align="center">15</td>
</tr>
<tr class="odd">
<td align="center">2020-04-18</td>
<td align="center">Guadalupe</td>
<td align="center">Texas</td>
<td align="center">48187</td>
<td align="center">53</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-01</td>
<td align="center">Broward</td>
<td align="center">Florida</td>
<td align="center">12011</td>
<td align="center">5144</td>
<td align="center">199</td>
</tr>
<tr class="odd">
<td align="center">2020-05-07</td>
<td align="center">Union</td>
<td align="center">Florida</td>
<td align="center">12125</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-06</td>
<td align="center">Pierce</td>
<td align="center">North Dakota</td>
<td align="center">38069</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-01</td>
<td align="center">Putnam</td>
<td align="center">Indiana</td>
<td align="center">18133</td>
<td align="center">11</td>
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
<td align="center">Alameda, California</td>
<td align="center">2020-04-06</td>
<td align="center">cases</td>
<td align="center">609</td>
<td align="center">22</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-30</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-12</td>
<td align="center">cases</td>
<td align="center">2154</td>
<td align="center">32</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-05</td>
<td align="center">cases</td>
<td align="center">1830</td>
<td align="center">33</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-21</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-08</td>
<td align="center">cases</td>
<td align="center">992</td>
<td align="center">155</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-26</td>
<td align="center">deaths</td>
<td align="center">4</td>
<td align="center">2</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-14</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-24</td>
<td align="center">deaths</td>
<td align="center">48</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-03</td>
<td align="center">cases</td>
<td align="center">464</td>
<td align="center">47</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-17</td>
<td align="center">deaths</td>
<td align="center">40</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-08</td>
<td align="center">cases</td>
<td align="center">695</td>
<td align="center">40</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-05</td>
<td align="center">deaths</td>
<td align="center">280</td>
<td align="center">24</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-01</td>
<td align="center">cases</td>
<td align="center">333</td>
<td align="center">65</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-25</td>
<td align="center">deaths</td>
<td align="center">183</td>
<td align="center">5</td>
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
