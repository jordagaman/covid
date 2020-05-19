COVID-19 Data from NYT for Counties of Interest
================

Updated: Tue May 19 07:45:19 2020

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

<table style="width:93%;">
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="18%" />
<col width="20%" />
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
<td align="center">2020-04-27</td>
<td align="center">Putnam</td>
<td align="center">Ohio</td>
<td align="center">39137</td>
<td align="center">54</td>
<td align="center">5</td>
</tr>
<tr class="even">
<td align="center">2020-03-25</td>
<td align="center">Linn</td>
<td align="center">Kansas</td>
<td align="center">20107</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-24</td>
<td align="center">Wharton</td>
<td align="center">Texas</td>
<td align="center">48481</td>
<td align="center">36</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-15</td>
<td align="center">Chautauqua</td>
<td align="center">New York</td>
<td align="center">36013</td>
<td align="center">24</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-04-05</td>
<td align="center">Dallas</td>
<td align="center">Iowa</td>
<td align="center">19049</td>
<td align="center">35</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-03-17</td>
<td align="center">Washtenaw</td>
<td align="center">Michigan</td>
<td align="center">26161</td>
<td align="center">7</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-11</td>
<td align="center">Polk</td>
<td align="center">Missouri</td>
<td align="center">29167</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-05</td>
<td align="center">Washburn</td>
<td align="center">Wisconsin</td>
<td align="center">55129</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-08</td>
<td align="center">Accomack</td>
<td align="center">Virginia</td>
<td align="center">51001</td>
<td align="center">11</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-14</td>
<td align="center">Zapata</td>
<td align="center">Texas</td>
<td align="center">48505</td>
<td align="center">3</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-03-26</td>
<td align="center">Unicoi</td>
<td align="center">Tennessee</td>
<td align="center">47171</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-03-28</td>
<td align="center">Wayne</td>
<td align="center">Indiana</td>
<td align="center">18177</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-05-03</td>
<td align="center">Spotsylvania</td>
<td align="center">Virginia</td>
<td align="center">51177</td>
<td align="center">186</td>
<td align="center">4</td>
</tr>
<tr class="even">
<td align="center">2020-03-21</td>
<td align="center">Jefferson</td>
<td align="center">Tennessee</td>
<td align="center">47089</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-11</td>
<td align="center">Davison</td>
<td align="center">South Dakota</td>
<td align="center">46035</td>
<td align="center">3</td>
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
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-06</td>
<td align="center">deaths</td>
<td align="center">19</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-11</td>
<td align="center">cases</td>
<td align="center">1434</td>
<td align="center">152</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-08</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-15</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-01</td>
<td align="center">cases</td>
<td align="center">380</td>
<td align="center">46</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-31</td>
<td align="center">cases</td>
<td align="center">334</td>
<td align="center">30</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-01</td>
<td align="center">cases</td>
<td align="center">333</td>
<td align="center">65</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-16</td>
<td align="center">cases</td>
<td align="center">2037</td>
<td align="center">181</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-26</td>
<td align="center">deaths</td>
<td align="center">52</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-01</td>
<td align="center">deaths</td>
<td align="center">63</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-22</td>
<td align="center">cases</td>
<td align="center">127</td>
<td align="center">40</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-14</td>
<td align="center">cases</td>
<td align="center">2202</td>
<td align="center">3</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-25</td>
<td align="center">cases</td>
<td align="center">3245</td>
<td align="center">159</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-02</td>
<td align="center">cases</td>
<td align="center">386</td>
<td align="center">53</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-26</td>
<td align="center">cases</td>
<td align="center">199</td>
<td align="center">43</td>
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
