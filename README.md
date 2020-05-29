COVID-19 Data from NYT for Counties of Interest
================

Updated: Thu May 28 19:48:10 2020

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

<table>
<caption>NYT COVID Tracking Data (continued below)</caption>
<colgroup>
<col width="16%" />
<col width="40%" />
<col width="22%" />
<col width="10%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">date</th>
<th align="center">county</th>
<th align="center">state</th>
<th align="center">fips</th>
<th align="center">cases</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2020-04-09</td>
<td align="center">Perry</td>
<td align="center">Alabama</td>
<td align="center">01105</td>
<td align="center">2</td>
</tr>
<tr class="even">
<td align="center">2020-05-03</td>
<td align="center">Harnett</td>
<td align="center">North Carolina</td>
<td align="center">37085</td>
<td align="center">174</td>
</tr>
<tr class="odd">
<td align="center">2020-04-22</td>
<td align="center">Meigs</td>
<td align="center">Ohio</td>
<td align="center">39105</td>
<td align="center">2</td>
</tr>
<tr class="even">
<td align="center">2020-05-07</td>
<td align="center">Stephenson</td>
<td align="center">Illinois</td>
<td align="center">17177</td>
<td align="center">78</td>
</tr>
<tr class="odd">
<td align="center">2020-05-03</td>
<td align="center">Wayne</td>
<td align="center">Pennsylvania</td>
<td align="center">42127</td>
<td align="center">109</td>
</tr>
<tr class="even">
<td align="center">2020-05-19</td>
<td align="center">Sitka City and Borough</td>
<td align="center">Alaska</td>
<td align="center">02220</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-04-28</td>
<td align="center">Horry</td>
<td align="center">South Carolina</td>
<td align="center">45051</td>
<td align="center">211</td>
</tr>
<tr class="even">
<td align="center">2020-05-22</td>
<td align="center">Livingston</td>
<td align="center">Missouri</td>
<td align="center">29117</td>
<td align="center">3</td>
</tr>
<tr class="odd">
<td align="center">2020-04-05</td>
<td align="center">Kerr</td>
<td align="center">Texas</td>
<td align="center">48265</td>
<td align="center">2</td>
</tr>
<tr class="even">
<td align="center">2020-05-01</td>
<td align="center">Harper</td>
<td align="center">Oklahoma</td>
<td align="center">40059</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-03-29</td>
<td align="center">Randolph</td>
<td align="center">Missouri</td>
<td align="center">29175</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-05-12</td>
<td align="center">Columbia</td>
<td align="center">Wisconsin</td>
<td align="center">55021</td>
<td align="center">34</td>
</tr>
<tr class="odd">
<td align="center">2020-05-03</td>
<td align="center">Shasta</td>
<td align="center">California</td>
<td align="center">06089</td>
<td align="center">31</td>
</tr>
<tr class="even">
<td align="center">2020-04-14</td>
<td align="center">Nevada</td>
<td align="center">California</td>
<td align="center">06057</td>
<td align="center">34</td>
</tr>
<tr class="odd">
<td align="center">2020-04-18</td>
<td align="center">Prince of Wales-Hyder Census Area</td>
<td align="center">Alaska</td>
<td align="center">02198</td>
<td align="center">2</td>
</tr>
</tbody>
</table>

<table style="width:11%;">
<colgroup>
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">8</td>
</tr>
<tr class="odd">
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">5</td>
</tr>
<tr class="even">
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">15</td>
</tr>
<tr class="even">
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">4</td>
</tr>
<tr class="even">
<td align="center">1</td>
</tr>
<tr class="odd">
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
<td align="center">2020-04-07</td>
<td align="center">deaths</td>
<td align="center">24</td>
<td align="center">5</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-05-09</td>
<td align="center">deaths</td>
<td align="center">72</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-18</td>
<td align="center">cases</td>
<td align="center">2478</td>
<td align="center">65</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-25</td>
<td align="center">cases</td>
<td align="center">6491</td>
<td align="center">78</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-16</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-20</td>
<td align="center">cases</td>
<td align="center">22</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-02</td>
<td align="center">deaths</td>
<td align="center">4</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-26</td>
<td align="center">cases</td>
<td align="center">1489</td>
<td align="center">31</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-27</td>
<td align="center">cases</td>
<td align="center">6592</td>
<td align="center">48</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-09</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-18</td>
<td align="center">deaths</td>
<td align="center">110</td>
<td align="center">9</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-04-21</td>
<td align="center">cases</td>
<td align="center">2753</td>
<td align="center">162</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-19</td>
<td align="center">deaths</td>
<td align="center">88</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-14</td>
<td align="center">cases</td>
<td align="center">29</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-19</td>
<td align="center">cases</td>
<td align="center">57</td>
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
