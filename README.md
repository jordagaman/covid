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

<table>
<caption>NYT COVID Tracking Data</caption>
<colgroup>
<col width="17%" />
<col width="28%" />
<col width="22%" />
<col width="10%" />
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
<th align="center">deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2020-04-15</td>
<td align="center">Pender</td>
<td align="center">North Carolina</td>
<td align="center">37141</td>
<td align="center">5</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-12</td>
<td align="center">Wilkes</td>
<td align="center">North Carolina</td>
<td align="center">37193</td>
<td align="center">243</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">2020-04-02</td>
<td align="center">Neshoba</td>
<td align="center">Mississippi</td>
<td align="center">28099</td>
<td align="center">4</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-05</td>
<td align="center">Madison</td>
<td align="center">New York</td>
<td align="center">36053</td>
<td align="center">89</td>
<td align="center">2</td>
</tr>
<tr class="odd">
<td align="center">2020-05-06</td>
<td align="center">Sullivan</td>
<td align="center">New Hampshire</td>
<td align="center">33019</td>
<td align="center">13</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-05-16</td>
<td align="center">Bartholomew</td>
<td align="center">Indiana</td>
<td align="center">18005</td>
<td align="center">412</td>
<td align="center">33</td>
</tr>
<tr class="odd">
<td align="center">2020-05-12</td>
<td align="center">Hancock</td>
<td align="center">West Virginia</td>
<td align="center">54029</td>
<td align="center">12</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-04-03</td>
<td align="center">Faulk</td>
<td align="center">South Dakota</td>
<td align="center">46049</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-16</td>
<td align="center">Jewell</td>
<td align="center">Kansas</td>
<td align="center">20089</td>
<td align="center">3</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-13</td>
<td align="center">Bethel Census Area</td>
<td align="center">Alaska</td>
<td align="center">02050</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-04-29</td>
<td align="center">DeKalb</td>
<td align="center">Illinois</td>
<td align="center">17037</td>
<td align="center">90</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">2020-04-24</td>
<td align="center">Windham</td>
<td align="center">Connecticut</td>
<td align="center">09015</td>
<td align="center">139</td>
<td align="center">4</td>
</tr>
<tr class="odd">
<td align="center">2020-04-02</td>
<td align="center">Union</td>
<td align="center">Florida</td>
<td align="center">12125</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">2020-05-05</td>
<td align="center">Greeley</td>
<td align="center">Nebraska</td>
<td align="center">31077</td>
<td align="center">2</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">2020-05-11</td>
<td align="center">Elbert</td>
<td align="center">Colorado</td>
<td align="center">08039</td>
<td align="center">39</td>
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
<td align="center">2020-03-25</td>
<td align="center">cases</td>
<td align="center">82</td>
<td align="center">24</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-02</td>
<td align="center">cases</td>
<td align="center">417</td>
<td align="center">37</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-08</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-03-18</td>
<td align="center">cases</td>
<td align="center">53</td>
<td align="center">4</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-15</td>
<td align="center">cases</td>
<td align="center">37</td>
<td align="center">8</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-05-06</td>
<td align="center">cases</td>
<td align="center">4860</td>
<td align="center">117</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-19</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-24</td>
<td align="center">cases</td>
<td align="center">1422</td>
<td align="center">72</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-04-03</td>
<td align="center">deaths</td>
<td align="center">12</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-01</td>
<td align="center">deaths</td>
<td align="center">8</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-06</td>
<td align="center">deaths</td>
<td align="center">69</td>
<td align="center">3</td>
</tr>
<tr class="even">
<td align="center">Mercer, New Jersey</td>
<td align="center">2020-03-25</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-05-13</td>
<td align="center">deaths</td>
<td align="center">80</td>
<td align="center">5</td>
</tr>
<tr class="even">
<td align="center">Alameda, California</td>
<td align="center">2020-04-19</td>
<td align="center">cases</td>
<td align="center">1185</td>
<td align="center">50</td>
</tr>
<tr class="odd">
<td align="center">Alameda, California</td>
<td align="center">2020-03-03</td>
<td align="center">deaths</td>
<td align="center">0</td>
<td align="center">0</td>
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
  geom_step() +
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
  geom_step() +
  scale_x_date(name        = '',
               date_breaks = '1 week',
               labels      = scales::date_format('%m/%d')) +
  scale_y_continuous(name   = 'Weekly Increase') +
  facet_grid(type~county, scales = 'free_y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
```

![](README_files/figure-markdown_github/plot-weekly-1.png)
