
## A View of the Vietnamese Communist Party’s Internal Tension through the Budget of the Central Party Office

**Authors:** [Thuy Nguyen](https://www.linkedin.com/in/nguyendata/),
Tuong Vu

[Read my full article
here](https://usvietnam.uoregon.edu/en/a-view-of-the-vietnamese-communist-partys-internal-tension-through-the-budget-of-the-central-party-office/)

``` r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(colorblindr)
library(lubridate)

data <- rio::import(here::here("data", "du toan chi co quan TW_complile_2006_2020.xlsx"), skip = 4) 

pd <- data %>% 
  pivot_longer(cols = c("2006":"2020"),
                names_to = "Year",
               values_to = "Amount") %>% 
  filter(`Central Agencies` != "Central finance management board") %>% 
  mutate(Amount_milusd = round(Amount/23000, 1)) #1 usd = 23000 vnd
```

### The Offices of the “Four Pillars”

The Vietnamese Communist Party (VCP) is not only the ruling party in
Vietnam but is also the only political party that exists legally in this
country of nearly 100 million people. As in other communist countries,
the VCP’s domination is written into the country’s constitution. There
is no law that bans the formation of political parties but the
authorities would arrest and imprison anyone who attempts to challenge
the VCP’s monopoly.

``` r
four_office <- pd %>% 
  filter(`Central Agencies` == "Presidential secretariat" |
           `Central Agencies` == "National assembly's secretariat"|
             `Central Agencies` == "Governmental office" |
           `Central Agencies` == "Office of central party committee") %>% 
  drop_na() %>% 
  mutate(Year = make_date(Year)) 


four_office %>% 
  ggplot(aes(Year, Amount_milusd, group = `Central Agencies`, fill = `Central Agencies`)) +
  geom_line(aes(color = `Central Agencies`), size = 1.5) +
   scale_color_OkabeIto(name = "Office of",
                       breaks = c("Office of central party committee", "National assembly's secretariat", # keep the breaks as the variables in the data set
                                   "Governmental office", "Presidential secretariat"),
                      labels = c("Central Party Office", "Office of the National Assembly", # how we want to call it in the plot
                                   "Office of Government", "State President’s Office")) + 
  labs(title = "Planned Expenditures of Four Offices \n2006 - 2017",
         x = "",
         y = "Planned Expenditures (in Million USD)",
         caption = "Note: Data of the Central Party Office for 2009, before 2008 and after 2015 are not available \nSource: Data complied by author from multiple reports by \nVietnam's Ministry of Finance") +
  #scale_x_discrete(breaks = 2006:2017) +
  theme(
  legend.position = c(.4, .6),
  legend.justification = c("right", "bottom"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.title = element_blank()
  ) 
```

<img src="README_files/figure-gfm/pd_four_office-1.png" style="display: block; margin: auto;" />

The Central Party Office (CPO) coordinates Party business under the
direct supervision of the Politburo and the Secretariat. The General
Secretary of the VCP is considered one of the four “pillars” of the
regime in Vietnam. The other three “pillars” include the State
President, the Prime Minister, and the Chair of the National Assembly.
The Secretary is primus inter pares among the four.

Like the Central Party Office (CPO), three other offices play the same
role for three remaining government “pillars,” including 1) the State
President’s Office (SPO) to coordinate the business of the State
President and the Vice President; 2) the Office of Government (OG), a
ministerial-level agency under the Prime Minister; and 3) the Office of
the National Assembly (ONA) to serve Chair of the National Assembly,
Vice-Chairs, the Standing Committee and other Committees, and the Ethnic
Council of the National Assembly.

Since the Prime Minister oversees the most activities of the government,
one would expect the Office of Government to have the largest budget
among the four. But this is not the case. As Vietnam’s national income
has increased at the rate of 5-7% in the last decade, one also expects
the budgets of all four agencies to increase; yet our data shows a
pattern of uneven increase under the leadership of the VCP’s General
Secretary Nguyen Phu Trong since 2011.

### Where Have All the Money Gone?

The planned expenditures of the Central Party Office show an increase of
4 times in 7 years between 2008 and 2015, from nearly USD 27 million
(VND 622 billion) to about USD 105 million USD (more than VND2400
billion).

``` r
pd_party <- pd %>% 
  filter(`Central Agencies` == "Office of central party committee") %>% 
  drop_na() %>% 
ggplot(aes(Year, Amount_milusd)) +
    geom_col(fill = "#0072B2", alpha = .9) +
  geom_text(aes(label= Amount_milusd), color = "white", vjust = 1.5) +
    labs(title = "Planned Expenditures of the Central Party Office \n2008 - 2015",
         x = "",
         y = "Planned Expenditures (in Million USD)",
         caption = "Note: Data for 2009, before 2008 and 2015 are not available \nSource: Data complied by author from multiple reports by \nVietnam's Ministry of Finance") +
  scale_x_discrete(breaks = 2008:2015) +
  theme_minimal()
pd_party
```

<img src="README_files/figure-gfm/pd_party-1.png" style="display: block; margin: auto;" />
