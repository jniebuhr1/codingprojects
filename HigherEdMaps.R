### census data R practice

install.packages('tidycensus')
install.packages('readxl')
install.packages('usmap')
install.packages('usa')
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(readxl)
library(maps)
library(usmap)
library(usa)
library(dplyr)



#load State Government Employment & Payroll Data
stategov <- read_excel('2022_state.xlsx')

#avoid scientific notation
options(scipen = 999)

#change column names to give appropriate titles
colnames(stategov) <- c('state', 'government_function', 'full_time_employees', 'full_time_payroll',
                        'part_time_employees', 'part_time_payroll', 'equivalent_full_time', 
                        'total_employees', 'total_payroll')
#remove first 14 useless rows from dataset
stategov2 <- stategov[-c(1:14),]

#create dataset with only national totals
fedgovtotals <- stategov2[c(1:38),]

#create dataset with only state data
sdata <- stategov2[c(39:nrow(stategov2)),]

#create dataset with only total higher ed employee payroll for each state
highereddat <- sdata %>% 
  filter(government_function == 'Education - Higher Education Total')

#change total_payroll column to numeric class for adjusting y ticks on plot
highereddat$total_payroll <- as.numeric(highereddat$total_payroll)


#plot total dollars spent on higher ed payrolls by state
plot1 <- ggplot(highereddat, aes(x = state, y = total_payroll)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Higher Education Monthly Employee Payroll',
       subtitle = 'Source: 2022 US Goverment Data',
       x = 'State',
       y = 'Cost (USD)') +
  scale_y_continuous(breaks = seq(0, max(highereddat$total_payroll), by = 250000000))

plot1

class(highereddat$total_payroll)

##US map
map <- us_map(regions = 'states')

ggplot(map, aes(x = x, y = y)) +
  geom_point()

ggplot(map, aes(x = x, y = y, group = group)) +
  geom_polygon()

#change name of map state column to match highereddat
map$state <- map$abbr

#get rid of abbr column as its redundant
map$abbr <- NULL

mergeddata <- full_join(highereddat, map, by = 'state')

class(map$state)
class(highereddat$state)

#create chloropleth map of US, shaded by state for higher ed payroll
plot2 <- ggplot(mergeddata, aes(x = x, y = y, group = group, fill = total_payroll)) +
  geom_polygon() +
  labs(title = 'Higher Education Total Monthly Employee Payroll',
       subtitle = 'Source: 2022 US Goverment Census Data',
       x = NULL,
       y = NULL,
       fill = 'Cost (USD)') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

plot2

state_census

#import state population data
statep <- usa::facts

highereddat$full_time_employees <- as.numeric(highereddat$full_time_employees)
highereddat$full_time_payroll <- as.numeric(highereddat$full_time_payroll)


highereddat <- highereddat %>% 
  mutate(ave_full_salary = full_time_payroll/full_time_employees)

#create chloropleth map of US, shaded by state for higher ed ave payroll per full-time employee
plot3 <- ggplot(mergeddata, aes(x = x, y = y, group = group, fill = ave_full_salary)) +
  geom_polygon() +
  labs(title = 'Higher Education Average Monthly Full-Time Salary',
       subtitle = 'Source: 2022 US Goverment Census Data',
       x = NULL,
       y = NULL,
       fill = 'Cost (USD)') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

plot3

#get rid of DC and Puerto Rico from state pop data
statep <- statep[-c(9, 40),]

#add state 2 letter abbreviations to statep dataset
statep <- statep %>% 
  mutate(state = c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT',
                    'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN',
                    'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA',
                    'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV',
                    'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH',
                    'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN',
                    'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI',
                    'WY'))

#merge statep data with mergeddata dataset
mergeddata <- full_join(mergeddata, statep, by = 'state')

#create column of higher ed salary per capita for each state by dividing total payroll by population
mergeddata <- mergeddata %>% 
  mutate(cost_per_capita = total_payroll/population)

#create chloropleth map based on this new variable
plot4 <- ggplot(mergeddata, aes(x = x, y = y, group = group, fill = cost_per_capita)) +
  geom_polygon() +
  labs(title = 'Monthly Higher Education Payroll per Capita',
       subtitle = 'Source: 2022 US Goverment Census Data',
       x = NULL,
       y = NULL,
       fill = 'Cost (USD)') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

plot4
