library(tidyverse)

#removing scientific notation for plot in section 4
options(scipen = 999)

#map libraries
library(plotly)
library(mapproj)


#loading data
inc_trends <- read.csv("C:\\Users\\emmao\\Documents\\INFO201\\Assignments\\a4-emmaodwyer\\incarceration-trends\\incarceration_trends.csv")

# The functions might be useful for A4
source("C:\\Users\\emmao\\Documents\\INFO201\\Assignments\\a4-emmaodwyer\\source\\a4-helpers.R")



## Section 2  ---- 
#----------------------------------------------------------------------------#
#Summary Info
summary_info <- list()

#looking at Texas only
tx <- inc_trends %>%
  filter(state == "TX" & year =="2016") 

#county with max male prison pop
summary_info$max_prison_pop_texas_county <- tx %>%
  filter(male_prison_pop == max(male_prison_pop, na.rm = TRUE))%>%
  select(county_name, male_prison_pop, urbanicity)

#county with max black pop
summary_info$max_black_pop_texas_county <- tx %>%
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = TRUE))%>%
  select(county_name, black_pop_15to64, urbanicity)

#mean black population
summary_info$mean_black_pop <- tx %>%
  summarise(mean_black_pop= mean(black_pop_15to64, na.rm=TRUE))

#mean white population
summary_info$mean_white_pop <- tx %>%
  summarise(mean_white_pop= mean(white_pop_15to64, na.rm=TRUE))

#mean black male prison pop
summary_info$mean_black_male_prison_pop <- tx %>%
  summarise(mean_black_male_prison_pop= mean(black_male_prison_pop, na.rm=TRUE))

#mean white male prison pop
summary_info$mean_white_male_prison_pop <- tx %>%
  summarise(mean_white_male_prison_pop= mean(white_male_prison_pop, na.rm=TRUE))






#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function creates the dataframe of year vs total jail population in US
get_year_jail_pop <- function(){
  as.data.frame(inc_trends %>%
    group_by(year) %>%
    summarise(total_jail_pop_year = sum(total_jail_pop, na.rm=TRUE))) 
}
get_year_jail_pop()


# This function creates the barplot seen in figure 1 on canvas
plot_jail_pop_for_us <- function()  {
  plot<- get_year_jail_pop() %>%
    ggplot(aes(x=year, y = total_jail_pop_year)) +
    geom_bar(stat="identity")+
    labs(title = "Increase of Jail population in U.S. (1970-2018)", y = "Total Jail Population", 
         x = "Year", caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018)")
  return(plot(plot))   
}



## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#Two different test groups for the plot
states = c("WA", "OR", "CA", "UT", "AZ")
southern_states <- c("AL", "GA", "FL", "SC", "NC", "TX", "LA", "MS")


get_jail_pop_by_states <- function(x){
  as.data.frame(inc_trends %>%
    filter(state %in% x) %>%
    group_by(year, state) %>%
    summarise(total_jail_pop_year_by_state = sum(total_jail_pop, na.rm=TRUE)))
}
get_jail_pop_by_states(states)
  
#This function creates the plot 
plot_jail_pop_by_states <- function(x){
  plot <- get_jail_pop_by_states(x) %>%
    ggplot(aes(x=year, y = total_jail_pop_year_by_state)) +
    geom_line(aes(colour = state))+
    labs(title = "Increase of Jail population in selected U.S. states (1970-2018)", y = "Total Jail Population", 
         x = "Year", caption = "Figure 2. Increase in Jail Population of Western U.S. States")
  return(plot(plot))   
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
#<Comparing the number of black male and white male prisoners as a percentage 
#of total black populations and white populations in rural vs urban areas in TX>

# Data Wrangling Function
tx_male_prison_by_race <- function(){
  #Defining urban vs rural areas
  urban <- inc_trends %>%
    # filtering to texas
    filter(state == "TX") %>% 
    #filtering to year when race data was recorded
    filter(year == "2016") %>%
    #filtering to cities with pop>100,000
    filter(total_pop > 100000) %>%
    select(county_name)
  #making into list
  urban_counties <- urban$county_name
  #Filtering dataframe to use
  tx <- inc_trends %>%
    #filtering to texas
    filter(state == "TX") %>% 
    #filtering to years when race data was recorded
    filter(year > "2004") %>% 
    #mutating new columns 
    mutate(black_male_prison_rate = black_male_prison_pop/ black_pop_15to64, 
           white_male_prison_rate = white_male_prison_pop/ white_pop_15to64) %>%
    #selecting relevant columns only
    select(year, state, fips, county_name, total_pop, total_pop_15to64,
           white_male_prison_pop, white_pop_15to64, white_male_prison_rate, 
           black_male_prison_pop, black_pop_15to64, black_male_prison_rate) %>%
    #adding column saying if county is rural or urban
    mutate(Location = ifelse(county_name %in% urban_counties, "Urban", "Rural")) %>%
    #grouping by urabn/rural and year to find means
    group_by(Location, year) %>%
    summarise(urban_white_male_prison_rate = mean(white_male_prison_rate, na.rm=TRUE), 
              urban_black_male_prison_rate = mean(black_male_prison_rate, na.rm=TRUE))
  tx
}    

tx_male_prison_by_race()



#Plotting function 
tx_male_race_plot <- function(X){
  plot <- 
    ggplot(data = X) +
    geom_line(mapping = aes(x = year, y = urban_black_male_prison_rate, colour = Location)) + 
    geom_line(mapping = aes(x = year, y = urban_white_male_prison_rate, colour = Location)) + 
    labs(title = "Rate of Male Imprisonment by Race in Rural and Urban Texas", y = "Rate of Population in Prison", 
         x = "Year", subtitle = "Urban is defined as counties which had a \npopoulation greater than 100,000 in 2017")
  ggplotly(plot) %>%
    layout(margin = list(b=160),
           annotations = 
             list(x =1.5, y = -0.75, text = "Figure 3. This chart measures the number of black and white males, respectively, as a percentage of the black population aged between 15-64 and the white population \naged 15-64 in rural and urban Texas.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10, color="black")))
  
}

tx_male_race_plot(tx_male_prison_by_race())


# See Canvas
#----------------------------------------------------------------------------#



## Section 6  ---- 
#----------------------------------------------------------------------------#
# <Map showing ratio of rate of black men in prison vs rate of white men in 
#prison in Texas by county in 2016>
#DATA WRANGLING FUNCTION
tx_male_race_map_data <- function(X){
  county_shape <- map_data("county") %>%
    #dplyr filter to counties in texas only
    filter(region == "texas") %>%
    mutate(county = as.character(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                                      subregion,
                                      perl = TRUE)))
  
  #Filtering dataframe to use
  tx_map_data <- inc_trends %>%
    #filtering to texas
    filter(state == "TX") %>% 
    #filtering to years when race data was recorded
    filter(year =="2016") %>% 
    #mutating new columns 
    mutate(black_male_prison_rate = black_male_prison_pop/ black_pop_15to64, 
           white_male_prison_rate = white_male_prison_pop/ white_pop_15to64) %>%
    #selecting relevant columns only
    select(year, state, fips, county_name, total_pop, total_pop_15to64,
           white_male_prison_pop, white_pop_15to64, white_male_prison_rate, 
           black_male_prison_pop, black_pop_15to64, black_male_prison_rate) %>%
    mutate(black_white_ratio = black_male_prison_rate / white_male_prison_rate) %>%
    mutate(county = as.character(gsub("\\s*\\w*$", "", county_name))) 
  
  tx_map_data %>%
    left_join(county_shape, by="county") # join data
}

tx_male_race_map_data(tx_male_prison_by_race())


#PLOTTING FUNCTION
plot_tx_male_race_map_data <- function(X){
  map_plot <- ggplot(X) +
    geom_polygon( 
      mapping = aes(x = long, y= lat, group = group, fill = black_white_ratio),
      color = "black",
      size  = .1, ) +
    coord_map() + 
    labs(title = "Chorpleth Map of Texas showing the Ratio of Rate of Black Men \nto Rate of White Men in Prison in County", x= "", y = "")+
    scale_fill_distiller(palette = "RdYlBu", direction = -1, name="Ratio ")
  
ggplotly(map_plot) %>%
    layout(margin = list(b=160),
           annotations = 
             list(x =0.5, y = -0.75, text = "Figure 4. Choropleth map of Texas showing racial inequalities in prison rates.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10, color="black")))
}


plot_tx_male_race_map_data(tx_male_race_map_data((tx_male_prison_by_race())))

# See Canvas
#----------------------------------------------------------------------------#

