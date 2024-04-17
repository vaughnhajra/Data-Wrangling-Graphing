# The Tennis "Big 3": Analyzing the Careers of Nadal, Djokovic, and Federer

## By: Harshal Rukhaiyar and Vaughn Hajra

## What Makes a GOAT?
- Winning Matches
- Being Ranked World #1
- Winning Grand Slams

### Our Data: via Tennis Abstract
- Different Page for Each Player
- Many Different Data Sets for Each Page

## Getting the Data Into R
- Used the `googlesheets4` package
### Data of Interest
- Career Win/Loss by Year Statistics (one table for each player)
- Career Year End Rankings (one table for each player)
- Age of each player (birth date found on players’ homepage)

## Graph type of interest: Line/dot plot
### Graph One: Winning Percentage
- Imported new dataset for each player “Rankings”
- Created Year_Mod variable and used `inner_join()`
- Then used `rbind()` to make one large dataset
- Created “Years #1” Table by filtering and using `nrow()` for each player, then creating a data frame and sorting descending

## Data of Interest: Career Notable Finals (One for Each Player)
- Graph type of interest: Stacked Bar Graph
### Graph Two: Grand Slams
- Data Manipulation Explained!
- Getting Number of Titles

## Recap of Topics Covered
- Data Visualization (theme_light, shapes, etc)
- Grammar (ggplot, aes, bar graph, etc)
- Data Wrangling (mutate, filter, arrange, etc)
- Data Wrangling with multiple tables (inner_join, rbind)
- Tidy data (googlesheets4, website, etc)
- Iteration (for loop)

### Other Considerations
- Relationship between Aces served and Win%
- Relationship between First set won and Win%
