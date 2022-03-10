# Proposal

## Motivation and purpose

Our role: Student group

Target audience: Various interest groups including demographers, sociologists, and public health organizations

Life expectancy is one of the most important metrics that gauge the well-being of a population, capturing many aspects of public health with its figure. As it is the metric that unequivocally states the average age of death, many research groups, such as demographers and sociologists, are keen on following its trend to assess the conditions of population health. In order to provide an effective tool for such research efforts, our group is proposing to build a data visualization app that will enable the users to freely explore the dataset on global population with a focus on life expectancy. Our visual dashboard will display important summaries on life expectancy including the global heat map, its correlation with GDP, and narrowed-down metric of the child mortality. The users will also be able to filter their findings according to different variables such as continent, country, and year by using the interactive control tools. 

## Description of the data

We would be using the infamous [Gapminder dataset](https://raw.githubusercontent.com/UofTCoders/workshops-dc-py/master/data/processed/world-data-gapminder.csv) to find insights about the health related indicators namely life expectancy and child mortality rates, along with the factors like income and population size that affect them. The dataset consists of 14 columns and 38982 rows containing various demographic features for each country from the year 1800 to 2018. The data from the earlier years is often missing, so majority of our visualization would be from the more recent years. The main focus of our dashboard would be on depicting life expectancy(in years) and child mortality(deaths of children under 5 per 1,000 live births) over the years across the world. Each country has an associated region and sub-region. The users would be able to filter on the basis of year, sub-regions(Northern America, Western Asia, etc) regions(Asia, America, etc) and view the life expectancies of top 10 countries, life expectancies across all regions in the world and the affect of income and population size(in the region) on life expectancy across the world. Furthermore, the impact of income on child mortality rate for each region in the world would also be showcased


## Research questions you are exploring

Jen is a demographer and sociologist who studies the social determinants of health and mortality. Her research seeks to explain differences in life expectancy and health over the life course across populations.

She is conducting research on the recent trends in life expectancy across high-income countries and wants to be able to [explore] a dataset in order to [compare] the effect of recent adverse trends in life expectancy on the US's overall life expectancy standing with other high-income countries.

This application shows an overview of the life expectancy across high-income countries over time. She can [filter] the data based on the region she is interested in the high-income countries, and the time period she is interested in the last 20 years.

When Jen logs on to the “mindthegap”, she will see an overview of all the available variables in her dataset, according to the desired country, sub-region and single year. She can filter out variables for head-to-head comparisons. When she does so, Jen may notice a recent decline in life expectancy in most high-come countries simultaneously and the fact that the USA is falling further and further behind its peer countries in the last 20 years. She hypothesizes that different facts may be driving the declines in life expectancy in the USA compared with other high-come countries, and decides she needs to conduct a follow-on study since the cause of death coding across countries is not captured in her current dataset.

## App sketch

See the sketch of the application [here](https://github.com/UBC-MDS/mindthegap/blob/main/img/dashboard-sketch.jpg).
