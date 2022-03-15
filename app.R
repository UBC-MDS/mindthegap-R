library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- read_csv("data/gapminder.csv") %>% drop_na()
ci <- read_csv('data/country_iso.csv')
df <- merge(df, ci, by = 'country')

year_range <- seq(min(df$year), max(df$year),5)
year_range <- setNames(as.list(as.character(year_range)), as.integer(year_range))


labels <- list(
  "life_expectancy" = "Life Expectancy",
  "pop_density" = "Population Density",
  "child_mortality" = "Child Mortality"
)

metrics <- list(
  list("label" = "Life Expectancy", "value" = "life_expectancy"),
  list("label" = "Child Mortality", "value" = "child_mortality"),
  list("label" = "Population Density", "value" = "pop_density")
)



app$layout(
  dbcContainer(
    list(

      dccSlider(
        id="yr",
        min=min(df$year),
        max=max(df$year),
        step=1,
        value=max(df$year),
        marks=year_range,
        tooltip=list(
          always_visible=TRUE,
          placement="top"
        )
      ),

      htmlBr(),
      htmlP("Statistical metric"),
      dccDropdown(
        id='metric',
        options = metrics,
        value='life_expectancy'),
      htmlBr(),
      dccGraph(id='worldmap')
    )
  )
)

app$callback(
  output('worldmap', 'figure'),
  list(input('yr', 'value'),
       input('metric', 'value')),
  function(yr, metric) {
    df = df %>% filter(year==yr)

    map <- plot_ly(df, type='choropleth', locations=~iso_code, text=~country, z=~df[,metric], colorscale = "Viridis", color=metric) %>%
      layout(
        title = paste(labels[[metric]], "by country for year", yr)
      )
    map
  }
)


#' Filter data based on region, sub region and year selection
#'
#' @param region A character vector to be used to select from the Region filter
#' @param sub_region A character vector to be used to select from the Sub Region filter
#' @param yr A numeric vector to be used to select from the Year filter
#' @return Returns a data frame that has been filtered on region, sub region and country selection
#' @examples
#' filter_data("Asia", "Western Asia", 2014)
filter_data <- function(region = NULL,
                        sub_region = NULL,
                        year = NULL) {
  if (!is.null(sub_region)) {
    filtered_df <- df |>
      filter(sub_region == {{ sub_region }})
  } else if (!is.null(region)) {
    filtered_df <- df |>
      filter(region == {{ region }})
  } else {
    filtered_df <- df
  }
  
  if (!is.null(year)) {
    filtered_df <- filtered_df |> 
      filter(year == {{ year }})
  }
  
  filtered_df
}

app$run_server(host = '0.0.0.0')
