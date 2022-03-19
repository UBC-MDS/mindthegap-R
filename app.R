library(dash)
library(dashHtmlComponents)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Read in data
df <- read_csv("data/gapminder.csv") %>%
  mutate(log_income = log(income)) %>%
  drop_na()

ci <- read_csv("data/country_iso.csv")
df <- merge(df, ci, by = "country")

year_range <- seq(min(df$year), max(df$year), 5)
year_range <- setNames(as.list(as.character(year_range)), as.integer(year_range))



############################## CONTROL PANEL FILTERS ##############################
FILTER_STYLE <- list("background-color" = "#f8f9fa", "width" = "15rem", "height" = "100%")

filter_panel <- dbcCard(
 dbcCol(list(
    # control panel title
    htmlH2("Control Panel", className = "text-center"),
    htmlBr(),
    # metric radio button
    dbcRow(list(
      htmlH5("1. Metric", className = "text-left"),
      dbcRadioItems(
        id = "metric",
        options = list(
          list("label" = "Life Expectancy", "value" = "life_expectancy"),
          list("label" = "Child Mortality", "value" = "child_mortality"),
          list("label" = "Population Density", "value" = "pop_density")
        ),
        value = "life_expectancy",
        labelStyle = list("display" = "block")
      )
    )),
    htmlBr(),
    # continent drop down
    dbcRow(list(
      htmlH5("2. Continent", className = "text-left"),
      dccDropdown(
        id = "region",
        options = list(
          list(label = "Asia", value = "Asia"),
          list(label = "Europe", value = "Europe"),
          list(label = "Africa", value = "Africa"),
          list(label = "Americas", value = "Americas"),
          list(label = "Oceania", value = "Oceania")
        ),
        value = NULL
      )
    )),
    htmlBr(),
    # sub-region drop down
    dbcRow(list(
      htmlH5("3. Sub Continent", className = "text-left"),
      dccDropdown(
        id = "sub_region",
        # options = purrr::map(unique(df$sub_region), function(c) list(label = c, value = c)),
        value = NULL
      )
    )),

    htmlBr(),
    # empty plot message
    htmlSmall(
      "Note: If a plot is empty, this means that there is no data based on your selections."
    )


  ), md=12),
  style = FILTER_STYLE,
  body = TRUE
)



############################## ORIGINAL METRICS ################################
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



############################## DASHBOARD LAYOUT ###################################
app$layout(
  dbcContainer(
    list(
      # title
      htmlDiv(
        style = list("textAlign" = "center", "color" = "Gray", "font-size" = "26px"),
        children = list(
          htmlH1("Mindthegap Dashboard")
        ),
      ),
      htmlBr(),
      dbcRow(list(
        dbcCol(filter_panel, md=3),

        dbcCol(list(
                htmlH3("Select Year"),
                dccSlider(
                  id = "yr",
                  min = min(df$year),
                  max = max(df$year),
                  step = 1,
                  value = max(df$year),
                  marks = year_range,
                  tooltip = list(
                    always_visible = TRUE,
                    placement = "top"
                  )
                ),

               dbcRow(dccGraph(id = "worldmap")),
               dbcRow(
                 list(
                   dbcCol(
                     dbcCard(
                       dccGraph(
                         id = "plot-bar")
                       ), md=6
                     ),
                   dbcCol(
                    list(
                     htmlDiv(

                       dbcTabs(

                           children=list(
                             dbcTab(

                               label = "GDP",
                               tab_id = "gdp"
                             ),
                             dbcTab(
                               label="Income",
                               tab_id="income"
                             )
                             ),
                           id = "tabs",
                           active_tab = "gdp",
                           )
                           ),

                     htmlDiv(id = "tab-content")
                    )
                     )
                   )
              )
       ), md=9)
)
      )
    )
  )
)



app$callback(
  output("worldmap", "figure"),
  list(
    input("yr", "value"),
    input("metric", "value")
  ),
  function(yr, metric) {
    df <- df %>% filter(year == yr)

    map <- plot_ly(
      df,
      type = "choropleth",
      locations = ~iso_code,
      text = ~country,
      z = ~ df[, metric],
      colorscale = "Viridis",
      color = metric) %>%
      layout(
        title = paste(labels[[metric]], "by country for year", yr)
      )
    map
  }
)



app$callback(
  output("tab-content", "children"),
  list(input("tabs", "active_tab")),
  function(tabs){
    if(tabs == "gdp"){

      return(htmlDiv(dccGraph(id = "bubblechart")))}
    else if(tabs == "income"){
      return(htmlDiv(dccGraph(id = "box-plot")))}

  }
)


# Box Plot
app$callback(
  output("box-plot", "figure"),
  list(
    input("region", "value"),
    input("sub_region", "value"),
    input("yr", "value"),
    input("metric", "value")
  ),
  function(region, sub_region, yr, metric) {
    filtered_df <- filter_data(region, sub_region, yr)

    p <- ggplot(filtered_df, aes(
      x = income_group,
      y = !!sym(metric),
      color = income_group
    )) +
      geom_boxplot() +
      labs(
        title = paste0(labels[[metric]], " group by Income Group for year ", yr),
        x = "Income Group",
        y = labels[[metric]],
        colour = "Income Group"
      ) +
      ggthemes::scale_color_tableau()

    ggplotly(p)
  }
)

# Bubble Chart
app$callback(
  output("bubblechart", "figure"),
  list(
    input("region", "value"),
    input("sub_region", "value"),
    input("yr", "value"),
    input("metric", "value")
  ),
  function(region, sub_region, yr, metric) {
    filtered_df <- filter_data(region, sub_region, yr)


    if (!is.null(region) && !is.null(sub_region)){
      p <- plot_ly(filtered_df, x = ~log_income, y = ~ filtered_df[, metric], color = ~country,
                   type = 'scatter', mode = 'markers', size = ~ pop_density,
                   marker = list(symbol = 'circle', sizemode = 'diameter',
                                 line = list(width = 2, color = '#FFFFFF')),
                   text = ~paste('Country:', country, '<br>', labels[[metric]], ':', metric, '<br>GDP:', log_income,
                                 '<br>Pop density:', pop_density))


    }
    else if(!is.null(region) && is.null(sub_region)){
      p <- plot_ly(filtered_df, x = ~log_income, y = ~ filtered_df[, metric], color = ~sub_region,
                   type = 'scatter', mode = 'markers', size = ~ pop_density,
                   marker = list(symbol = 'circle', sizemode = 'diameter',
                                 line = list(width = 2, color = '#FFFFFF')),
                   text = ~paste('Country:', country, '<br>', labels[[metric]], ':', metric, '<br>GDP:', log_income,
                                 '<br>Pop density:', pop_density))



    }
    else{
      p <- plot_ly(filtered_df, x = ~log_income, y = ~ filtered_df[, metric], color = ~region,
                             type = 'scatter', mode = 'markers', size = ~ pop_density,
                             marker = list(symbol = 'circle', sizemode = 'diameter',
                                           line = list(width = 2, color = '#FFFFFF')),
                             text = ~paste('Country:', country, '<br>', labels[[metric]], ':', metric, '<br>GDP:', log_income,
                                           '<br>Pop density:', pop_density))
    }
    p <- p %>% layout(
            title = paste0(labels[[metric]], " GDP per Capita ($USD) by region"),
                          xaxis = list(title = 'GDP per capita (2000 dollars)'),

                          yaxis = list(title = paste0(labels[[metric]])))

    ggplotly(p)
  }
)




# Bar Chart
app$callback(
  output("plot-bar", "figure"),
  list(
    input("region", "value"),
    input("sub_region", "value"),
    input("yr", "value"),
    input("metric", "value")
  ),
  function(region, sub_region, yr, metric) {
    filtered_df <- filter_data(region, sub_region, yr) %>%
      arrange(desc(!!sym(metric))) %>%
      slice(1:10)

    p <- ggplot(
      filtered_df,
      aes(
        x = !!sym(metric),
        y = reorder(country, !!sym(metric)),
        fill = country
      )
    ) +
      geom_bar(stat = "identity") +
      labs(
        title = paste0(labels[[metric]], " - Top 10 Country for Year ", yr),
        y = "Country",
        x = labels[[metric]],
        fill = "Country"
      ) +
      scale_fill_brewer(palette = "Set3")

    ggplotly(p)
  }
)


app$callback(
  output("sub_region", "options"),
  list(input("region", "value")),
  function(region){
    options=c()
    if(is.null(region)){
      all_sr <- df$sub_region |> drop_na() |> unique()
      print(all_sr)

      for(sub_region in all_sr){
        options <- append(options, c(label = sub_region, value = sub_region))
      }
    }

    else{
      sub_regions <- df |> filter(region == {{region}}) |> select(sub_region) |> drop_na() |> unique()
      for (sr in sub_regions){
        options <- append(options, c(label= sr, value = sr))
      }



    }


  print(options)
  options

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
  region <- unlist(region)
  sub_region <- unlist(sub_region)

  if (!is.null(sub_region)) {
    filtered_df <- df %>%
      filter(sub_region == {{ sub_region }})
  } else if (!is.null(region)) {
    filtered_df <- df %>%
      filter(region == {{ region }})
  } else {
    filtered_df <- df
  }

  if (!is.null(year)) {
    filtered_df <- filtered_df %>%
      filter(year == {{ year }})
  }

  filtered_df
}






app$run_server(host = "0.0.0.0")
