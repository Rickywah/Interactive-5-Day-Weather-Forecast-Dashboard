library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)

kelvin_to_temperature = function(temperature_k, scale) {
  if (scale == "Celsius") {
    return(temperature_k - 273.15)
  } else {
    return((temperature_k - 273.15) * 9/5 + 32)
  }
}

ui = fluidPage(
  titlePanel("5-Day Weather Forecast Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "city_input", label = "Enter City", value = "Chicago"),
      radioButtons("temperature_scale", "Temperature Scale",
                   choices = c("Celsius", "Fahrenheit"), selected = "Fahrenheit"),
      actionButton("get_weather_btn", "Get 5-Day Forecast")
    ),
    mainPanel(
      plotOutput("temperature_plot"),
      tableOutput("weather_summary")
    )
  )
)

server = function(input, output) {
  city_name = reactiveVal("Chicago")
  temperature_scale = reactiveVal("Celsius")

  observeEvent(input$get_weather_btn, {
    city_name(input$city_input)
    temperature_scale(input$temperature_scale)
  })

  getWeatherData = eventReactive(input$get_weather_btn, {
    api_key = "789945b46d1c6413fb962d8fe365fa72"
    city = input$city_input

    if (is.null(city) || city == "") {
      return(NULL)
    }

    api_url = "https://api.openweathermap.org/data/2.5/forecast"
    response = GET(api_url, query = list(q = city, appid = api_key))
    rr = content(response, as = "text", encoding = "UTF-8")
    data = jsonlite::fromJSON(rr, flatten = TRUE)

    return(data)
  })


  output$temperature_plot = renderPlot({
    data = getWeatherData()
    scale = temperature_scale()

    if (!is.null(data)) {
      plot_data = data$list |>
        mutate(datte = as.Date(dt_txt)) |>
        group_by(as.Date(datte)) |>
        summarise(
          Forecast_Temperature = mean(kelvin_to_temperature(main.temp,scale)),
          Forecast_Feels_Like = mean(kelvin_to_temperature(main.feels_like,scale)),
          Forecast_Min_Temperature = min(kelvin_to_temperature(main.temp_min,scale)),
          Forecast_Max_Temperature = max(kelvin_to_temperature(main.temp_max,scale))
        ) |>
        rename(datee = `as.Date(datte)`)

      plot_data_long = plot_data |>
        pivot_longer(cols = starts_with("Forecast_"), names_to = "Temperature_Type", values_to = "Temperature(°C)")

      ggplot(plot_data_long, aes(x = factor(datee), y = `Temperature(°C)`, fill = Temperature_Type)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        labs(title = paste("5-Day Weather Forecast in", city_name()),
             x = "Day", y =  paste("Temperature (", ifelse(scale == "Celsius", "°C", "°F"), ")")) +
        scale_fill_manual(values = c("Forecast_Temperature" = "deeppink", "Forecast_Feels_Like" = "darkorange", "Forecast_Min_Temperature" = "cyan3", "Forecast_Max_Temperature" = "red")) +
        geom_text(aes(label = sprintf("%.f", `Temperature(°C)`), group = Temperature_Type),position = position_dodge(width = 0.9), vjust = -0.7, size = 5)+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size = 10, colour = "black"))
    } else {
      return(ggplot() + theme_void())
    }
  })

  output$weather_summary = renderTable({
    data = getWeatherData()
    scale = temperature_scale()

    if (!is.null(data)) {
      summary_data = data$list |>
        mutate(Datee = substr(dt_txt,1,10)) |>
        group_by(Datee) |>
        summarise(
          Country = first(data$city$country),
          City = city_name(),
          Temperature = mean(kelvin_to_temperature(main.temp,scale)),
          Feels_Like = mean(kelvin_to_temperature(main.feels_like,scale)),
          Min_Temperature = min(kelvin_to_temperature(main.temp_min,scale)),
          Max_Temperature = max(kelvin_to_temperature(main.temp_max,scale)),
          Humidity = mean(main.humidity),
          Wind_Speed = mean(wind.speed)
        )

      colnames(summary_data) = c("Date", "Country", "City", paste("Temperature (", ifelse(scale == "Celsius", "°C", "°F"), ")"), paste("Feels Like (", ifelse(scale == "Celsius", "°C", "°F"), ")"), paste("Min Temperature (", ifelse(scale == "Celsius", "°C", "°F"), ")"), paste("Max Temperature (", ifelse(scale == "Celsius", "°C", "°F"), ")"), "Humidity(%)", "Wind Speed(m/s)")

      return(summary_data)
    }
  })
}
shinyApp(ui = ui, server = server)

