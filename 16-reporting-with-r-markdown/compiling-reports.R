---
title: "Cloud Cover"
author: "Anonymous"
date: "December 2, 2014"
output: pdf_document
---

## Data

The `atmos` data set resides in the `nasaweather` package of the *R* programming language. It contains a collection of atmospheric variables measured between 1995 and 2000 on a grid of 576 coordinates in the western hemisphere. The data set comes from the [2006 ASA Data Expo](http://stat-computing.org/dataexpo/2006/).

Some of the variables in the `atmos` data set are:

* **cloudlow** - The mean percent of the sky covered by clouds at low altitudes.

* **cloudmid** - The mean percent of the sky covered by clouds at mid-range altitudes.

* **cloudhigh** - The mean percent of the sky covered by clouds at high altitudes.

You can convert the temperature unit from Kelvin to Celsius with the formula

$$ celsius = kelvin - 273.15 $$

And you can convert the result to Fahrenheit with the formula

$$ fahrenheit = celsius \times \frac{9}{5} + 32 $$

## Cleaning

```{r echo = FALSE}
year <- 2000
```

For the remainder of the report, we will look only at data from the year `r year`. We aggregate our data by location, using the *R* code below.

```{r echo = FALSE, message = FALSE}
library(nasaweather)
library(dplyr)
library(tidyr)
```

```{r}
means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
         pressure = mean(pressure, na.rm = TRUE),
         ozone = mean(ozone, na.rm = TRUE),
         cloudlow = mean(cloudlow, na.rm = TRUE),
         cloudmid = mean(cloudmid, na.rm = TRUE),
         cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

clouds <- means %>%
  select(-(temp:ozone)) %>%
  gather("altitude", "coverage", 3:5)
```


---
title: "Cloud Cover"
author: "Anonymous"
date: "December 2, 2014"
output: slidy_presentation
---

## Data

The `atmos` data set resides in the `nasaweather` package of the *R* programming language. It contains a collection of atmospheric variables measured between 1995 and 2000 on a grid of 576 coordinates in the western hemisphere. The data set comes from the [2006 ASA Data Expo](http://stat-computing.org/dataexpo/2006/).

***

Some of the variables in the `atmos` data set are:

* **cloudlow** - The mean percent of the sky covered by clouds at low altitudes.

* **cloudmid** - The mean percent of the sky covered by clouds at mid-range altitudes.

* **cloudhigh** - The mean percent of the sky covered by clouds at high altitudes.

***

You can convert the temperature unit from Kelvin to Celsius with the formula

$$ celsius = kelvin - 273.15 $$

And you can convert the result to Fahrenheit with the formula

$$ fahrenheit = celsius \times \frac{9}{5} + 32 $$

## Cleaning

```{r echo = FALSE}
year <- 2000
```

For the remainder of the report, we will look only at data from the year `r year`. We aggregate our data by location, using the *R* code below.

```{r echo = FALSE, message = FALSE}
library(nasaweather)
library(dplyr)
library(tidyr)
```

```{r}
means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
         pressure = mean(pressure, na.rm = TRUE),
         ozone = mean(ozone, na.rm = TRUE),
         cloudlow = mean(cloudlow, na.rm = TRUE),
         cloudmid = mean(cloudmid, na.rm = TRUE),
         cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

clouds <- means %>%
  select(-(temp:ozone)) %>%
  gather("altitude", "coverage", 3:5)
```


---
title: "Ozone"
author: "Anonymous"
date: "January 1, 2015"
output:
  html_document:
    toc: true
    number_sections: true
---

## Data

The `atmos` data set resides in the `nasaweather` package of the *R* programming language. It contains a collection of atmospheric variables measured between 1995 and 2000 on a grid of 576 coordinates in the western hemisphere. The data set comes from the [2006 ASA Data Expo](http://stat-computing.org/dataexpo/2006/).

Some of the variables in the `atmos` data set are:

* **temp** - The mean monthly air temperature near the surface of the Earth (measured in degrees kelvin (*K*))

* **pressure** - The mean monthly air pressure at the surface of the Earth (measured in millibars (*mb*))

* **ozone** - The mean monthly abundance of atmospheric ozone (measured in Dobson units (*DU*))

You can convert the temperature unit from Kelvin to Celsius with the formula

$$ celsius = kelvin - 273.15 $$

And you can convert the result to Fahrenheit with the formula

$$ fahrenheit = celsius \times \frac{9}{5} + 32 $$

## Cleaning

```{r echo = FALSE}
year <- 2000
```

For the remainder of the report, we will look only at data from the year `r year`. We aggregate our data by location, using the *R* code below.

```{r message = FALSE}
library(nasaweather)
library(dplyr)
library(ggvis)
```

```{r}
means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
         pressure = mean(pressure, na.rm = TRUE),
         ozone = mean(ozone, na.rm = TRUE),
         cloudlow = mean(cloudlow, na.rm = TRUE),
         cloudmid = mean(cloudmid, na.rm = TRUE),
         cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()
```

where the `year` object equals `r year`.


## Ozone and temperature

Is the relationship between ozone and temperature useful for understanding fluctuations in ozone? A scatterplot of the variables shows a strong, but unusual relationship.

```{r echo = FALSE, fig.height = 4, fig.width = 5}
means %>%
  ggvis(~temp, ~ozone) %>%
  layer_points()
```

We suspect that group level effects are caused by environmental conditions that vary by locale. To test this idea, we sort each data point into one of four geographic regions:

```{r}
means$locale <- "north america"
means$locale[means$lat < 10] <- "south pacific"
means$locale[means$long > -80 & means$lat < 10] <- "south america"
means$locale[means$long > -80 & means$lat > 10] <- "north atlantic"
```

### Model

We suggest that ozone is highly correlated with temperature, but that a different relationship exists for each geographic region. We capture this relationship with a second order linear model of the form

$$ ozone = \alpha + \beta_{1} temperature + \sum_{locales} \beta_{i} locale_{i} + \sum_{locales} \beta_{j} interaction_{j} + \epsilon$$

This yields the following coefficients and model lines.

```{r}
lm(ozone ~ temp + locale + temp:locale, data = means)
```

```{r echo = FALSE, fig.height = 4, fig.width = 5}
means %>%
  group_by(locale) %>%
  ggvis(~temp, ~ozone) %>%
  layer_points(fill = ~locale) %>%
  layer_model_predictions(model = "lm", stroke = ~locale) %>%
  hide_legend("stroke") %>%
  scale_nominal("stroke", range = c("darkorange", "darkred", "darkgreen", "darkblue"))
```

### Diagnostics

An anova test suggests that both locale and the interaction effect of locale and temperature are useful for predicting ozone (i.e., the p-value that compares the full model to the reduced models is statistically significant).

```{r}
mod <- lm(ozone ~ temp, data = means)
mod2 <- lm(ozone ~ temp + locale, data = means)
mod3 <- lm(ozone ~ temp + locale + temp:locale, data = means)

anova(mod, mod2, mod3)
```

---
title: "Shiny Demo"
author: "DataCamp"
output: html_document
runtime: shiny
---

This R Markdown document is made interactive using Shiny. Unlike the more traditional workflow of creating static reports, you can now create documents that allow your readers to change the assumptions underlying your analysis and see the results immediately.

To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

## Inputs and Outputs

You can embed Shiny inputs and outputs in your document. Outputs are automatically updated whenever inputs change.  This demonstrates how a standard R plot can be made interactive by wrapping it in the Shiny `renderPlot` function. The `selectInput` and `sliderInput` functions create the input widgets used to drive the plot.

```{r, echo=FALSE}
inputPanel(
  selectInput("n_breaks", label = "Number of bins:",
              choices = c(10, 20, 35, 50), selected = 20),

  sliderInput("bw_adjust", label = "Bandwidth adjustment:",
              min = 0.2, max = 2, value = 1, step = 0.2)
)

renderPlot({
  hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
       xlab = "Duration (minutes)", main = "Geyser eruption duration")

  dens <- density(faithful$eruptions, adjust = input$bw_adjust)
  lines(dens, col = "blue")
})
```

## Embedded Application

It is also possible to embed an entire Shiny application within an R Markdown document using the `shinyAppDir` function. This example embeds a Shiny application located in another directory:

```{r, echo=FALSE}
shinyAppDir(
  system.file("examples/06_tabsets", package = "shiny"),
  options = list(
    width = "100%", height = 550
  )
)
```

Note the use of the `height` parameter to determine how much vertical space the embedded application should occupy.

You can also use the `shinyApp` function to define an application inline rather then in an external directory.

In all of R code chunks above the `echo = FALSE` attribute is used. This is to prevent the R code within the chunk from rendering in the document alongside the Shiny components.



---
title: "ggvis"
author: "DataCamp"
output: html_document
runtime: shiny
---

ggvis provides a number of ways to enhance plots with interacticity. For example, the density plot below allows users to set the kernel and bandwidth of the plot.

```{r echo = FALSE, message = FALSE}
library(ggvis)

mtcars %>% ggvis(x = ~wt) %>%
    layer_densities(
      adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
      kernel = input_select(
        c("Gaussian" = "gaussian",
          "Epanechnikov" = "epanechnikov",
          "Rectangular" = "rectangular",
          "Triangular" = "triangular",
          "Biweight" = "biweight",
          "Cosine" = "cosine",
          "Optcosine" = "optcosine"),
        label = "Kernel")
    )
```




