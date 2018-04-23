## Cleaning

For the remainder of the report, we will look only at data from the year 1995. We aggregate our data by location, using the *R* code below.

```{r}
library(nasaweather)
library(dplyr)

year <- 1995

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

## Cleaning

For the remainder of the report, we will look only at data from the year 1995. We aggregate our data by location, using the *R* code below.

```{r message = FALSE}
library(nasaweather)
library(dplyr)
```

```{r}
year <- 1995

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

For the remainder of the report, we will look only at data from the year 1995. We aggregate our data by location, using the *R* code below.

```{r message = FALSE}
load(url("http://assets.datacamp.com/course/rmarkdown/atmos.RData")) # working with a subset
library(dplyr)
library(ggvis)
```

```{r}
year <- 1995
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
output: html_document
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



## Exploring the mtcars data set

Have you ever wondered whether there is a clear correlation between the gas consumption of a car and its weight?
To answer this question, we first have to load the `dplyr` and `ggvis` packages.

```{r message = FALSE}
library(dplyr)
library(ggvis)
```

```{r chained, results = 'hide'}
mtcars %>%
  group_by(factor(cyl)) %>%
  ggvis(~mpg, ~wt, fill = ~cyl) %>%
  layer_points()
```

The `ggvis` plot gives us a nice visualization of the `mtcars` data set:

```{r ref.label='chained', echo = FALSE}
```



