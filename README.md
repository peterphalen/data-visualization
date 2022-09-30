# Mapping police killings
R scripts that generate interactive online visualizations of police killings. Uses US census data, a file scraped from the Guardian's The Counted website for pre-2017 data, and a file scraped from the Washington Post for post-2017 (some GIS assembly required). 

The [map-police-killings.R](map-police-killings.R) script outputs a [map of armed versus unarmed killings](https://www.peterphalen.com/datavisualization/map-police-killings.html) as well as a [map showing killings with overlaid income demographics](http://www.peterphalen.com/datavisualization/poverty-police-killings.html). 

The repo includes [this function](process-wapost-killings.R), which processes [the Washington Post's dataset](https://www.washingtonpost.com/graphics/national/police-shootings-2017/) to determine latitude and longitude for post-2017 killings. However, this function is no longer called because WaPo started releasing latitude and longitude a couple years ago ago. 

The [output](https://www.peterphalen.com/datavisualization/police-killings-graph-viz.html) of the [per-capita-killings-by-state.R](per-capita-killings-by-state.R) script shows police killings of black people per capita by state.

