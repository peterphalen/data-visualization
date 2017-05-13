# Mapping police killings
R scripts that generate interactive online visualizations of police killings. Uses US census data, a file scraped from the Guardian's The Counted website for pre-2017 data, and a file scraped from the Washington Post for post-2017 (some GIS assembly required). 

Output of the [map-police-killings.R](map-police-killings.R)
: 
https://www.peterphalen.com/datavisualization/map-police-killings.html

Output of [poverty-police-killings.R](poverty-police-killings.R) here: 
https://www.peterphalen.com/datavisualization/poverty-police-killings.html

 Both the above scripts reference [this function](process-wapost-killings.R), which processes [the Washington Post's dataset](https://www.washingtonpost.com/graphics/national/police-shootings-2017/) for post-2017 killings.

The output of [per-capita-killings-by-state.R](per-capita-killings-by-state.R) shows police killings of black people per capita by state:
https://www.peterphalen.com/datavisualization/police-killings-graph-viz.html
