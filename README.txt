Authors of the scripts: Jan van de Kassteele & Annemieke Mulder

# Background information
The scripts published at this github page are part of an (in the near future) published article of my 
PHD project with the following title:

Spatial Effects of Livestock Farming Exposure on Human Infections with Shiga Toxin-Producing Escherichia coli O157 
in A Small But Densely Populated Region: The Case of the Netherlands

The article can be found here: ... (will be updated as soon as the article is published.

# Information about scripts

Masterscript: 
STEC_O157_and_animal_numbers_2007-2016_25-03-2019_github.R

In this script all data is gathered, cleaned and used for the spatial analysis as are described in the afore mentioned article.

# Data gathering

Before this script can be used, it is important to gather the proper data: 

	1. Polygon files of the postal code 4 (pc4) areas (polygons) for each year that the analysis will be performed
	2. A text file in which postal code 6 (pc6) point locations are given together with the total population per pc6 point location
	3. Case data of the infection including information on:
		- Age
		- Sex
		- foreign visits
		- PC4
		- onset date
		- report date
	4. Animal data including point locations (x and y coordinates)

And run the following scripts first to gather the proper data and to have the necessary functions available:

	1. download_CBS_bevolking_en_huishoudens_viercijferige_postcode_github.R
	2. group_geometries_github.R
	3. st_make_hexgrid_github.R
	4. create_hexagons_github.R
	5. sf2nb_github.R

# PAF calculation

For the article, we wanted to calculate the population attributable fraction (PAF). I got an example script from Jan van de Kassteele.
The script with the extension _article_ is the one used for the final calculation. Those scripts are not necessary to be able to run
the masterscript.

	- PAF_calculation_example_github.R
	- PAF_calculation_article_github.R

# Interpretation RR as outcome of our analyses

This script delivers a figure that is used in the appendix of the article for the interpretation of the RRs as outcome of our analysis.
It is also written by Jan van de Kassteele. This script is not necessary to be able to run the masterscript.

	- Interpretation_RR_Poisson_regression_log-link_function_github.R

# How to use log1p scaling in ggplot

Example script of how to use log1p scaling in ggplot, created by Jan van de Kassteele. This script is not necessary to be able 
to run the masterscript.

	- log1p_scaling_ggplot_github.R






