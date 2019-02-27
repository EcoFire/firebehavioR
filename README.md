[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1412014.svg)](https://doi.org/10.5281/zenodo.1412014)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) 
![GitHub release](https://img.shields.io/badge/devel%20version-v0.1.0-blue.svg) [![Travis-CI Build Status](https://travis-ci.org/EcoFire/firebehavioR.svg?branch=master)](https://travis-ci.org/EcoFire/firebehavioR) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![](https://www.r-pkg.org/badges/version/firebehavioR)](https://www.r-pkg.org/badges/version/firebehavioR)

firebehavioR
================

An R package for predicting fire behavior using the Rothermel modelling system or the Crown Fire Initiation & Spread modelling system.

Features
--------

Fire behavior predictions using:
* the Rothermel modelling system, similar to BehavePlus, NEXUS, and FuelCalc
* the Crown Fire Initiation & Spread modelling system

Fire weather indices of two types:
* Static (uses instantaneous weather data)
* Dynamic (evolving index value based on continuous daily weather data)

Helper functions which can:
* Calculate the wind adjustment factor, using a little or a lot of site-specific forest canopy information
* Predict canopy fuels characteristics such as canopy bulk density and canopy fuel load
* Determine fine fuel moisture from meteorological observations
* Plot fire behavior outputs onto the Fire Characteristics Chart

Helper data:
* Stylized surface fuel models
* Surface fuel moisture scenarios

Installation
------------
firebehavioR is available on the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/package=firebehavioR).
Alternatively, you can install firebehavioR from GitHub for the development version.

    devtools::install_github("EcoFire/firebehavioR")

 fireChart() requires ggplot2; otherwise there are no package dependencies. You should have R (&gt;= 3.4.1) installed.

Usage
-----

The vignette in the above references GitHub repo will help you get up and going.

Future Development
-----

This package is a continual work in progress. Suggestions for improvements are welcomed. Currently, additional helper functions are planned: 
* Incorporation of models to estimate ~~dead and~~ live fuel moistures using RAWS weather observations 
* Additional methods to estimate canopy fuels characteristics
* ~~Visual interpretation of fire behavior results via the Fire Characteristics Chart~~

Authors
-------

-   **Justin Ziegler** - developer and maintainer, <Justin.Ziegler@Colostate.edu>

License
-------

GPL (&gt;= 2)
