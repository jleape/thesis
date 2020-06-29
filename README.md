
# Winning the Housing Lottery in Rio de Janeiro: Curse or Cure?  
Available soon on [dspace.mit.edu](https://dspace.mit.edu)

This repo contains the R code for all data analysis in my MIT Master's thesis. I borrowed heavily from both [Taina Pacheco](http://bibliotecadigital.fgv.br/dspace/handle/10438/27253) and [Rafael H M Pereira](https://github.com/rafapereirabr), and reference them both in the thesis.

## IMPORT DATA
- participants.R – combines PMCMV data to identify applicants, lottery winners and beneficiaries for all lotteries in a single table.
- import_cadunico.R – imports, standardizes and combines yearly CADUNICO files, filtering for variables used in balancing and accessibility computation.
- import_rais.R – imports, standardizes and combines yearly RAIS – Vinculados files, filtering for variables used in balancing, matching and accessibility computation, and for program participants.

## ACCESSIBILITY
- hex_grid.R – creates a hexagonal grid over the Rio de Janeiro municipality for raster analysis.
- hex_opps.R – calculates the number of jobs in each cell of hex grid by spatially joining the RAIS – Establecimentos database.
- osrm_ttm.R – calculates travel time matrices for the hex grid for each year using the GTFS feeds, OpenStreetMap and OpenTripPlanner.
- accessibility.R – calculates percentage of jobs accessible from each hex grid cell for each year and for a range of travel times. Maps results.

## PANEL CONSTRUCTION
- balancing_F.R – conducts omnibus test of orthogonal and normalized differences in means between lottery winners and losers in order to gauge their balance throughout the construction of the panel.
- correlation.R – creates a correlation matrix for all variables in the panel
- panel2.R – constructs panels and fits linear regression models for analyses described in Deliverables. Coded but cluster required to execute.

## RESULTS
- decision.R – runs a logistic regression to estimate the probability of deciding to move among lottery winners.
- regressions.R – runs one and two-staged regressions to estimate the effect of moving to a PMCMV unit on the probability of employment, income and weekly contracted hours in the formal labor market
- hetero_effects.R – discretizes variables to estimate their CATE on the probability of employment, income and weekly contracted hours in the formal labor market
