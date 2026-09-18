# gcamsdg
This repository includes different scripts to automatically compute and report Sustainable development Goals (SDG)-related indicators for alternative GCAM scenarios.

The tool covers two steps: (1) extracting results for a set of scenarios from a GCAM database (via [rgcam](https://github.com/JGCRI/rgcam)), and (2) post-processing those results, combined with external datasets and tools (e.g. [rfasst](https://github.com/bc3LC/rfasst) for health, [Demeter](https://github.com/JGCRI/demeter) for land) into SDG-specific indicators. Both steps happen through a single function, `run()`.

## Repository structure

`gcamsdg` is a real R package (not a folder of scripts to `source()`), following the same conventions as the sibling BC3 tools [gcamreport](https://github.com/bc3LC/gcamreport) and [rfasst](https://github.com/bc3LC/rfasst). Note: the repository is named `gcamsdg` (GitHub renamed from the earlier `gcam_sdg`, which R package names can't contain an underscore for anyway).

```
gcamsdg/
├── R/
│   ├── run.R                    - the single entry point (see below)
│   ├── cluster_submit.R         - internal: fills in and submits the bundled sbatch job for run(cluster = TRUE)
│   ├── load_prj.R               - load a saved rgcam project
│   ├── create_prj.R             - create an rgcam project from a GCAM database + queries
│   ├── sdg0_population.R        - SDG 0: population (basis for other indicators)
│   ├── sdg1_gdp.R                - SDG 1: GDP per capita
│   ├── sdg1_expenditure.R        - SDG 1: food + energy expenditure share of income
│   ├── sdg2_food_basket_bill.R  - SDG 2: food basket bill
│   ├── sdg3_health.R            - SDG 3: air-pollution-attributable mortality
│   ├── sdg6_water_scarcity.R    - SDG 6: water scarcity index
│   ├── sdg15_land_indicator.R   - SDG 15: potential species loss (PSL)
│   └── postprocess_sdg_diff.R   - shared diff-vs-baseline post-processing (used by run(show_diff = TRUE))
└── inst/extdata/                # bundled GCAM queries (queries_*.xml), lookup data (CF.csv, Ecoregions_shp/, etc.),
                                  # and the cluster job template (gcamsdg_cluster.sbatch, run_from_args.R)
```

## Installation

```r
# install.packages("devtools")
devtools::install_github("bc3LC/gcamsdg")
library(gcamsdg)
```

Or, when working directly on a local clone:

```r
devtools::load_all("path/to/gcamsdg")
```

## Usage

Everything goes through one function, `run()`. It accepts data three ways:

- an already-loaded rgcam project (`prj`)
- one or more existing project files (`prj_name`, no database info needed)
- one or more raw GCAM databases to extract from (`db_path` + `db_name` — pass a vector to process several separate scenario databases and combine the results in one call)

```r
# extract a single database and compute every SDG indicator
run(db_name = "database_basexdb_myscenario", sdgs = "all")

# just the ones you need - skips SDG15's slow Demeter run entirely
run(db_name = "database_basexdb_myscenario", sdgs = c("gdp", "water"))

# several policy-scenario databases at once, diffed against a baseline
run(db_name = c("database_basexdb_policyA", "database_basexdb_policyB", "database_basexdb_base"),
    show_diff = TRUE, base_scen = "myBaselineScenario")

# also produce the standard gcamreport output from the same project
run(db_name = "database_basexdb_myscenario", run_gcamreport = TRUE, GCAM_version = "v7.1")
```

See `?run` for the full parameter list, and the [step-by-step vignette](vignettes/Step_By_Step_Full_Example.Rmd) for worked examples of each mode.

## Outputs

`run()` returns a named list in memory, one data frame per computed SDG (`result$gdp`, `result$water`, ...), plus `$gcamreport` when requested. The same data is also written to disk under `<base_path>/gcamsdg/output/<SDG folder>/`, one subfolder per indicator (`SDG0-POP`, `SDG1-GDP`, `SDG1-Expenditure`, `SDG2-Poverty`, `SDG3-Health`, `SDG6-Water`, `SDG15-Land`), each with an `indiv_results/` (or similar) folder holding the raw CSV.

Set `makeFigures = TRUE` to also get a basic PNG per computed indicator under each folder's `figures/` subfolder: a scenario-colored time series for indicators with a year dimension, or a bar chart by scenario for SDG15's PSL (a single net 2020-2050 value, no year dimension). Regional indicators (population, GDP, health) are aggregated to one global series first. This does **not** produce geographic maps (e.g. water scarcity by basin, PSL by ecoregion) - the underlying indicator functions currently aggregate that spatial detail away before returning their result, so real choropleth maps would need a separate, deeper change.

## Running locally vs. on the BC3 cluster

`run()` takes `base_path` (run directory containing `output/`/`prj_files/`) and `conda_env` (Demeter's conda environment, only needed for `sdgs` including `"land"`) as plain arguments, defaulting to the BC3 "DIPC" cluster's values. For a local run or a different cluster, just pass your own:

```r
run(db_name = "...", base_path = "/path/to/your/GCAM_run_dir",
    conda_env = "/path/to/your/demeter-conda-env")
```

**To actually run on the BC3 cluster as a SLURM job** instead of in your current R session, set `cluster = TRUE`:

```r
run(db_name = "database_basexdb_myscenario", sdgs = "all", cluster = TRUE)
```

This fills in the bundled sbatch template ([inst/extdata/gcamsdg_cluster.sbatch](inst/extdata/gcamsdg_cluster.sbatch)) with the call's arguments and submits it via `sbatch` — **fire-and-forget**: it returns the job ID immediately rather than waiting for the job to finish (SLURM jobs run later, asynchronously, on a compute node). Check on it the normal SLURM way (`squeue`, the log file path returned by the call); results land under `<base_path>/gcamsdg/output/` once it completes.

Before your first cluster run, edit the `#SBATCH` directives at the top of `inst/extdata/gcamsdg_cluster.sbatch` once for your own account/partition/resource needs (or override individual ones per call via `run(..., cluster = TRUE, sbatch_args = list(time = "48:00:00"))` without touching the file). `cluster = TRUE` only works with the `db_path`+`db_name` or existing-`prj_name`-file input modes — an in-memory `prj` object can't be handed to a separate job on another node.

## SDG description

### SDG 1: Poverty

Source: https://www.un.org/sustainabledevelopment/poverty/

Script: [sdg1_expenditure.R](R/sdg1_expenditure.R)

To measure the impact of mitigation pathways on poverty, we focus on household expenditures on residential (home) energy and food relative to their average income, reflecting the relative pressure on households to meet their basic needs. 

This indicator is calculated for all representative households globally, which are based on 10 income deciles for each of the 32 socioeconomic regions in GCAM, combining to a total of 320 representative households. We then take the average value for all representative households weighted by the relative population in each group. While this includes results for households that do not suffer poverty, the quantification of “absolute poverty” is subjective, and we preferred to include relative poverty in the equation. The highest values in terms of expenditures, as well as variation between scenarios, will be found in the poorest households which have the smallest denominator (i.e., income), and hence these households will mainly drive the outcome of this indicator. For the interpretation of the indicator, one should thus wary that small global variations may include significant impacts in the poorest households.

The following formula describes the calculation of the income expenditure share required for basic needs (IESBN):

$$
IESBN_{t,s} =
\frac{
    \sum_r \sum_h 
    \left(
        \frac{E_{t,s}}{I_{t,s}} + \frac{F_{t,s}}{I_{t,s}}
    \right) \cdot pop_{t,s}
}{
    \sum_r \sum_h pop_{t,s}
}
$$

Where $E reflects home energy expenditure, $F food expenditure, $I income, r the socioeconomic regions in GCAM, h the representative households (income deciles) within those regions, pop the population of the representative household in the region, t the model simulation year and r the scenario.

### SDG 2: Zero hunger
Source: https://www.un.org/sustainabledevelopment/hunger/

Script: [sdg2_food_basket_bill.R](R/sdg2_food_basket_bill.R)

Description: 

In this implementation, SDG 2 is represented as the (avoided) per capita food basket bill. The calculations estimate the annual regional expenditure by a median consumer. Specifically, for each period t and region r, all food items are aggregated into *Staples* and *Non-Staples*. Then, the total consumption of each group has been multiplied by its price, as described in the equation below:

$$FoodExpenditurePC_{t,r} = \sum_{fs\ in\ foodStapleItems} ConsumptionPC_{fs,t,r} \cdot PricePCStaples_{t,r} \ + $$

$$\sum_{fc\ in\ foodNonStapleItems} ConsumptionPC_{fn,t,r} \cdot PricePCNonStaples_{t,r}$$

### SDG 3: Ensure healthy lives and promote well-being for all at all ages
Source: https://www.un.org/sustainabledevelopment/health/

Script: [sdg3_health.R](R/sdg3_health.R)

Description: 

In this implementation, SDG 3 is represented as the (avoided) premature mortalities attributable to long-term exposure to both fine particulate matter (PM2.5) and tropospheric ozone (O3). Calculations have been computed using rfasst, a tool designed to quantify adverse health and agricultural effects attributable to air pollution for alternative scenarios (Sampedro et al 2022). The tool mimics the well-stablished TM5-FASST model (Van Dingenen et al 2018). 
The calculation of premature mortalities is based on the population-attributable fraction approach, so premature mortality (Mort) for cause c, in period t, region r, associated with exposure to pollutant j, is calculated as the product between the baseline mortality rate, the change in the RR relative risk of death attributable to a change in population-weighted mean pollutant concentration, and the population exposed. This is described in the following equation:

$$Mort_{c,t,r,j}=mo_{c,r,j}\cdot\frac{RR_{c,j}-1}{RR_{c,j}}\cdot Pop_{t,r}$$

Mortalities are estimated for six causes, namely stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI), lung cancer (LC), and diabetes Mellitus Type II (DM). IHD and STROKE are calculated for different age groups, using “age-group-specific” parameters (i.e., mortality rates and relative risks), while COPD, ALRI, LC and DM are estimated for the whole population exposed (pop > 25 years).
- Population exposed is cause-specific. Population fractions are calculated from the from the SSP database: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=10

- Cause-specific baseline mortality rates are computed using absolute mortality from the Global Burden of Disease (GBD) (https://vizhub.healthdata.org/gbd-compare/) and population statistics (to get exposed population and fractions). The projected changes of these rates over time are taken from the World Health Organization projections.

- For PM2.5, relative risk is calculated based on Integrated Exposure-Response functions (ERFs) from the GBD 2017 (Stanaway et al 2018). Compared to previous assessments (e.g., Burnett et al 2014), this method includes age-group-specific parameters and the addition of diabetes mellitus type II.

- For O3, relative risk is based on the ERFs from Jerrett et al 2009. 

References SDG3

- Burnett R T, Pope C A III, Ezzati M, Olives C, Lim S S, Mehta S, Shin H H, Singh G, Hubbell B, Brauer M, Anderson H R, Smith K R, Balmes J R, Bruce N G, Kan H, Laden F, Prüss-Ustün A, Turner M C, Gapstur S M, Diver W R and Cohen A 2014 An Integrated Risk Function for Estimating the Global Burden of Disease Attributable to Ambient Fine Particulate Matter Exposure Environmental Health Perspectives Online: http://ehp.niehs.nih.gov/1307049/

- Jerrett M, Burnett R T, Pope III C A, Ito K, Thurston G, Krewski D, Shi Y, Calle E and Thun M 2009 Long-term ozone exposure and mortality New England Journal of Medicine 360 1085–95

- Sampedro J, Khan Z, Vernon C R, Smith S J, Waldhoff S and Dingenen R V 2022 rfasst: An R tool to estimate air pollution impacts on health and agriculture Journal of Open Source Software 7 3820

- Stanaway J D, Afshin A, Gakidou E, Lim S S, Abate D, Abate K H, Abbafati C, Abbasi N, Abbastabar H and Abd-Allah F 2018 Global, regional, and national comparative risk assessment of 84 behavioural, environmental and occupational, and metabolic risks or clusters of risks for 195 countries and territories, 1990–2017: a systematic analysis for the Global Burden of Disease Study 2017 The Lancet 392 1923–94

- Van Dingenen R, Dentener F, Crippa M, Leitao J, Marmer E, Rao S, Solazzo E and Valentini L 2018 TM5-FASST: a global atmospheric source–receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants Atmospheric Chemistry and Physics 18 16173–211

## SDG 6: Clean Water and Sanitation
Source: https://www.un.org/sustainabledevelopment/water-and-sanitation/

Script: [sdg6_water_scarcity.R](R/sdg6_water_scarcity.R)

Description: 

The water module of GCAM is structured in 235 basins. The water withdrawals are defined as water diverted from a surface water or groundwater source. They are estimated for six major sectors: agriculture, electricity generation, industrial manufacturing, primary energy production, livestock and municipal uses. 

The water supply separates three distinct sources of fresh water: renewable water, non-renewable groundwater and desalinated water. Renewable water is water that is replenished naturally by surface runoff and subsurface infiltration and release. It is determined by the natural streamflow, baseflow, the total reservoir storage and the environmental flow requirement for each basins (Kim et al., 2016). 

We quantify physical water scarcity as the ratio of water withdrawals to renewable water supply for each basin (Birnbaum et al., 2022). The fraction of water demand relative to available renewable surface water supply has also been labelled ‘water stress index’ in previous studies (Byers et al., 2018). To derive a single estimate per scenario, we compute the average of the index weighted by the volume of renewable water withdrawal of each basin at baseline year (2015). The index is estimated with the following equations: 

$$I_{s,b}=W_{s,b}/S_{s,b}$$ 

and

$$I_{s} = sum_{b=1}^b (I_{s,b} * W_{s,b,2015}) / sum_{b=1}^b W_{s,b,2015}$$

Where I is the water scarcity index per basin (dimensionless), W is the water withdrawal in km3, S is the renewable water supply in km3, b are the basins, and s are the scenarios. 

References SDG6

- Birnbaum, A., Lamontagne, J., Wild, T., Dolan, F., & Yarlagadda, B. (2022). Drivers of Future Physical Water Scarcity and Its Economic Impacts in Latin America and the Caribbean. Earth’s Future, 10(8), e2022EF002764. https://doi.org/10.1029/2022EF002764
- Byers, E., Gidden, M., Leclère, D., Balkovic, J., Burek, P., Ebi, K., Greve, P., Grey, D., Havlik, P., Hillers, A., Johnson, N., Kahil, T., Krey, V., Langan, S., Nakicenovic, N., Novak, R., Obersteiner, M., Pachauri, S., Palazzo, A., … Riahi, K. (2018). Global exposure and vulnerability to multi-sector development and climate change hotspots. Environmental Research Letters, 13(5), 055012. https://doi.org/10.1088/1748-9326/aabf45
- Kim, S. H., Hejazi, M., Liu, L., Calvin, K., Clarke, L., Edmonds, J., Kyle, P., Patel, P., Wise, M., & Davies, E. (2016). Balancing global water availability and use at basin scale in an integrated assessment model. Climatic Change, 136(2), 217–231. https://doi.org/10.1007/s10584-016-1604-6

## SDG15: Life of Land
Source: https://www.un.org/sustainabledevelopment/biodiversity/

Script: [sdg15_land_indicator.R](R/sdg15_land_indicator.R)

Description: 

The land module is structured around 384 distinct land-water regions, called LUTs, and provides outputs on land allocation for 43 land uses per LUT. It computes supply, demand, and land utilisation in various sectors, encompassing food, feed, fiber, forestry, and bioenergy production. The land uses are categorised into two categories, namely managed and unmanaged land. The latter includes unmanaged forests, unmanaged pasture, shrubland, grassland, tundra and other land uses (e.g., rock, ice and desert). 

The indicator reported for SDG15 is the net Potential Species Loss (PSL) indicator, which represents the number of plant and vertebrate species committed to extinction due to habitat loss through land use change (LUC) from unmanaged to managed land. The PSL indicator considers the current threat level faced by the species, the level of endemism, and the capacity of the species to adapt to this new habitat (Chaudhary et al., 2015; Chaudhary & Brooks, 2018). 

First, the land use projections from GCAM were downscaled with the Demeter model (Vernon et al., 2018) to increase the results’ resolution (0.5ºx0.5º) for the main land uses. These downscaled land use projections are then aggregated at the ecoregion level which is the spatial unit required to derive the biodiversity indicator.  (Chaudhary & Brooks, 2018). For each ecoregion and land use, the LUC is estimated as the area difference between 2020 and 2050. 

Second, the PSL is estimated from the ecoregion-based LUC projections and aggregated characterisation factors (CF). CFs provide PSL per unit area (ha) for five broad managed land uses (forestry, cropland, both irrigated and rainfed, pastureland, and urban land) under distinct management intensity levels for 804 terrestrial ecoregions. Extended Data Table 3 illustrates the matching between GCAM land uses and the land use types and management intensity of the CFs. 

The aggregated CFs reflect species loss across five taxa combined (mammals, birds, amphibians, reptiles and plants) and were obtained from Chaudhary & Brooks, (2018). The PSL indicator of each scenario is estimated with the following equations: 


$$
LUC_{l,r} = A_{l,r,2050} - A_{l,r,2020}
$$

$$
PSL_{l,r} = LUC_{l,r} \times CF_{l,r}
$$

$$
PSL = \sum_l \sum_r PSL_{l,r}
$$

Where A is the area in ha, LUC is the land use change between 2020 and 2050 in ha, CF is the aggregated characterisation factor in species loss per ha, PSL is the net potential species loss of the scenario, l are the managed land uses, and r are the ecoregions.  

References SDG15

- Chaudhary, A., & Brooks, T. M. (2018). Land Use Intensity-Specific Global Characterization Factors to Assess Product Biodiversity Footprints. Environmental Science & Technology, 52(9), 5094–5104. https://doi.org/10.1021/acs.est.7b05570
- Chaudhary, A., Verones, F., de Baan, L., & Hellweg, S. (2015). Quantifying Land Use Impacts on Biodiversity: Combining Species–Area Models and Vulnerability Indicators. Environmental Science & Technology, 49(16), 9987–9995. https://doi.org/10.1021/acs.est.5b02507
- Vernon, C. R., Le Page, Y., Chen, M., Huang, M., Calvin, K. V., Kraucunas, I. P., & Braun, C. J. (2018). Demeter – A Land Use and Land Cover Change Disaggregation Model. Journal of Open Research Software, 6(1), 15. https://doi.org/10.5334/jors.208


## GCAM SDG studies

- Moreno, J., Campagnolo, L., Boitier, B., Nikas, A., Koasidis, K., Gambhir, A., Gonzalez-Eguino, M., Perdana, S., Van de Ven, D.J., Chiodi, A. and Delpiazzo, E., 2024. The impacts of decarbonization pathways on Sustainable Development Goals in the European Union. Communications Earth & Environment, 5(1), p.136.

- Moreno, J., Van de Ven, D.J., Sampedro, J., Gambhir, A., Woods, J. and Gonzalez-Eguino, M., 2023. Assessing synergies and trade-offs of diverging Paris-compliant mitigation strategies with long-term SDG objectives. Global Environmental Change, 78, p.102624.

- Iyer, G., Calvin, K., Clarke, L., Edmonds, J., Hultman, N., Hartin, C., McJeon, H., Aldy, J. and Pizer, W., 2018. Implications of sustainable development considerations for comparability across nationally determined contributions. Nature Climate Change, 8(2), pp.124-129.

