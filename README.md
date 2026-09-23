# gcamsdg

The `gcamsdg` package automates the extraction and post-processing of **GCAM** and **GCAM-Europe** scenario data to compute key Sustainable Development Goal (SDG) indicators.

The workflow consists of two integrated phases:

1. **Extraction:** Querying raw scenario data from a GCAM database via [`rgcam`](https://github.com/JGCRI/rgcam).

2. **Post-Processing:** Coupling extracted GCAM outputs with specialized models and external datasets (e.g., [`gcamreport`](https://github.com/bc3LC/gcamreport) for standardized reporting, [`rfasst`](https://github.com/bc3LC/rfasst) for health impacts, and [`Demeter`](https://github.com/JGCRI/demeter) for spatial land disaggregation) to compute policy-relevant SDG indicators.

Both steps are executed seamlessly through a single master function: `generate_sdg_report()`.

---

<!-- ------------------------>
<!-- ------------------------>
## <a name="Contents"></a>Contents
<!-- ------------------------>
<!-- ------------------------>

- [Installation](#Installation)
- [Usage](#Usage)
- [Outputs](#Outputs)
- [Differece with a Baseline](#Difference_with_a_Baseline)
- [Local Execution vs. BC3 Cluster (SLURM)](#Difference_with_a_Baseline)
  - [SDG 1: Poverty](#SDG1:)
  - [SDG 2: Zero hunger](#SDG2)
  - [SDG 3: Ensure healthy lives and promote well-being for all at all ages](#SDG3)
  - [SDG 6: Clean water and sanitation](#SDG6)
  - [SDG 15: Life of land](#SDG15)
- [How to contribute](#How_to_contribute) 
- [Citation](#Citation)

---
  
<!-- ------------------------>
<!-- ------------------------>
## <a name="Installation"></a>Installation
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


Install the latest version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("bc3LC/gcamsdg")
library(gcamsdg)
```

Or, for local development within an RStudio project clone:
```r
devtools::load_all()
```


<!-- ------------------------>
<!-- ------------------------>
## <a name="Usage"></a>Usage
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


The main function is `generate_sdg_report()`. It supports three input modalities:
  
a) In-memory `rgcam` project (`prj`): Pass an already loaded project object.

b) Saved `rgcam` project files (`prj_name`): Pass one or more `.dat`/`.prj` file paths.

c) Raw GCAM databases (`db_path` + `db_name`): Extract directly from one or multiple databases in a single pass.


```r
library(gcamsdg)

# Example 1. - Process an existing project file for all SDGs
generate_sdg_report(
  prj_name = "database_basexdb_myscenario.dat", 
  sdgs = "all"
)

# Example 2. - Extract specific SDGs directly from a database (e.g., skipping land to avoid Demeter overhead)
generate_sdg_report(
  db_name = "database_basexdb_myscenario",
  db_path = "path/to/db",
  sdgs = c("gdp", "water")
)

# Example 3. - Combine multiple policy scenario databases in one call
generate_sdg_report(
  db_name = c("db_policyA", "db_policyB", "db_policyC"),
  db_path = "path/to/all/dbs",
  sdgs = "all"
)

# Example 4. - Include full gcamreport standardized variables alongside SDG indicators
generate_sdg_report(
  db_name = "database_basexdb_myscenario",
  db_path = "path/to/db",
  run_gcamreport = TRUE, 
  GCAM_version = "v7.1"
)
```
See `?generate_sdg_report` for full parameter options and consult the [step-by-step vignette](vignettes/Step_By_Step_Full_Example.Rmd) for detailed workflows.

<!-- ------------------------>
<!-- ------------------------>
## <a name="Outputs"></a>Outputs
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


When executed, `generate_sdg_report()` yields both an in-memory dataset and disk outputs inside an automatically created `output/` directory:
  
- Combined results: Exported as CSV files directly in `output/` and as RData elements in the same folder.

- Indicator directories: Subdirectories are created per indicator module (`output/SDG0-POP`, `output/SDG1-GDP`, `output/SDG2-Poverty`, `output/SDG3-Health`, `output/SDG6-Water`, `output/SDG15-Land`). Each contains an `indiv_results/` subfolder storing raw outputs.

- Automated visualizations (`makeFigures = TRUE`): Generates PNG summary plots in `output/figures/`. These are time-series scenario-colored trend plots for time-dependent indicators (aggregated globally for regional indicators).

- Standardized GCAM variables (`run_gcamreport = TRUE`): Integrates broad IAMC-style outputs generated via `gcamreport::generate_report()`. Pass custom arguments using the `gcamreport_args``` list and specify `GCAM_version`.

<!-- ------------------------>
<!-- ------------------------>
## <a name="Difference_with_a_Baseline"></a>Difference with a Baseline
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


CURRENTLY NOT SUPPORTED, WORK IN PROGRESS


<!-- ------------------------>
<!-- ------------------------>
## <a name="Cluster"></a>Local Execution vs. BC3 Cluster (SLURM)
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


CURRENTLY NOT SUPPORTED, WORK IN PROGRESS


<!-- ------------------------>
<!-- ------------------------>
## <a name="SDG_indicators"></a>SDG indicators
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


| Indicator | Target | Underlying Script | Key Dependencies |
| :--- | :--- | :--- | :--- |
| **SDG 1** | No Poverty | `sdg1_expenditure.R` | `gcamreport` |
| **SDG 2** | Zero Hunger | `sdg2_food_basket_bill.R` | GCAM Food Consumption & Prices |
| **SDG 3** | Good Health & Well-being | `sdg3_health.R` | `rfasst` |
| **SDG 6** | Clean Water & Sanitation | `sdg6_water_scarcity.R` | GCAM Basin Water Demands |
| **SDG 15** | Life on Land | `sdg15_land_indicator.R` | `Demeter`, Conda |



<!-- ------------------------>
<!-- ------------------------>
### <a name="SDG1"></a>SDG 1: Poverty
<!-- ------------------------>
<!-- ------------------------>


**Source**: https://www.un.org/sustainabledevelopment/poverty/

**Script**: [sdg1_expenditure.R](https://github.com/bc3LC/gcamreport/blob/gcam-core/R/functions.R#L1777)

To measure the impact of mitigation pathways on poverty, we focus on household expenditures on residential (home) energy and food relative to their average income, reflecting the relative pressure on households to meet their basic needs. 

This indicator is calculated for all representative households globally, which are based on 10 income deciles for each of the 32 socioeconomic regions in GCAM (or 66 socioeconomic regions in GCAM-Europe), combining to a total of 320 (respectively 660) representative households. We then take the average value for all representative households weighted by the relative population in each group. While this includes results for households that do not suffer poverty, the quantification of “absolute poverty” is subjective, and we preferred to include relative poverty in the equation. The highest values in terms of expenditures, as well as variation between scenarios, will be found in the poorest households which have the smallest denominator (i.e., income), and hence these households will mainly drive the outcome of this indicator. For the interpretation of the indicator, one should thus wary that small global variations may include significant impacts in the poorest households.

The following formula describes the calculation of the income expenditure share required for basic needs (IESBN):

$$
IESBN_{t} =
\frac{
    \sum_r \sum_h 
    \left(
        \frac{E_{t}}{I_{t}} + \frac{F_{t}}{I_{t}}
    \right) \cdot pop_{t}
}{
    \sum_r \sum_h pop_{t}
}
$$

Where $E$ reflects home energy expenditure, $F$ food expenditure, $I$ income, $r$ the socioeconomic regions in GCAM, $h$ the representative households (income deciles) within those regions, $pop$ the population of the representative household in the region, and $t$ the model simulation year.

Note: it is reported through `gcamreport(desired_variable = "Expenditure*")`.


<!-- ------------------------>
<!-- ------------------------>
### <a name="SDG2"></a>SDG 2: Zero hunger
<!-- ------------------------>
<!-- ------------------------>

**Source**: https://www.un.org/sustainabledevelopment/hunger/

**Script**: [sdg2_food_basket_bill.R](https://github.com/bc3LC/gcamsdg/blob/main/R/sdg2_food_basket_bill.R)

**Description**: 

In this implementation, SDG 2 is represented as the (avoided) per capita food basket bill. The calculations estimate the annual regional expenditure by a median consumer. Specifically, for each period $t$ and region $r$, all food items are aggregated into *Staples* and *Non-Staples*. Then, the total consumption of each group has been multiplied by its price, as described in the equation below:

$$FoodExpenditurePC_{t,r} = \sum_{fs\ in\ foodStapleItems} ConsumptionPC_{fs,t,r} \cdot PricePCStaples_{t,r} \ + $$

$$\sum_{fc\ in\ foodNonStapleItems} ConsumptionPC_{fn,t,r} \cdot PricePCNonStaples_{t,r}$$


<!-- ------------------------>
<!-- ------------------------>
### <a name="SDG3"></a>SDG 3: Ensure healthy lives and promote well-being for all at all ages
<!-- ------------------------>
<!-- ------------------------>

**Source**: https://www.un.org/sustainabledevelopment/health/

**Script**: [sdg3_health.R](https://github.com/bc3LC/gcamsdg/blob/main/R/sdg3_health.R)

**Description**: 

In this implementation, SDG 3 reports two complementary metrics across alternative scenarios using [`rfasst`](https://github.com/bc3LC/rfasst) (Sampedro et al., 2022), an R tool that emulates the TM5-FASST source-receptor model (Van Dingenen et al., 2018):

1. **Population-Weighted Pollutant Concentrations ($Conc$):** Estimates the average concentrations ($\mu g/m^3$ for $\text{PM}_{2.5}$ and $ppb$ for $\text{O}_3$) to which the population is exposed in each region and year.
2. **Premature Mortalities ($Mort$):** Quantifies health impacts attributable to long-term exposure to fine particulate matter ($\text{PM}_{2.5}$) and tropospheric ozone ($\text{O}_3$).

Premature mortality ($Mort$) for cause $c$, year $t$, region $r$, and pollutant $j$ is calculated using the Population-Attributable Fraction (PAF) approach:

$$Mort_{c,t,r,j} = mo_{c,r,j} \cdot \left( \frac{RR_{c,j} - 1}{RR_{c,j}} \right) \cdot Pop_{t,r}$$

Where $mo_{c,r,j}$ is the cause-specific baseline mortality rate derived from Global Burden of Disease (GBD) data and WHO projections; $RR_{c,j}$ is the relative risk of death attributable to changes in population-weighted mean pollutant concentration ($Conc$), based on Integrated Exposure-Response (IER) functions for $\text{PM}_{2.5}$ (Stanaway et al., 2018) and concentration-response functions for $\text{O}_3$ (Jerrett et al., 2009); and $Pop_{t,r}$ is the exposed population (subdivided by age group where applicable).

Health outcomes are evaluated across six distinct causes: stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory infections (ALRI), lung cancer (LC), and Type II diabetes mellitus (DM).

**References SDG3**:

- Burnett R T, Pope C A III, Ezzati M, Olives C, Lim S S, Mehta S, Shin H H, Singh G, Hubbell B, Brauer M, Anderson H R, Smith K R, Balmes J R, Bruce N G, Kan H, Laden F, Prüss-Ustün A, Turner M C, Gapstur S M, Diver W R and Cohen A 2014 An Integrated Risk Function for Estimating the Global Burden of Disease Attributable to Ambient Fine Particulate Matter Exposure Environmental Health Perspectives Online: http://ehp.niehs.nih.gov/1307049/
- Jerrett M, Burnett R T, Pope III C A, Ito K, Thurston G, Krewski D, Shi Y, Calle E and Thun M 2009 Long-term ozone exposure and mortality New England Journal of Medicine 360 1085–95
- Sampedro J, Khan Z, Vernon C R, Smith S J, Waldhoff S and Dingenen R V 2022 rfasst: An R tool to estimate air pollution impacts on health and agriculture Journal of Open Source Software 7 3820
- Stanaway J D, Afshin A, Gakidou E, Lim S S, Abate D, Abate K H, Abbafati C, Abbasi N, Abbastabar H and Abd-Allah F 2018 Global, regional, and national comparative risk assessment of 84 behavioural, environmental and occupational, and metabolic risks or clusters of risks for 195 countries and territories, 1990–2017: a systematic analysis for the Global Burden of Disease Study 2017 The Lancet 392 1923–94
- Van Dingenen R, Dentener F, Crippa M, Leitao J, Marmer E, Rao S, Solazzo E and Valentini L 2018 TM5-FASST: a global atmospheric source–receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants Atmospheric Chemistry and Physics 18 16173–211

<!-- ------------------------>
<!-- ------------------------>
### <a name="SDG6"></a>SDG 6: Clean water and sanitation
<!-- ------------------------>
<!-- ------------------------>

**Source**: https://www.un.org/sustainabledevelopment/water-and-sanitation/

**Script**: [sdg6_water_scarcity.R](https://github.com/bc3LC/gcamsdg/blob/main/R/sdg6_water_scarcity.R)

**Description**: 

The water module of GCAM is structured in 235 basins. The water withdrawals are defined as water diverted from a surface water or groundwater source. They are estimated for six major sectors: agriculture, electricity generation, industrial manufacturing, primary energy production, livestock and municipal uses. 

The water supply separates three distinct sources of fresh water: renewable water, non-renewable groundwater and desalinated water. Renewable water is water that is replenished naturally by surface runoff and subsurface infiltration and release. It is determined by the natural streamflow, baseflow, the total reservoir storage and the environmental flow requirement for each basins (Kim et al., 2016). 

We quantify physical water scarcity as the ratio of water withdrawals to renewable water supply for each basin (Birnbaum et al., 2022). The fraction of water demand relative to available renewable surface water supply has also been labelled ‘water stress index’ in previous studies (Byers et al., 2018). To derive a single estimate per scenario, we compute the average of the index weighted by the volume of renewable water withdrawal of each basin at baseline year (2015). The index is estimated with the following equations: 

$$I_{s,b}=W_{s,b}/S_{s,b}$$ 

and

$$I_{s} = sum_{b=1}^b (I_{s,b} * W_{s,b,2015}) / sum_{b=1}^b W_{s,b,2015}$$

Where $I$ is the water scarcity index per basin (dimensionless), $W$ is the water withdrawal in km3, $S$ is the renewable water supply in km3, $b$ are the basins, and $s$ are the scenarios. 

**References SDG6**:

- Birnbaum, A., Lamontagne, J., Wild, T., Dolan, F., & Yarlagadda, B. (2022). Drivers of Future Physical Water Scarcity and Its Economic Impacts in Latin America and the Caribbean. Earth’s Future, 10(8), e2022EF002764. https://doi.org/10.1029/2022EF002764
- Byers, E., Gidden, M., Leclère, D., Balkovic, J., Burek, P., Ebi, K., Greve, P., Grey, D., Havlik, P., Hillers, A., Johnson, N., Kahil, T., Krey, V., Langan, S., Nakicenovic, N., Novak, R., Obersteiner, M., Pachauri, S., Palazzo, A., … Riahi, K. (2018). Global exposure and vulnerability to multi-sector development and climate change hotspots. Environmental Research Letters, 13(5), 055012. https://doi.org/10.1088/1748-9326/aabf45
- Kim, S. H., Hejazi, M., Liu, L., Calvin, K., Clarke, L., Edmonds, J., Kyle, P., Patel, P., Wise, M., & Davies, E. (2016). Balancing global water availability and use at basin scale in an integrated assessment model. Climatic Change, 136(2), 217–231. https://doi.org/10.1007/s10584-016-1604-6


<!-- ------------------------>
<!-- ------------------------>
### <a name="SDG15"></a>SDG15: Life of land
<!-- ------------------------>
<!-- ------------------------>

**Source**: https://www.un.org/sustainabledevelopment/biodiversity/

**Script**: [sdg15_land_indicator.R](https://github.com/bc3LC/gcamsdg/blob/main/R/sdg15_land_indicator.R)

**Description**: 

The land module is structured around 384 distinct land-water regions, called LUTs, and provides outputs on land allocation for 43 land uses per LUT. It computes supply, demand, and land utilisation in various sectors, encompassing food, feed, fiber, forestry, and bioenergy production. The land uses are categorised into two categories, namely managed and unmanaged land. The latter includes unmanaged forests, unmanaged pasture, shrubland, grassland, tundra and other land uses (e.g., rock, ice and desert). 

The indicator reported for SDG15 is the net Potential Species Loss (PSL) indicator, which represents the number of plant and vertebrate species committed to extinction due to habitat loss through land use change (LUC) from unmanaged to managed land. The PSL indicator considers the current threat level faced by the species, the level of endemism, and the capacity of the species to adapt to this new habitat (Chaudhary et al., 2015; Chaudhary & Brooks, 2018). 

First, the land use projections from GCAM were downscaled with the Demeter model (Vernon et al., 2018) to increase the results’ resolution (0.5ºx0.5º) for the main land uses. These downscaled land use projections are then aggregated at the ecoregion level which is the spatial unit required to derive the biodiversity indicator.  (Chaudhary & Brooks, 2018). For each ecoregion and land use, the LUC is estimated as the area difference between 2020 and 2050. 

Second, the PSL is estimated from the ecoregion-based LUC projections and aggregated characterisation factors (CF). CFs provide PSL per unit area (ha) for five broad managed land uses (forestry, cropland, both irrigated and rainfed, pastureland, and urban land) under distinct management intensity levels for 804 terrestrial ecoregions. Extended Data Table 3 illustrates the matching between GCAM land uses and the land use types and management intensity of the CFs. 

The aggregated CFs reflect species loss across five taxa combined (mammals, birds, amphibians, reptiles and plants) and were obtained from Chaudhary & Brooks, (2018). The PSL indicator of each scenario is estimated with the following equations: 


$$LUC_{l,r} = A_{l,r,2050} - A_{l,r,2020}$$

$$PSL_{l,r} = LUC_{l,r} \times CF_{l,r}$$

$$PSL = \sum_l \sum_r PSL_{l,r}$$

Where $A$ is the area in ha, $LUC$ is the land use change between 2020 and 2050 in ha, $CF$ is the aggregated characterisation factor in species loss per ha, $PSL$ is the net potential species loss of the scenario, $l$ are the managed land uses, and $r$ are the ecoregions.  

**References SDG15**:

- Chaudhary, A., & Brooks, T. M. (2018). Land Use Intensity-Specific Global Characterization Factors to Assess Product Biodiversity Footprints. Environmental Science & Technology, 52(9), 5094–5104. https://doi.org/10.1021/acs.est.7b05570
- Chaudhary, A., Verones, F., de Baan, L., & Hellweg, S. (2015). Quantifying Land Use Impacts on Biodiversity: Combining Species–Area Models and Vulnerability Indicators. Environmental Science & Technology, 49(16), 9987–9995. https://doi.org/10.1021/acs.est.5b02507
- Vernon, C. R., Le Page, Y., Chen, M., Huang, M., Calvin, K. V., Kraucunas, I. P., & Braun, C. J. (2018). Demeter – A Land Use and Land Cover Change Disaggregation Model. Journal of Open Research Software, 6(1), 15. https://doi.org/10.5334/jors.208


<!-- ------------------------>
<!-- ------------------------>
## <a name="GCAM_SDG_studies"></a>GCAM SDG studies
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

- Moreno, J., Campagnolo, L., Boitier, B., Nikas, A., Koasidis, K., Gambhir, A., Gonzalez-Eguino, M., Perdana, S., Van de Ven, D.J., Chiodi, A. and Delpiazzo, E., 2024. The impacts of decarbonization pathways on Sustainable Development Goals in the European Union. Communications Earth & Environment, 5(1), p.136.

- Moreno, J., Van de Ven, D.J., Sampedro, J., Gambhir, A., Woods, J. and Gonzalez-Eguino, M., 2023. Assessing synergies and trade-offs of diverging Paris-compliant mitigation strategies with long-term SDG objectives. Global Environmental Change, 78, p.102624.

- Iyer, G., Calvin, K., Clarke, L., Edmonds, J., Hultman, N., Hartin, C., McJeon, H., Aldy, J. and Pizer, W., 2018. Implications of sustainable development considerations for comparability across nationally determined contributions. Nature Climate Change, 8(2), pp.124-129.


<!-- ------------------------>
<!-- ------------------------>
## <a name="How_to_contribute"></a>How to contribute
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

You are welcome to contribute to this project! Follow the steps below to facilitate the implementation:

1. Fork this repository.

2. Commit your modifications.

3. Open a Pull Request (PR) against the main target branch. Clearly describe the purpose of your modifications and outline the specific changes made. Ensure there are no merge conflicts and that all automated tests pass successfully.

4. Set @klau506, @jonsampedro and/or @LinoHub as reviewers (or mention them in the PR description text).

5. Once everything is tested, we will merge the PR for you.



<!-- ------------------------>
<!-- ------------------------>
## <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

TODO
