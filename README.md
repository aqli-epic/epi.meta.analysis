
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Epidemiological Studies: Meta Analysis

<!-- badges: start -->
<!-- badges: end -->

This analysis is our best first attempt to capture all epidemiological
cohort studies (\>3 people; max cohort size: 73 million), that are
relatively long term ((\> 1 years, max study duration: 36 years) and
measure the impact of ambient PM<sub>2.5</sub>, PM<sub>10</sub>, TSP,
Ultra Fine Particulate Matter on Mortality (cause-specific, all cause,
pre-mature)/Life Expectancy published between 1993 and present\*,
findable in available peer-reviewed literature. Hereafter, referred to
as **AQ epi studies** for short.

We are seeking to make this analysis as current, complete and error-free
as possible and view it as a continual work in progress. We would
appreciate the air quality community’s comments, corrections, and
suggestions. Please contact **<aqli-epic@uchicago.edu>** or leave a
comment in [this](https://github.com/aqli-epic/epi.meta.analysis) GitHub
repository.

## Purpose

The purpose of this analysis is to understand the landscape of
epidemiological research on the relationship between PM<sub>2.5</sub>,
PM<sub>10</sub>, TSP, Ultrafine Particulate Matter and Mortality (all
types, as specified above)/Life Expectancy and to surface demographic,
geographic, or other trends that may exist in the current state of
literature. While the overall arc of the relationship between these
pollutants and human health is clear to take action, such trends can
help the field reflect on itself, take stock of any biases or gaps – and
point toward future research and policy opportunities.

## Why do epidemiological studies on air pollution and mortality matter?

While global estimates of air pollution’s toll on public health vary,
they all point in the same direction: air pollution poses one of the
largest health risks on the planet to humans
\[[Paper1](https://www.pnas.org/doi/10.1073/pnas.1300018110),
[Paper2](https://www.pnas.org/doi/full/10.1073/pnas.1616784114),
[Paper3](https://pubs.acs.org/doi/pdf/10.1021/acs.estlett.8b00360),
[Paper4](https://pubs.acs.org/doi/pdf/10.1021/acs.estlett.8b00360)\].
Epidemiological studies on air pollution and mortality help us
understand the burden of air pollution on human health at global,
national and regional levels. According to [Vahlsing and Smith
(2012)](https://link.springer.com/content/pdf/10.1007/s11869-010-0131-2.pdf),
these sorts of studies can also help countries take policy action,
pushing forward and shaping national-level ambient air quality
standards.

The burden of air pollution across the world is also not uniform. While
97.3 percent of the world population is out of compliance with the
latest WHO annual PM<sub>2.5</sub> guideline of 5 µg/m³, there is huge
variation in the quality of air one breathes.

## Inclusion/Exclusion Criteria for studies

-   The underlying [****analysis
    dataset****](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996)
    for this meta analysis focuses only on those papers that study the
    link between **PM<sub>2.5</sub>**, **PM<sub>10</sub>**, **TSP**,
    **Fine Particulate Matter** and ****Mortality/Life Expectancy****.
    In addition to the analysis dataset, there is a [****master
    dataset****](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=0)
    that expands on the analysis dataset to include other useful
    information. The idea behind the master dataset is to record any
    additional details (whether additional facts about the paper, or
    details on other pollutants studied in the paper) that will not be a
    part of the main data analysis exercise.

-   The analysis dataset excludes the following types of papers:
    meta-analysis, unpublished papers, papers studying the effects of
    indoor air pollution on Mortality/Life Expectancy, papers
    forecasting future air pollution/life expectancy/mortality. But, the
    master dataset lists all of these studies.

-   In Multi-Country (pooled) studies, one entry (one row in the
    analysis dataset) is recorded for each country in papers where
    country level data is available. There are some pooled studies where
    country level data is not available, but rather data is available
    for a custom region (e.g. South-East Asia), such pooled studies are
    excluded from the analysis dataset. But, the master dataset still
    lists all of these additional studies for reference.

-   In cases where the same cohort is studied by different research
    groups at different point in time/using different methods: we have
    included all of those studies.

-   Papers studying the health effects of pollution segregated by
    sectors (source apportionment type studies) are excluded from this
    analysis.

-   All papers studying the measure the impact of pollutants on DALY’s,
    have not been included in this analysis.

## Other important points

-   In papers, where the minimum PM<sub>2.5</sub> concentration was not
    reported, we have assumed the lowest available percentile data
    available on PM2.5 concentration as the minimum concentration.

-   -   In many papers, only one of the mean PM<sub>2.5</sub> or
        PM<sub>2.5</sub> range is reported but not both. In these cases,
        wherever the data is not available (whether it is mean PM2.5 or
        PM2.5 range) we have recorded a NA. Apart from this, anywhere we
        couldn’t find data, we have recorded a NA.

-   In multi-pollutant studies where different pollutants have been
    studied over different periods of time, we have chosen the specific
    time period that corresponds to the PM<sub>2.5</sub> pollutant (in
    cases where we PM<sub>2.5</sub> is not present, we have recorded the
    time period corresponding to one of the other pollutants).

-   We have recorded one mean PM<sub>2.5</sub> value for each paper.
    But, there are papers where more than one mean PM<sub>2.5</sub>
    value is reported (for example, one for the male group and one for
    the female group). In these cases, we have picked one value from the
    ones that are available.

-   In some papers, the exact start and/or end year of a study is
    unclear. In these cases, we have mostly chosen the first instance of
    the multiple “study duration ranges” (depending on multiple
    plausible interpretations of when the final follow up ended) the
    paper.

-   In many papers, the upper limit or lower limit for age is not
    precisely specified. In these cases, we have recorded a NA. For
    example, there are papers where the upper limit category for age is
    85+. In these cases, although we know the upper limit category, we
    still don’t know the upper limit of the age, which could be 90, 95,
    100, etc.

-   In places, where the cohort size is specifed in terms of “number of
    regions” (for example, 17 districts) and not “number of people”, we
    have recorded a NA in the cohort size column.

-   Under the “methods” column (in the master dataset), we have only
    mentioned a subset of methods that were used to carry out different
    parts of the study. Authors may have used other methods than those
    mentioned in our meta analysis.

-   Different graphs are generated using different subsets of the
    [analysis
    dataset](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996).
    Most of them are PM<sub>2.5</sub> specific (our main focus), others
    include all pollutants (PM<sub>2.5</sub>, PM<sub>10</sub>, TSP,
    Ultrafine Particles). The type of papers used to generate a given
    graph is specified within the graph and/or the accompanying text.

## Results

 

### PM<sub>2.5</sub> exposure range and the Global Population Distribution

 

<img src="man/figures/README-mean_pm2.5_population_graph_calcs-1.png" width="100%" /><img src="man/figures/README-mean_pm2.5_population_graph_calcs-2.png" width="100%" />

 

-   In total 103 AQI epi studies were included in the final [analysis
    dataset](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996).

-   12.7 percent of the world population, or 962.9 million people, live
    in areas where the annual average PM<sub>2.5</sub> pollution is
    greater than 50 µg/m³. But, only 10.6 percent (7 PM<sub>2.5</sub>
    specific studies) of the total studies, have been performed in these
    highly polluted parts of the world. These highly polluted areas are
    areas where the average PM<sub>2.5</sub> pollution is at least 10
    times the WHO PM<sub>2.5</sub> safe guideline of 5 µg/m³.

-   Approximately 5.5 percent of the world population (420.4 million
    people) live in the most severely polluted parts of the world, where
    annual average PM<sub>2.5</sub> pollution is upwards of 75 µg/m³ (at
    least 15 times the WHO safe guideline). In these most severely
    polluted parts of the world, 2 AQ epi studies have been performed .

-   Most of the AQ epi studies (75.8 percent of the total, or 50
    studies) performed so far, are concentrated in areas where the
    average PM<sub>2.5</sub> concentration is in the 0-25 µg/m³ range.
    People living in these areas (59.2 percent of the world population)
    are breathing air that is much less polluted relative to the people
    living in the most polluted parts of the world (as seen above). But,
    even in the 0-25 µg/m³ bucket, anyone living above 5 µg/m³, is out
    of compliance with the WHO PM<sub>2.5</sub> guideline.

-   Note: In making the above 2 graphs, multiple countries of a pooled
    study are counted as separate studies.

 

### Geographic Distribution of Studies

<img src="man/figures/README-geographic_dist_studies_calcs-1.png" width="100%" />

-   49 (27.8 percent) studies were performed in the USA and Canada.

-   105 studies (59.7 percent) were performed in the USA, Canada and
    Europe combined. Remaining 40.3 percent studies are scattered across
    China, Japan, Iran, Hong Kong, Taiwan, Brazil, India, Australia,
    Chile, Colombia, Mexico, South Africa, South Korea, Thailand,
    Mongolia, Bangladesh, Bhutan, North Korea, Philippines, Cambodia,
    Laos, Myanmar, Vietnam, Pakistan, Nepal, Sri Lanka.

-   In terms of the total number of studies conducted, Asia dominates
    the rest of the continents (with 36.4 percent of total studies).
    This is closely followed by Europe (with 31.8 percent of total
    studies) and North America (with 28.4 percent of total studies).
    South America, Africa, Oceania combined, have only seen 6 studies.

-   Note: In making the above graph, multiple countries of a pooled
    study are counted as separate studies. Also, the above graph
    represents all studies (i.e. includes all pollutants and is not just
    PM<sub>2.5</sub> specific).

-   **Note for discussion: In the total count of studies, each country
    in a multi-country is counted as separate studies.**

 

### AQ Epi studies over time

 

<img src="man/figures/README-aq_epi_studies_over_time_graph-1.png" width="100%" />

-   In most of the 90’s and early 2000’s, the rate of AQ epi studies
    publishing was around 1 to 2 studies per annum on average.

-   Post 2009, there has been a noticeable increase in the overall
    volume of AQ epi studies published.

-   **Note for discussion: In making the above graph, multiple countries
    of a pooled study are *not* counted separately, i.e. a pooled
    multi-country study is counted as a single study. Also, the above
    graph represents all studies (i.e. includes all pollutants and is
    not just PM<sub>2.5</sub> specific).**

### Distribution of duration of study by continent

 

<img src="man/figures/README-dist_duration_study_calcs-1.png" width="100%" />

-   All of the “really” long term studies (\> 25 years) ever conducted
    are concentrated only in Canada, USA, France, Australia, Brazil,
    Chile, China, Colombia, Czech Republic, Estonia, Finland, Greece,
    Italy, Japan, Mexico, Portugal, South Africa, South Korea, Spain,
    Sweden, Switzerland, Taiwan, Thailand, United Kingdom.

-   Asia, Europe, North America have seen a relatively large number of
    studies (across a wide range of study durations). In contrast to
    this, in South America, Africa, Oceania combined, only 6 studies
    have been performed.

-   **Note for discussion: In making the above graph, multiple countries
    of a pooled study are counted separately. Also, the above graph
    represents all studies (i.e. includes all pollutants and is not just
    PM<sub>2.5</sub> specific).**

### Major takeaways

-   27.8 percent of the total number of studies are concentrated in the
    USA and Canada alone.

-   Aproximately 5.5 percent of the world population, or 420.4 million
    people live in areas where PM<sub>2.5</sub> pollution concentrations
    are upwards of 75 µg/m³ (at least 15 times the WHO safe guideline).
    In these parts of the world, 2 AQ epi studies have been performed.

-   All of the “really” long term studies (\> 25 years) ever conducted
    are concentrated only in Canada, USA, France, Australia, Brazil,
    Chile, China, Colombia, Czech Republic, Estonia, Finland, Greece,
    Italy, Japan, Mexico, Portugal, South Africa, South Korea, Spain,
    Sweden, Switzerland, Taiwan, Thailand, United Kingdom.

-   South America, Africa, Oceania combined, have only seen 6 studies.

-   In the last decade, there has been a noticeable increase in the
    total number of AQ epi studies performed. But, most of these new
    studies are concentrated in the low pollution concentration ranges
    of 0-25 µg/m³. The high pollution concentration ranges remain poorly
    explored.

### Conclusion

All studies included in this analysis point to the same overall
picture:air pollution is a serious health threat. The existing state of
scientific literature on air pollution and health is clear that air
pollution’s impact on health is well-established and taking action in a
polluted environment should not be delayed in order to complete
multi-year large cohort epidemiological studies in an area, even if
there has not been a prior study in that particular geography. That
said, it is important for the field of air quality epidemiolgy to
understand the contours of its current research landscape to most
effectively identify directions for future research and deploy limited
resources.

 

### Methodology

Through a comprehensive and ongoing\* literature review, we are making
an attempt at creating an exhaustive public listing of all the
epidemiological studies out there (that we could find) that examine the
relationship between PM<sub>2.5</sub> and Life Expectancy/Mortality.

For each study, we record data on key defining features, such as:
Geography, Cohort Size, Study Duration, PM<sub>2.5</sub> exposure range,
etc. Then we used
[this](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996)
analysis dataset to carry out a meta-analysis, results of which are
detailed in the Results section above.

We are seeking to make this analysis as current, complete and error-free
as possible and view it as a continual work in progress. We would
welcome the air quality community’s any comments, corrections, andor
suggestions. Please contact **<aqli-epic@uchicago.edu>** or leave a
comment in this GitHub repository.

### Limitations (To be discussed)

 

### How can you (the community) help in improving this analysis?

-   Add to the [****analysis
    dataset****](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996)
    and and grow the epi database:

-   If you know of other papers that: (a) are attempting to study the
    link between PM<sub>2.5</sub> and Life Expectancy/Mortality and (b)
    are not included in this analysis: Please leave a comment in the
    [****analysis
    dataset****](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996),
    providing a link to the paper. You can also write to us at
    <aqli-info@uchicago.edu> (with the link to the paper mentioned in
    the email).

    -   As a next step, we’ll go through your submission, and if it fits
        our inclusion criteria, we will update the underlying analysis
        dataset and re-render the entire blog, so that it represents the
        most up to date data and figures.

    -   If you are unsure (given the inclusion criteria) about whether a
        particular paper (that you want to post) should be posted on
        not, we encourage you to post it and let us worry about the
        inclusion/exclusion bit.

-   In case you have comments on some aspects of a paper/if you find any
    errors, please leave a comment in the [****analysis
    dataset****](https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996)
    on the cell (tagging <aqli-info@uchicago.edu>, using \*\*@\*\*
    symbol) where the error is found or write to us at
    <aqli-info@uchicago.edu>, detailing the error and its location (cell
    address on the sheet).

 

 

<hr>

### References

 

 

<hr>

### Other Graphs and Interactive Summary Dashboard

To further explore these graphs and more in an interactive setup, visit
the [AQ Epi
dashboard](https://aarsh.shinyapps.io/aqli-epic-epi-meta-analysis/).
