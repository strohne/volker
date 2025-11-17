# ChatGPT Adoption Dataset CG-GE-APR23

A small random subset of data from a survey about ChatGPT adoption. The
survey was conducted in April 2023 within the population of German
Internet users.

## Usage

``` r
chatgpt
```

## Format

### `chatgpt`

A data frame with 101 rows and 22 columns:

- case:

  A running case number

- sd\_:

  Columns starting with sd contain sociodemographics of the respondents.

- adopter:

  Adoption groups, inspired by Roger's innovator typology.

- use\_:

  Columns starting with use contain data about ChatGPT usage in
  different contexts.

- cg_adoption\_:

  A scale consisting of items about advantages, fears, and social
  aspects. The scales match theoretical constructs inspired by Roger's
  diffusion model and Davis' Technology Acceptance Model.

- cg_activities:

  Text answers to the question, what the respondents do with ChatGPT.

- cg_act_write:

  Manual content analysis of cg_activities: Does the activities involve
  generating text, code or other artifacts?

- cg_act_test:

  Manual content analysis of cg_activities: Does the activities involve
  testing, experimenting or playing around?

- cg_act_search:

  Manual content analysis of cg_activities: Does the activities involve
  searching for information, advice or inspiration?

## Source

Communication Department of the University of Münster
(<gehrau@uni-muenster.de> and <jakob.juenger@uni-muenster.de>).

## Details

Call codebook(volker::chatgpt) to see the items and answer options.
