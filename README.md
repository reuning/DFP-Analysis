# DFP-Analysis
The purpose of this repository is to provide some transprency for blogs I've written for [Data for Progress](https://www.dataforprogress.org/). Because of data limitations not everything can be shared, but for what can be shared I will share it here. As I get chance I will see what I can make available from previous analysis. 

## Overview 
Each blog is a separate folder. Included also are the DPF plot themes. To run this though you need to have three fonts: FuturaBT-Heavy, Montserrat-Thin, and Montserrat-Regular.

Prior to making this repository I put the analysis for two other blogs in other repositories.
1. The Analysis of the 2018 Early Voters. [Repository](https://github.com/reuning/2018-live-poll-results)
2. Estimates of 2020 Democratic Candidate Ideology. [Repository](https://github.com/reuning/2020-Candidate-Positions)


## Blogs 
1. [MaxDiff Estimates of Candidate Support by Demographic Group](https://www.dataforprogress.org/blog/2019/6/17/a-better-measure-of-candidate-support-by-demographic-groups). [Code](MaxDiff_Candidates)
2. [6 Apparent Political Clusters](https://wthh.dataforprogress.org/blog/2018/11/25/the-six-apparent-political-clusters). [Code](WTHH_Classes)

## Packages used (generally)
Most analysis is done in [RStan](https://github.com/stan-dev/stan) with plots made in [ggplot2](https://ggplot2.tidyverse.org/). In addition this repostiory uses [here](https://github.com/r-lib/here) to help with working directories. 

## Data
Data is almost always pulled directly from DFP servers and so is not included in this repository. 



