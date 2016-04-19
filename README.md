## A perceptual bias in the blind spot

### Overview
This github documents the code and data (bioRxiv forthcoming).


### Code

#### Prepare:
You need R and several packages (ggplot, ddply, rstan,...). So far no exhaustive list has been compilled.

Do this to clone the repository:
'''git clone https://github.com/behinger/blind_spot_psychophy'''

If you do not want to rerun the bayesian mixed model, go to https://osf.io/wphbd/files and download the stan files into 'blind_spot_psychophy/cache/stan'. The behavioural data is included in the github (see the folder 'export' for a compiled description of the fields).

#### Data as CSV
The data have been exported as csv [here](export/2016-01-28_allDat.csv) accompanied by a [description of fields ](scripts/bs_export_csv.html)


#### Blind Spot Characteristics
To run the analyses about the sizes of the blind spot see [here](scripts/bs_blindSpot.Rmd) or the [*knitr'ed* file](scripts/bs_blindSpot.html)

#### Bayesian Mixed Model Analysis
The analyses of the mixed models are somewhat extensive. There are two main files [this](scripts/bs_stan_matrix.R) for the analysis of all experiments, and [this](scripts/bs_stan_singleExperiment_analysis.Rmd) for the analysis of the single experiments. The first script calls the second one.
A *kniter'ed* version can be found [here](scripts/bs_stan_matrix.html).

The stan mixed models can be found [here (logistic) ](scripts/matrix_model.stan) and [here (reaction time)](scripts/matrix_model_RT.stan)

#### Which subjects to remove
[this Rmd](scripts/bs_which_to_remove.Rmd) ([or the *kniter'ed* version](scripts/bs_which_to_remove.html)) makes statistics of which subjects were removed and which the number of trials
