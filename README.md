# COVID19 [COVID-19 modeling]

This modifies an SEIR model to include numbers for hospitalization and deceased.  It's simply a set of ODEs.  Details given in the ModelNotes.docx file, in docx form because that was easiest for my team to have access to.

## model_v1.0

The original, crappy, version is model.R.  Don't use this one.  The dynamical system is deeply flawed.

## model_v2.0

The one to use is model\_master.R, which sets up and runs model\_configuration.R (configuring the model), model\_initialization.R (initializing the model), model\_dynamic\_core.R (running the actual model dynamic core), and model_out.R (writing the model output).

## model_v3.0

This directory is admittedly messy.  Current contents:
- DetermineStateSpace.R -- for running the dynamical model in v2.0 but for a batch over a specified state space in R0 and the intervention reduction of R0.
- DetermineDistance.R -- To be used on the output of DetermineStateSpace.R.  Finds the nearest neighbor in the state space to observed deaths attributed to COVID.
- DetermineRcritStateSpace.R -- for running the dynamical model in v2.0 but for a batch over a specified state space in the ratio of ICU to infectious.
- DetermineCritDistance.R -- To be used on the output of DetermineRcritStateSpace.R.  Finds the nearest neighbor in the state space to observed ICU cases attributed to COVID (including PUIs).

Workflow example:  Use DetermineStateSpace.R and DetermineDistance.R to find the most likely R0.  Then use DetermineRcritStateSpace.R and DetermineCritDistance.R to determine the most likely critical rate and intervention reduction.  Rerun DetermineDistance.R to reproduce figures with improved Rcrit.