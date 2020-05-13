# COVID19 [COVID-19 modeling]

This modifies an SEIR model to include numbers for hospitalization and deceased.  It's simply a set of ODEs.  Details given in the ModelNotes.docx file, in docx form because that was easiest for my team to have access to.

## model_v1.0

The original, crappy, version is model.R.  Don't use this one.  The dynamical system is deeply flawed.

## model_v2.0

The first one to use is model\_master.R, which sets up and runs model\_configuration.R (configuring the model), model\_initialization.R (initializing the model), model\_dynamic\_core.R (running the actual model dynamic core), and model_out.R (writing the model output).

This directory also contains an attempt to cast the SEIR equations in integral form, which turned out to be an unworkable failure.  The model becomes too deterministic and rigid and doesn't accurately reflect reality.

There are a number of groups of code starting with ModelThePlan.  This is specifically to model Governor Holcombe's plan to re-open Indiana, both on the statewide and district level.  The ones including Scenarios refer to specific scenarios designated by the modeling team.


## model_v3.0

This directory is admittedly messy.  Previous contents:
- DetermineStateSpace.R -- for running the dynamical model in v2.0 but for a batch over a specified state space in R0 and the intervention reduction of R0.
- DetermineDistance.R -- To be used on the output of DetermineStateSpace.R.  Finds the nearest neighbor in the state space to observed deaths attributed to COVID.
- DetermineRcritStateSpace.R -- for running the dynamical model in v2.0 but for a batch over a specified state space in the ratio of ICU to infectious.
- DetermineCritDistance.R -- To be used on the output of DetermineRcritStateSpace.R.  Finds the nearest neighbor in the state space to observed ICU cases attributed to COVID (including PUIs).

Workflow example:  Use DetermineStateSpace.R and DetermineDistance.R to find the most likely R0.  Then use DetermineRcritStateSpace.R and DetermineCritDistance.R to determine the most likely critical rate and intervention reduction.  Rerun DetermineDistance.R to reproduce figures with improved Rcrit.

Since this, much new code has been added.  First, the previous code was deprecated by new code that fits death, hospital bed, and ICU curves simultaneously:  Fit3curves.R and District3Fit.R.  Those work off of input from DetermineStateSpace.R and DetermineDistrictStateSpace.R.  Additional code including PhaseTwo in the title is specifically for trying to make fits to Stage Two of Governor Holcombe's plan.  These use the R0 and Phase One lift reductions found by previous fits (Fit3curves.R and District3Fit.R) and add new dimensions for Stage Two lift reductions.  Corresponding to these is Fit3curvesPhaseTwo.R and District3FitPhaseTwo.R

There was also an attempt to implement the integral version of the SEIR equations, which was abandoned.  Those begin with IntegralModel.

The ModelThePlanDistricts code really should be under model\_v3.0.  Not sure what it's doing here.