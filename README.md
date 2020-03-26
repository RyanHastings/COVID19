# COVID19
COVID-19 modeling

This modifies an SEIR model to include numbers for hospitalization and deceased.  It's simply a set of ODEs.  Details given in the ModelNotes.docx file, in docx form because that was easiest for my team to have access to.

The original, crappy, version is model.R.  Don't use this one.

The one to use is model_master.R, which sets up and runs model_configuration.R (configuring the model), model_initialization.R (initializing the model), model_dynamic_core.R (running the actual model dynamic core), and model_out.R (writing the model output).
