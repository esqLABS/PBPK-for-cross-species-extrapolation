This setup allows to easily modify the data compounds or species, by modifying accordingly either the Workflow.R, CompoundData and Rodent_PBPK_TimeValuesData files.
For example: 

To remove/add a compound:
-To remove a compound, we only need to change the defined compounds used in Workflow.R
-To add a compound, we need to:
	o Add it to the defined vector of compound used in the Workflow.R
	o Add the compound information in the CompounData.xlsx file
	o Add the observed in vivo data in Rodent_PBPK_TimeValuesData.xlsx file
•	To remove/add a species:
-	To remove a species, we only need to change the defined compounds used in Workflow.R

-To add a species, we need to:
	o Add it to the defined vector of species in the Workflow.R
	o Add the all the compound information for the new species in the CompounData.xlsx file
	o Add the observed in vivo data in Rodent_PBPK_TimeValuesData.xlsx
	o Export a generic pkml simulation with the new species for each partition coefficient method

-To remove or add a dataset, we simply have to delete or add a new sheet to the: Rodent_PBPK_TimeValuesData.xlsx file, following the same structure as the other sheets.


For each step of the workflow some helper functions or scripts have been written and are called from the within Workflow.R:

	o CreateModelParameterSheet.R: defines the createModelParameterSheet function used to convert the CompoundData.xlsx file to another excel with all model parameters needed (compound phys-chem, fraction unbound and clearance)  which is compliant with esqlabsR package framework

	o CreatePIScenarios.R: defines the createPIScenarios function used to generate an excel with all the scenarios needed (mapping of model parameter sheets and pkml file) for parameter identification which is compliant esqlabsR package framework.

	o RunPICompleteModel.R: defines the runPICompleteModel function used to run a parameter identification fitting the lipophilicity and clearance (either plasma or GFR for renally cleared compounds) on all the scenarios defined in the previous step. It also take care of adjusting the administration and mapping the in vivo data correctly. It also outputs objective function value, fitted parameters values and plots and simulations pkml if needed.

	o CreateExtrapolationScenarios.R: defines the createExtrapolationScenarios function used to create an excel compliant with the esqlabsR package framework with all the scenarios needed for the extrapolation step by selecting the correct pkml, updating values (lipophilicity and clearance) depending on the extrapolation chosen and species from and to considered. For the clearance extrapolation a scaling factor between the in vitro and fitted specific clearance was calculated and applied to the other species if both had measures for the same in vitro assay.

	o RunExtrapolation.R: defines the runExtrapolation function that run simulation for each extrapolation scenario, and calculates and returns various extrapolations metrics and also output comparison plots of the different individual extrapolations. 

	o GenerationFiguresMetric.R: scripts that compares the metrics on different extrapolations across compound and species to produce different plots, such as boxplot of AUC depending on type of extrapolation.

	o ReportGeneration.qmd: generate a html report that gather all the different results form the various steps
