# Framework description

The extrapolation process was automated to have a reproducible workflow and was developed in **R** to avoid manually fitting each model (Figure S1).  
This workflow ensures **consistency, reproducibility, and scalability**.  

It uses two main input files:  

- `CompoundData.xlsx` — includes compound-specific properties such as physicochemical values, fraction unbound, and clearance for all species.  
- `Rodent_PBPK_TimeValuesData.xlsx` — includes in vivo PK data and administration protocols for each study.  

Those two input files are situated in the `Data`folder.

The workflow output includes:  

- Metrics tables for each extrapolation model  
- Comparative plots  
- An automated HTML report  

Intermediate outputs (such as parameter fitting results) are also generated for validation purposes.  

Each time the framework in run, a new folder containing the date and hour is created in the `Results`folder to store all outputs.

The main steps of the workflow are handled by `Workflow.R`, which calls a series of helper scripts to execute specific tasks:  

1. **CreateModelParameterSheet.R**  
   Converts `CompoundData.xlsx` into a new Excel file that includes all necessary model parameters (e.g., fraction unbound, clearance values) formatted for the `esqLabsR` package.  

2. **CreatePIScenarios.R**  
   Generates an Excel sheet mapping model parameter sheets to PK-Sim project files (`*.pkml`), defining the scenarios needed for parameter identification.  

3. **RunPICompleteModel.R**  
   Performs parameter identification, fitting lipophilicity and clearance (plasma or GFR value for renal cleared compounds) values for each compound and scenario using the **BOBYQA** optimization algorithm available in the **OPSuite’s Parameter Identification R package**.  
   It adjusts administration settings, maps in vivo data, and outputs fitted parameter values and updated PK-Sim models.  

4. **CreateExtrapolationScenarios.R**  
   Creates an Excel file defining all extrapolation scenarios, linking the correct PK-Sim models and updating parameters based on the extrapolation type and target species.  
   It calculates scaling factors for clearance when in vitro data is available for both origin and target species.  

5. **RunExtrapolation.R**  
   Runs simulations for each extrapolation scenario and computes metrics such as AUC ratios and relative changes.  
   The script also generates plots to visualize the results of the individual extrapolations.  

6. **GenerationFiguresMetric.R**  
   Aggregates metrics across compounds and species, producing plots such as boxplots of AUC ratios grouped by extrapolation type.  

7. **ReportGeneration.qmd**  
   Compiles all results into an automated HTML report that includes metrics, plots, and summaries of each step.  

All of those helper scripts are situated in the `Code`folder. 

---

**Figure S1.** Visual representation of the R workflow for the species extrapolation scenarios  

---

# Flexibility of the framework

The workflow was designed to be easily adaptable to changes in compounds, species, or datasets.  
Modifications can be made directly in the input files (`CompoundData.xlsx` and `Rodent_PBPK_TimeValuesData.xlsx`) and `Workflow.R`.  

For example:  

- **Adding or removing a compound:**  
  - To remove a compound, update the list of compounds in `Workflow.R`.  
  - To add a compound, include its details in `CompoundData.xlsx` and its observed in vivo data in `Rodent_PBPK_TimeValuesData.xlsx`.  

- **Adding or removing a species:**  
  - To remove a species, update the species list in `Workflow.R`.  
  - To add a species, update `CompoundData.xlsx` with the compound information for the new species, include in vivo data in `Rodent_PBPK_TimeValuesData.xlsx`, and export generic PK-Sim models for each partition coefficient method for the new species.  

- **Adding or removing a dataset:**  
  - To modify datasets, add or remove the relevant sheet in `Rodent_PBPK_TimeValuesData.xlsx`.  

---

These features make the workflow **scalable and reproducible**, enabling its application to diverse scenarios in **eco-toxicological modeling**.  