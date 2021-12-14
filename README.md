# HMS520 Final project: refactoring health financing and MDR-TB analysis 

Refactored lmer_exp.R and tb_funding_analysis.R
Problems:
- Intermixed pipeline of reading in, cleaning and merging data, making visualizations, and running models
- Unnecessary datasets and variables no longer of interest
- Poorly organized, causing difficulty in adding new data and modifying or adding new visualizations or analyses
- Poor readibility

New scripts:
- tb_data_prep_refactor.R: read in datasets, merge, clean, and save prepped dataset
- tb_model_refactor.R: read in prepped dataset, run mixed linear effects model, and save model results
- tb_visualization_refactor.R: read in prepped dataset and model results, create visualizations, and save figures
- tb_explore_data_refactor.R: empty, template script to explore new datasets that may be added to the analysis via models and visualization
