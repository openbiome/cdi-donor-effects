# Fecal microbiota transplantation “donor effects” are not clinically relevant for *Clostridioides difficile* infection

Code to support publication in *Gastroenterology* (2020). doi: 10.1053/j.gastro.2020.12.057

## File structure

### Scripts

- `analyze-models.R`: Script to analyze models that have been run
- `run-models.R`: Runs the models
- `Snakefile`: Runs the two analysis scripts
- `utils.R`: Reads and cleans the input data

### Inputs

The scripts rely on a file `data.tsv`, which is not included in the repository
for privacy reasons. Instead, a file `example-data.tsv` shows the format of the
data for 500 rows. Running the scripts on this example file will not reproduce
the results of the published analysis.

### Outputs

- `models.rds`: Cached model fits
- `plot.pdf`: Output plot
- `results.txt`: Output results

## Author

Scott Olesen <solesen@openbiome.org>
