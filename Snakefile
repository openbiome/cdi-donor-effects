rule all:
    input: "models.rds", "results.txt"

rule analyze:
    output: "plot.pdf", txt="results.txt"
    input: "models.rds", "utils.R", script="analyze-models.R"
    shell: "./{input.script} > {output.txt}"

rule run_models:
    output: "models.rds"
    input: "data.tsv", "utils.R", script="run-models.R"
    shell: "./{input.script}"
