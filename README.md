# MA223 Exam R Package

Statistical inference and visualization for machine learning experiment results.
Uses data from [ML Repo](https://github.com/myrstad/ma223-exam).

### Prerequisites
- R >= 4.0.0
- The following packages auto-install on first run:
  - `tidyverse` (ggplot2, dplyr, readr, etc.)
  - `jsonlite` (JSON parsing)
  - `conflicted` (namespace conflict resolution)
  - `cowplot` (plot arrangement)

### Running the Analysis

#### From Command Line
```bash
Rscript main.R
```

#### From RStudio
1. Open `ma223-exam-r.Rproj`
2. Open `main.R`
3. Click "Source" or run line-by-line

### Statistical Analyses
- **Confidence Intervals**
- **Hypothesis Testing**
- **Uncertainty Analysis**

All outputs are saved to the `output/` directory as PDF

## Input Data

The code expects experiment data organized as:
```
results/experiments/<group>/<exp_id>/
├── config.json                    # Model configuration
├── summary.json                   # Final metrics
├── history_train.csv             # Training history
├── history_valid.csv             # Validation history
├── test_results.csv              # Per-sample predictions
└── confusion_matrix.csv          # Confusion matrix
```

### Experiment Groups
- **ActivationComparison**: Compares activation functions (ReLU, GELU, SiLU, etc.)
- **InterpolateLearningRate**: Learning rate experiments
- **Linear3**: Linear model variants
- **LinearRepetition**: Repeated experiments
- **LinearRepetitionManyEpoch**: Longer training runs

## License

This project is part of the MA223 Statistics course at University of Agder.
