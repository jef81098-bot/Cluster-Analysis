# Classification of Dietary Habits and Obesity Status

## 1. Project Overview

This project investigates whether dietary intake variables can be used to classify BMI categories among adolescents aged 12–18 using data from the National Health and Nutrition Examination Survey (NHANES) 2021–2023.

The analysis focuses on dietary patterns and their potential relationship with obesity status. Two classification approaches, k-Nearest Neighbors (kNN) and Classification Trees, are implemented and compared.

## 2. Data Sources

The project uses the following NHANES datasets:

- `DR1TOT_L.xpt`: Dietary Interview – Total Nutrient Intakes, Day 1
- `DEMO_L.xpt`: Demographic Variables and Sample Weights
- `BMX_L.xpt`: Body Measures

The datasets are merged using the participant identifier `SEQN`.

After applying the inclusion criteria, the final analytical dataset contains 667 participants aged 12–18 with valid BMI measurements and dietary recall information.

## 3. Data Processing

The analysis includes:

- Selecting adolescents aged 12–18.
- Excluding participants with unreliable dietary recalls or missing BMI values.
- Categorizing BMI into four groups:
  - Underweight: BMI < 18.5
  - Normal: 18.5 ≤ BMI < 24
  - Overweight: 24 ≤ BMI < 27
  - Obesity: BMI ≥ 27
- Calculating nutrient intake per 1,000 kcal to account for differences in total energy intake.
- Applying log transformation to dietary variables.
- Splitting the dataset into training (70%) and testing (30%) sets using stratified sampling.
- Handling missing values through median imputation based on the training data.

**Note:** The BMI categories in this project use fixed cutoffs rather than age- and sex-specific pediatric BMI-for-age percentiles. This should be considered when interpreting the results.

## 4. Statistical Analysis and Modeling

### Feature Selection

- Kruskal–Wallis tests are used to examine differences in dietary variables across BMI categories.
- Benjamini–Hochberg adjustment is applied to control the false discovery rate.
- Pearson correlation is used to reduce redundancy among predictors.

### k-Nearest Neighbors (kNN)

- Stratified 5-fold cross-validation is used to select the optimal number of neighbors.
- Candidate values of k: 3, 5, 7, 9, 11, 15, and 21.
- Imputation and scaling are performed within each cross-validation fold.

### Classification Tree

- A classification tree is fitted using the selected dietary predictors.
- 10-fold cross-validation is used to evaluate tree complexity and determine the pruning level.
- The original and pruned trees are evaluated on the test dataset.

## 5. Model Evaluation

The models are evaluated using:

- Accuracy
- Macro-Precision
- Macro-Recall
- Macro-F1 Score
- Balanced Accuracy

The final test-set results are summarized below.

| Model | Accuracy | Macro-F1 | Balanced Accuracy |
|---|---:|---:|---:|
| kNN (k = 7) | 31.19% | 0.2386 | 0.2439 |
| Original Classification Tree | 37.13% | 0.2043 | 0.2499 |
| Pruned Classification Tree | 41.58% | 0.1469 | 0.2500 |

Although the pruned tree achieves the highest accuracy, it predicts every observation as the Normal BMI category because cross-validation selects a single-node tree. Therefore, its accuracy does not indicate effective discrimination among BMI categories.

The kNN model achieves a higher Macro-F1 score, but its overall classification performance remains limited.

## 6. Project Structure

```text
Cluster_Analysis/
│
├── classification_tree_fixed.R
├── dietary_habits_classification_report.Rmd
├── dietary_habits_classification_report.pdf
├── output.zip
├── output/
│   └── (Analysis outputs and figures)
│
└── README.md
7. Limitations
Several limitations should be considered:
1. Fixed BMI cutoffs do not account for age- and sex-specific growth patterns in adolescents.
2. Dietary intake is based on a single 24-hour recall and may not represent usual dietary habits.
3. The cross-sectional study design does not establish temporal or causal relationships.
4. Potential confounders, such as physical activity, socioeconomic status, sleep, and pubertal development, are not fully considered.
5. Feature selection is performed before cross-validation, which may introduce selection uncertainty.
6. NHANES complex survey design features, including sampling weights, strata, and primary sampling units, are not incorporated into the classification analysis.
7. The classification models show limited ability to distinguish among the four BMI categories.
8. Conclusion
Dietary variables alone provide limited predictive information for classifying BMI categories among adolescents in this analysis. While kNN performs better in terms of Macro-F1, neither model demonstrates satisfactory classification performance.

Future studies could incorporate age- and sex-specific BMI percentiles, additional lifestyle and socioeconomic variables, and more representative dietary measurements to improve classification performance.
9. Data Source
National Health and Nutrition Examination Survey (NHANES), August 2021–August 2023.

https://www.cdc.gov/nchs/nhanes/
