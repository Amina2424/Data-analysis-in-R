# Data-analysis-in-R
This project was completed as part of the advanced training program ‘Big Data Analysis in Biomedicine’ (72 academic hours), which focused on developing expertise in large-scale biological data processing, computational methods, and their applications in medical research.

Here’s a professional `README.md` file for your project in English, structured for clarity and reproducibility:

---

# **Cardiovascular Disease Analysis Using Data Mining Techniques**  
**Advanced Training Program in *Big Data Analysis in Biomedicine*** (72 academic hours)  

## **Project Overview**  
This project analyzes cardiovascular diseases (CVDs) using statistical and machine learning methods to identify correlations between lifestyle factors and heart disease prevalence. The dataset includes **319,795 observations** with 18 features (4 numerical, 14 categorical).  

### **Key Objectives**  
1. Investigate relationships between lifestyle factors and CVD risk.  
2. Apply exploratory data analysis (EDA) and statistical tests.  
3. Build a predictive logistic regression model for heart disease.  

---

## **Dataset Features**  
### **Target Variable**  
- `HeartDisease`: Binary indicator of CVD presence.  

### **Numerical Variables**  
- `BMI`, `PhysicalHealth`, `MentalHealth`, `SleepTime`.  

### **Categorical Variables**  
- `Smoking`, `AlcoholDrinking`, `Stroke`, `Sex`, `AgeCategory`, `Diabetic`, etc.  

---

## **Methodology**  
### **1. Data Preprocessing**  
- Removed duplicates (`n=18,078`) and outliers (IQR method).  
- Handled imbalanced classes (e.g., only 8.5% of samples had CVD).  

### **2. Statistical Analysis**  
- **Normality Tests**: Anderson-Darling, Kolmogorov-Smirnov (all variables non-normal, *p* < 2.2e−16).  
- **Correlation**: Spearman’s rank (weak correlations: |ρ| < 0.06).  
- **Group Comparisons**:  
  - Mann-Whitney *U* test for `BMI` by sex (*p* < 0.001).  
  - Kruskal-Wallis for `BMI` by race (*p* < 0.001).  
  - Chi-square tests for categorical-CVD associations (all *p* < 0.001).  

### **3. Predictive Modeling**  
- **Logistic Regression**: AUC = 0.753, Recall = 0.13 (class imbalance impacted performance).  
- **Key Predictors**: Age, diabetes, kidney disease, stroke history (`p` < 0.05).  

---

## **Key Findings**  
1. **Demographics**: Men had higher CVD prevalence (61.9% vs. 47.7% in women).  
2. **Lifestyle**: No strong correlation between alcohol/smoking and CVD (*contradicts some literature*).  
3. **Health Metrics**: Poor `GenHealth` and `DiffWalking` were strongly linked to CVD.  

---

## **How to Reproduce**  
### **Dependencies**  
- R libraries: `nortest`, `ggplot2`, `dplyr`, `MASS`.  
- Python (optional): `pandas`, `scipy`, `sklearn`.  

### **Code Structure**  
```bash
├── Data/                 # Processed dataset (CSV)  
├── Scripts/              # R/Python scripts for analysis  
│   ├── 01_Data_Cleaning.R  
│   ├── 02_EDA.R  
│   └── 03_Modeling.R  
├── Results/              # Outputs (plots, tables)  
└── References/           # Literature sources  
```

---

## **References**  
1. Tsao et al. (2023). *Circulation* 147(8): e93–e621.  
2. Lemieux et al. (2024). *Canadian Journal of Cardiology*.  
3. Soni et al. (2011). *International Journal of Computer Applications*.  

---

**Author**: Aminat Imanaliyeva (HSE University, KIMB-211)  
**Supervisor**: Svetlana Zhuravleva  
**Date**: Moscow, 2025  

--- 

### **Notes**  
- For model improvement, consider oversampling or tree-based algorithms (e.g., Random Forest).  
- Full analysis details in [`report.pdf`](Доклад (1).pdf).  

Let me know if you'd like to emphasize any specific section (e.g., technical details, visualizations)!
