# CocoaLab
# Chocolate Bar Ratings: Clustering and Prediction Analysis

**Category**: Clustering, Classification, Feature Engineering  
**Tools Used**: Python, Random Forest, Hierarchical Clustering  

## Overview
This project explores machine learning techniques to analyze and predict chocolate bar ratings. Using a dataset with variables like cocoa content, bean origins, and company locations, the goals were to identify clusters of similar chocolates and build a predictive model for chocolate ratings. These insights aim to assist chocolate manufacturers in refining products and strategies to capture market success.

## Dataset
The dataset contains key features about chocolate bars, such as:
- **Cocoa Content**: Transformed into categories (Low, Medium, High).
- **Bean Origin Continent**: Geographical region of cocoa beans.
- **Company Location Continent**: Company region.
- **Review Date**: Year the chocolate was reviewed.
- **Rating Categories**: Focused on 'Premium,' 'Satisfactory,' and 'Disappointing.'

## Goals
1. Cluster chocolates based on their features to uncover trends and insights.
2. Use machine learning (Random Forest) to predict chocolate ratings.
3. Provide actionable recommendations for the chocolate industry.

## Methodology
### Clustering
- Used **hierarchical clustering** with features such as Review Date, Cocoa Level, Company Location Continent, and Bean Origin Continent.
- Determined optimal clusters using dendrogram analysis and model performance metrics.
- Identified 4 clusters:
  - **Global Classic Chocolates**: Modern, diverse origins.
  - **Traditional and Old Chocolates**: Historical flavors from classic regions.
  - **Regional Classic Chocolates**: Regionally tailored flavors.
  - **Contemporary Chocolates**: Innovative products aligned with market trends.

### Classification
- Applied **Random Forest** to predict ratings.
- Engineered a "Cluster" feature from the clustering step.
- Focused on predicting 'Premium,' 'Satisfactory,' and 'Disappointing' categories.
- Key features: Cocoa Level, Cluster, Review Date, and Company Location Continent.

## Results
- **Model Accuracy**: ~60% (OOB error rate: 39.99%).
- **Feature Importance**:
  1. Review Date
  2. Company Location Continent
  3. Cocoa Level
  4. Cluster (engineered feature)
- High accuracy in predicting 'Premium' chocolates (13.03% error rate).
- Lower accuracy for 'Satisfactory' and 'Disappointing' due to limited data.

## Insights
- **Market Segmentation**: Clusters identify target markets (e.g., modern vs. traditional).
- **Product Development**: Emphasize trends and regional preferences for better product design.
- **Strategic Expansion**: Focus on high-demand regions to improve sales.
- **Model Improvement**: Collect more data for underrepresented rating categories.

## How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/chocolate-rating-analysis.git
