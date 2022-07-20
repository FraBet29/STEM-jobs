# STEM jobs

### About

Final project for the university course "Modelli e metodi per l'inferenza statistica" ("Models and methods for statistical inference"). The aim of the project is to perform a statistical analysis of a dataset - in this case, the dataset is about STEM jobs in the major tech companies in the USA (source: https://www.kaggle.com/datasets/jackogozaly/data-science-and-stem-salaries).

### Data preprocessing

The original dataset contained ~ 62000 rows, each representing a person, and 29 columns. We removed rows with missing data and selected some columns - yearly compensation, location, years of experience, company, job title, education, gender. The final dataset contains ~ 6000 rows and 7 columns. 

### Data visualization

- Ggpairs: we can see a correlation between years of experience and annual remuneration
- Boxplot yearly compensation - job title: we can notice, on average, a higher salary for the category of Software Engineering Managers
- Boxplot yearly compensation - company: we can notice, on average, a higher salary within Facebook
- Boxplot yearly compensation - education: we can notice, on average, a higher salary for PhDs
- Boxplot yearly compensation - gender: we can notice, on average, a higher salary for men

The boxplots also highlight the presence of several outliers.

### ANOVA

We used three one-way ANOVA tests to compare quantitatively: the average salaries among job titles; the average salaries among companies; the average salaries based on education. In all cases, there is at least one group with a significantly different average.

However, the ANOVA assumptions (normality of residuals and homogeneous variances between groups) are not verified. Hence, we performed the Kruskal-Wallis test, a non-parametric test that compares the means/medians without requiring the hypothesis of normality; the test confirms the results of the classic ANOVA.

Finally, we applied the Wilcoxon pairwise test to determine which groups are significantly different: for job titles, the different group is Software Engineering Manager; all companies are significantly different; all levels of education are significantly different.

### Linear regression

We decided to build linear regression models in order to predict the yearly compensation as a function of the years of experience. In particular, we focused on software engineers (the largest category) and we built one model for each company separately to ensure good interpretability.

All the fitted models shared common characteristics:
- $R^2$ coefficient ranging from 0.3 to 0.4
- Omoschedastic residuals
- Non normal data (Shapiro test with p-value ~ $10^{-16}$)

In order to try to satisfy the hypothesis of normal residuals, we performed a Box-Cox transformation of the data; however, the results were still unsatisfactory. So, we tried to remove leverage points and points with a high Cook distance. The best model we could obtain was the one without points with a high Cook distance, with a $R^2$ coefficient ranging from 0.35 to 0.5, even though the hypothesis of normal residuals was still not satisfied.

By comparing the final regression lines for each company, we can see that the base salary for entry-level roles is almost the same, while the compensation growth per year varies a lot (ranging from $5000/year for Microsoft to $20000/year for Facebook).

We built analogous models introducing gender as a categorical variable. Apart from Amazon, there are no statistically significant differences in salary between men and women.

### Final remarks

All the different models showed that the yearly compensation as a function of the years of experience seems to follow well a linear relationship in the range 0 to 10 years, while there seems to be a “leveling” for >10 years. This is because wages cannot grow indefinitely; we could therefore use generalized linear models (with logarithmics terms, for example) to better describe the trend of the data.
