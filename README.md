# Price-optimization-for-product-recommendation-engine

This is a script that classifies users into buyers or not buyers depending on the mobile device model they browse with, the category of product they are looking for, and the avg. price of the product they are looking at.

stat_vs_all.r: Compares a specific device model against the rest of the models, to check for significant difference in avg. product purchased (it only makes sense to train this model if there is significant differences)
stat_dif.r: Compares all models against all models


trainer.r: Clean initial data, split into training and validation groups. Also outputs file that has the values that yield the greatest sensitivity and specificity
validator.r: Validate model
revenue_calc_best_parameters.r: Calculates extra revenue coming from more accurate product recommendations, takes into account lost revenue from outlier users.
classic_calculator: Optimizes prices ranges to generate maximum revenue, instead of maximizing model accuracy.

