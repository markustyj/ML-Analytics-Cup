# ML-Analytics-Cup
Business analytics and machine learning for given data

1st try of data cleaning:
1) Binary type is changed to numeric type for convenience, i.e. 1 or 0
2) Date is changed to numeric type too. Starting from 2020.01.01, difference is calculated, i.e. year*365 + month*30 + day
   e.g. 2021.04.12 = (2021-2020)*365 + (4-1)*30 + 12
   I add Diff_Date myself, which stands for the difference between creation_date and release_date
3) Creator has 100+ different string values, but one specific value accounts for over 60%. 
   So I add a binary data type for it, which is = 1, if Creator == the specific value.
4) MATKL is also added for service-maps.csv. MATKL = 1, if Material_Class == one of the values of Service_Map.
There are still much room for improvement, but let's do the model first and see how to change the model better.
