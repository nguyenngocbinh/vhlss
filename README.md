
# ETL data for VHLSS 

# Note: In windows should using eval rather than source

source('D:/R/VHLSS/labels.R') # Should not

eval(parse("labels.R", encoding = "UTF-8")) # Better
