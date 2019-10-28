
# ETL data for VHLSS 

# Note: In windows should using eval rather than source

source('D:/R/VHLSS/labels.R') # Should not

eval(parse("labels.R", encoding = "UTF-8")) # Better

# With graphs in rmarkdown is performed well 
eval(parse("graphs.R", encoding = "UTF-8"))
