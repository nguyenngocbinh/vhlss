
# ETL data for VHLSS 

# Note: In windows, code should use eval rather than source

source('D:/R/VHLSS/labels.R') # Should not

eval(parse("labels.R", encoding = "UTF-8")) # Better

# Graphs in rmarkdown are performed well 
eval(parse("graphs.R", encoding = "UTF-8"))
