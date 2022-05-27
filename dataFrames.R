# Create the data frame.
emp.data <- data.frame(
  emp_id = c(1:5), 
  emp_name = c("Brijesh","Anuj","Krupa","Khanjan","Preksha"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)


# Print the data frame.			
print(emp.data) 

str(emp.data)


# Add the "dept" coulmn.
emp.data$dept <- c("IT","Operations","IT","HR","Finance")
v <- emp.data
print(v)


#extract emp name and salary

#extract first two rows

#extract 3rd and 5th row with 2nd and 4th column
