uciCar<-read.table('http://www.win-vector.com/dfiles/car.data.csv',sep=',',header=T)
dim(uciCar)

#1 list
# Create a list containing strings, numbers, vectors and a logical
# values.

list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)

#2 naming list
# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), 
                  matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))


mat<-matrix(c(3,9,5,1,-2,8,10,11,12,20), nrow = 3)


matrix(c(),)


# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "Matrix", "An Inner list")

# Show the list.
print(list_data)

#3 accessing list elements
# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("Vector", "Matrix", "An Inner list")

# Access the first element of the list.
print(list_data[3])

# Access the thrid element. As it is also a list, all its elements will be printed.
print(list_data[5])

# Access the list element using the name of the element.
print(list_data$Vector)


#4 manipulating list data
# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

# Add element at the end of the list.
list_data[4] <- "New element"
print(list_data[4])

# Remove the last element.
list_data[4] <- NULL

# Print the 4th Element.
print(list_data[4])

# Update the 3rd Element.
list_data[3] <- list(c("Apr","May","June"))
print(list_data[3])


#5 merging list
# Create two lists.
list1 <- list(1,2,3)
list2 <- list("Sun","Mon","Tue")
v1 <- c(1,2,3)
v2 <- c(4,6,7)
# Merge the two lists.
merged.list <- c(list1,list2)

# Print the merged list.
print(merged.list)

#6 converting list into vector  
# Create lists.
list1 <- list(1:5)
print(list1)

list2 <-list(10:14)
print(list2)

# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)

print(v1)
print(v2)

# Now add the vectors
result <- v1+v2
print(result)

#merge vectors
v1 <- c(10,20,30)
v2 <- c(40,50,60)
v3 <- c(v1,v2)
print(class(v3))
