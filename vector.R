# Atomic vector of type character.
print(class("abc"))
print(class("12"))
print(class('12'))
# Atomic vector of type double.
print(12.5)
print(class(12.5))
# Atomic vector of type integer.
print(63L)
print(class(63L))
# Atomic vector of type logical.
print(TRUE)
print(class(TRUE))
# Atomic vector of type complex.
print(2+3i)
print(class(2+3i))

# Atomic vector of type raw.
print(charToRaw('abc'))

#1. Charcter 2. Double 3. Integer 4. Complex 5. Logical 6. Raw

# Creating a sequence from 5 to 13.
v <- 5:13
print(v)
print(class(v))
# Creating a sequence from 6.6 to 12.6.
v <- 6.6:12.6
print(v)

# If the final element specified does not belong to the sequence then it is discarded.
v <- 3.8:11.90
print(v)


# Create vector with elements from 5 to 9 incrementing by 0.4.
print(seq(5, 9.5, by = 0.5))
print(seq(1, 50, by=2))

# Accessing vector elements using position.
t <- c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat") #combine
t1 <- c(1L,2L,3,4,4.5)
t2 <- c(1L,FALSE, 55.55,"Sun")

u <- t[c(2,3,6)]

# Accessing vector elements using logical indexing.
v <- t[c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
v<-t[c(1:3)]
print(v)

# Accessing vector elements using negative indexing.
x <- t[c(-1,-6)]
print(x)

# Accessing vector elements using 0/1 indexing.
y <- t[c(3,2,0,0,0,0,1)]
print(y)

# Create two vectors.
v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11,0,8,1,2)

# Vector addition.
add.result <- v1+v2
print(add.result)

# Vector subtraction.
sub.result <- v1-v2
print(sub.result)

# Vector multiplication.
multi.result <- v1*v2
print(multi.result)

# Vector division.
divi.result <- v1/v2
print(divi.result)

v <- c(3,8,4,5,0,11, -9, 304)

# Sort the elements of the vector.
print(v)
sort.result <- sort(v)
print(sort.result)

# Sort the elements in the reverse order.
revsort.result <- sort(v, decreasing = TRUE)
print(revsort.result)

# Sorting character vectors.
v <- c("Red","Blue","yellow","Y")
sort.result <- sort(v)
print(sort.result)

# Sorting character vectors in reverse order.
revsort.result <- sort(v, decreasing = TRUE)
print(revsort.result)

v1 <- c(10,20,30,40,20,50)
v2 <- c(50,60,10)

