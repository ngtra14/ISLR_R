# Chapter 11 data import
# inine csv file
read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

# skip lines
read_csv("The first line of metadat
the second line of metadata
        a, b, c
         1, 2, 3
         4, 5, 6", skip = 2)

read_csv("# The first line of metadat
# the second line of metadata
        a, b, c
         1, 2, 3
         4, 5, 6", comment = "#")

# no column names
read_csv("1, 2, 3\n4, 5, 6", col_names = FALSE)

read_csv("1, 2, 3\n4, 5, 6", 
         col_names = c("x", "y", "z"))

# dealing with NAs
read_csv("a, b, c\n4, 5, .", 
         na = ".")

# alternave 
# data.table::fread()

# quiz
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")
