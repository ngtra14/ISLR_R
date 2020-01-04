# Chapter 2 Basics
library(mlr3)
data("mtcars", package = "datasets")
data <- mtcars[, 1:3]
str(data)


# 2.2.2 task creation -----------------------------------------------------

task_mtcars <- TaskRegr$new(id = "cars", backend = data, target = "mpg")
print(task_mtcars)

# library(mlr3viz)


# 2.2.3 predefined tasks --------------------------------------------------

mlr_tasks

library(data.table)
as.data.table(mlr_tasks)

task_iris <- mlr_tasks$get("iris")
print(task_iris)

tsk("iris")

# retrieving data
task_iris$nrow
task_iris$ncol

task_iris$data()

task_iris$data(rows = c(1, 51, 101))

task_mtcars = tsk("mtcars")
head(task_mtcars$row_ids)

task_mtcars$data(rows = "Datsun 710")
task_iris$data(rows = c(1, 51, 101), cols = "Species")

summary(as.data.table(task_iris))

# rows andd columns
print(task_mtcars$col_roles)

data = as.data.table(mtcars[, 1:3], keep.rownames = TRUE)
task = TaskRegr$new(id = "cars", backend = data, target = "mpg")
task$row_ids
