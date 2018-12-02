
ColNames <- function(interval){
  col_names <- c()
  for(i in 1:interval){
    if(i != 1){
      col = paste("a", i, sep = "")
      col_names <- c(col_names, col)
    }
    
    col = paste("b", i, sep = "")
    col_names <- c(col_names, col)
    
    col = paste("c", i, sep = "")
    col_names <- c(col_names, col)
  }
  
  col_names <- c(col_names, "RHS")
  return(col_names)
}

RowNames <- function(interval){
  rows = c()
  for(i in 1:interval){
    rows <- c(rows, i)
  }
  return(rows);
}

FirstSet <- function(x, y, n, m, matrix_size){
  col = 0
  row = 1
  RHS <- c()
  equations_list <- c()
  
  for(i in 2:(n-1)){
    var = paste("a", i-1, sep = "") 
    term = paste(x[i]^2, var, sep = " * ")
    if((i-1) != 1){ 
      col = col - 2
      row = row + 1
      m[row, col] = x[i]^2
    }
    
    var = paste("b", i-1, sep = "") 
    second_term = paste(x[i], var, sep = " * ")
    term = paste(term, second_term, sep = " + ")
    col = col + 1 
    m[row, col] = x[i]
    
    var = paste("c", i-1, sep = "") 
    term = paste(term, var, sep = " + ")
    col = col + 1
    m[(row), col] = 1
    row = row + 1
    
    term = paste(term, y[i], sep = " = ")
    equations_list <- c(equations_list, term)
    RHS <- c(RHS, y[i])
    
    
    var = paste("a", i, sep = "")
    term = paste(x[i]^2, var, sep = " * ")
    col = col+1
    m[row, col] = x[i]^2 
    
    var = paste("b", i, sep = "")
    second_term = paste(x[i], var, sep = " * ")
    term = paste(term, second_term, sep = " + ")
    col = col+1
    m[row, col] = x[i]
    
    var = paste("c", i, sep = "")
    term = paste(term, var, sep = " + ")
    col = col+1
    m[row, col] = 1
    
    term = paste(term, y[i], sep = " = ")
    equations_list <- c(equations_list, term)
    RHS <- c(RHS, y[i])
  }  
  
  return(list(equations_list = equations_list, m=m, curr_row=row, RHS=RHS))
}


SecondSet <- function(x, y, n, m, row, matrix_size, RHS){
  equations_list <- c()
  col = 1
  
  var = "a1"
  term = paste(x[1]^2, var, sep = " * ")
  row = row+1
  
  var = "b1"
  second_term = paste(x[1], var, sep = " * ")
  term = paste(term, second_term, sep = " + ")
  m[row, col] = x[1]
  col = col+1
  
  var = "c1"
  term = paste(term, var, sep = " + ")
  m[row, col] = 1
  
  term = paste(term, y[1], sep = " = ")
  equations_list <- c(equations_list, term)
  RHS <- c(RHS, y[1])
  
  
  var = paste("a", n-1, sep = "")
  term = paste(x[(n)]^2, var, sep = " * ")
  row = row + 1
  col = matrix_size - col
  m[row, col] = x[n]^2
  col = col+1
  
  var = paste("b", n-1, sep = "")
  second_term = paste(x[(n)], var, sep = " * ")
  term = paste(term, second_term, sep = " + ")
  m[row, col] = x[n]
  col = col+1
  
  var = paste("c", n-1, sep = "")
  term = paste(term, var, sep = " + ")
  m[row, col] = 1
  
  term = paste(term, y[(n)], sep = " = ")
  equations_list <- c(equations_list, term)
  RHS <- c(RHS, y[n])
  return(list(equations_list = equations_list, m=m, curr_row=row, RHS=RHS))
}

ThirdSet <- function(x, y, n, m, row){
  equations_list <- c()
  col = 1
  
  for(i in 2:(n-1)){
    var = paste("a", i-1, sep = "")
    term = paste(2*x[i], var, sep = " * ")
    row = row+1
    if((i-1) != 1){
      m[row, (col-1)] = 2*x[i]
    }
    
    var = paste("b", i-1, sep = "")
    term = paste(term, var, sep = " + ")
    m[row, col] = 1
    
    var = paste("a", i, sep = "")
    rhs_term = paste(2*x[i], var, sep = " * ")
    term = paste(term, rhs_term, sep = " = ")
    col = col+2
    m[row, col] = -2*x[i]
    
    var = paste("b", i, sep = "")
    term = paste(term, var, sep = " + ")
    col = col+1
    m[row, col] = -1
    
    equations_list <- c(equations_list, term)
  }
  return(list(equations_list = equations_list, m=m))
}

SetUpRHS <- function(m, RHS, interval, matrix_size){
  col = matrix_size+1
  for(i in 1:(2*interval)){
    m[i, col] = RHS[i]
  }
  return(m)
}

QuadraticSpline <- function(x, y, n, matrix_size, interval){
  equations_list <- c()
  init_m = matrix(data=0, nrow=matrix_size, ncol=matrix_size+1, dimnames = list(RowNames(matrix_size), ColNames(interval)))
  
  o = order(x)
  x = x[o]
  y = y[o]
  
  first_set = FirstSet(x, y, n, init_m, matrix_size)
  second_set = SecondSet(x, y, n, first_set$m, first_set$curr_row, matrix_size, first_set$RHS)
  third_set = ThirdSet(x, y, n, second_set$m, second_set$curr_row)
  RHS = second_set$RHS
  
  equations_list <- c(equations_list, first_set$equations_list)
  equations_list <- c(equations_list, second_set$equations_list)
  equations_list <- c(equations_list, third_set$equations_list)
  
  print(third_set$m)
  final_matrix = SetUpRHS(third_set$m, RHS, interval, matrix_size)
  print(final_matrix)
  return(equations_list)
}


x <- c(3, 7,4.5, 9)
y <- c(2.5, 2.5, 1, 0.5)
#x <- c(1.6, 2, 2.5)
#y <- c(2, 8, 14)
n = length(x)

interval = n-1
matrix_size = (3*interval)-1

functions_list = QuadraticSpline(x, y, n, matrix_size, interval)
print(functions_list)



