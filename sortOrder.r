elements <- NA
number.of.elements <- NA
model <- NA
constraint.names <- vector()


GetOptimalOrder <- function(solved.model) {
  variables <- get.variables(solved.model)
  choices.by.element <- matrix(variables, number.of.elements, number.of.elements)
  ordered.elements <- rep('', number.of.elements)
  for (i in 1:number.of.elements) {
    position <- which(choices.by.element[,i] == 1)
    ordered.elements[position] <- elements[i]
  }
  return(ordered.elements)
}

AddOneElementPerPositionConstraints <- function() {
  constraint.constants <- rep(1, number.of.elements)
  for (i in 1:number.of.elements) {
    indices <- seq(i, number.of.elements ^ 2, number.of.elements)
    add.constraint(model, constraint.constants, type='=', rhs=1, indices=indices)
    constraint.names <<- c(constraint.names, paste('Only one element in position', i))
  }
}

GetIndices <- function(element) {
  start.index = 1 + number.of.elements * (which(elements == element) - 1)
  return(start.index:(start.index + number.of.elements - 1))
}

AddOnePositionPerElementConstraints <- function() {
  constraint.constants <- rep(1, number.of.elements)
  for (i in 1:number.of.elements) {
    element = elements[i]
    indices <- GetIndices(element)
    add.constraint(model, constraint.constants, type='=', rhs=1, indices=indices)
    constraint.names <<- c(constraint.names, paste(element, 'only has one position'))
  }
}

AddPositionalConstraint <- function(element1, order='before', element2) {
  indices <- c(GetIndices(element1), GetIndices(element2))
  constraint.constants <- c(1:number.of.elements, -1:-number.of.elements)
  constraint.type <- ifelse(tolower(order) == 'before', '<=', '>=')
  add.constraint(model, constraint.constants, type=constraint.type, rhs=0.1, indices=indices)
  constraint.names <<- c(constraint.names, paste(element1, 'comes', order, element2))
}

AssignDimnames <- function() {
  column.names = vector()
  for (i in 1:number.of.elements) {
    for (j in 1:number.of.elements) {
      column.names <- c(column.names, paste(elements[i], 'in position', j)) 
    }
  }
  dimnames(model) <- list(constraint.names, column.names)
}

Init <- function() {
  library('lpSolveAPI')
  elements <<- c('Joe', 'Bob', 'Dave', 'Mike')
  number.of.elements <<- length(elements)
  model <<- make.lp(0, number.of.elements ^ 2)
  set.type(model, 1:(number.of.elements ^ 2), 'binary')
  set.objfn(model, rep(1, number.of.elements ^ 2))
}

Main <- function() {
  Init()
  AddOneElementPerPositionConstraints()
  AddOnePositionPerElementConstraints()
  AddPositionalConstraint('Bob', 'before', 'Mike')
  AddPositionalConstraint('Mike', 'after', 'Joe')
  AssignDimnames()
  solve(model)
  write.lp(model, 'model.lp', type='lp')
  cat(GetOptimalOrder(model))
}

Main()