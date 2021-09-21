#
























# all letters
LETTERS <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

# Token Codes
var.INT_LIT = 10
var.IDENT = 11
var.ASSIGN_OP = 20
var.ADD_OP = 21
var.SUB_OP = 22
var.MULT_OP = 23
var.DIV_OP = 24
var.LEFT_PAREN = 25
var.RIGHT_PAREN = 26

# Final lexeme
lexeme <- c()
variable <- ''

# Grabs the expression stored in the "expression" file
expr = toString(read.delim("code.txt", header = FALSE))
expr_split <- strsplit(expr, '')[[1]]

# Loops through the expression characters
loop_char <- function(){
  for(c in expr_split){
    if(c != ' ')
      getClass(c)
  }
}

# looks up the operators and parentheses
lookup_op <- function(char){
  var.token = switch(char, '('=var.LEFT_PAREN, ')'=var.RIGHT_PAREN, '+'=var.ADD_OP, '-'=var.SUB_OP, '*'=var.MULT_OP, '/'=var.DIV_OP)
  return (var.token)
}

isNumber <- function(n){
  if(!is.na(as.numeric(n))){
    return(TRUE)
  }
  return(FALSE)
}

# Determine character class
getClass <- function(c){
  if(isNumber(c)){
    build_lexeme(c(var.INT_LIT,c))
  } else if(isLetter(c)){
    build_lexeme(c(var.IDENT, c))
  } else {
    build_lexeme(c(lookup_op(c),c))
  }
}

# Check if character is a letter
isLetter <- function(c){
  for (l in LETTERS){
    if(c == l){
      return(TRUE)
    }
  }
  return(FALSE)
}

# Builds the lexeme
build_lexeme <- function(code){
  cat(paste('Next token is:',
  code[1],
  'Next lexeme is:',
  code[2],
  '\n', sep=' '))

  # append doesn't work for some reason so I'm just printing the components out...
  append(lexeme, code, after = length(lexeme))
}

# Supposed to print lexeme, but R is weird and doesn't really work
print_final <- function(l){
  print(length(lexeme))
  for(c in lexeme){

  }
}

loop_char()
print_final(lexeme)
