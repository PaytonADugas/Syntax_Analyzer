# Char Classes
LETTER <- 0
DIGIT <- 1
UNKNOWN <- 99

# Token Codes
INT_LIT <- 10
IDENT <- 11
ASSIGN_OP <- 20
ADD_OP <- 21
SUB_OP <- 22
MULT_OP <- 23
DIV_OP <- 24
LEFT_PAREN <- 25
RIGHT_PAREN <- 26

# Open File as String
data = readLines(file.choose(), warn = FALSE)
data = toString(data)

# Split string into a list of characters
listData <- strsplit(data, " ")[[1]]

# Main Driver
main <- function() {
  if (is.null(data)) {
    print("ERROR - cannot open inFile.txt")
  } else {
    for (i in seq.int(1, length(listData), 1)) {
      getChar(listData[i])
    }
  }
}

# Lookup - looks up opperators and parentheses and returns the token
lookup <- function(char) {
  token <- switch(
        char,
        "(" = LEFT_PAREN,
        ")" = RIGHT_PAREN,
        "+" = ADD_OP,
        "-" = SUB_OP,
        "*" = MULT_OP,
        "/" = DIV_OP
    )
  return(token)
}

# isAlpha - checks if the character is a letter
isAlpha <- function(char) {
  return(!grepl("[^A-Za-z]", char))
}

# isDigit - checks if the character is a digit
isDigit <- function(char) {
  return(!grepl("\\D", char))
}

# getChar - get the next character of input and determine its character class
getChar <- function(char) {
  if (nchar(char) > 1) {
    tempStr <- ""
    listChar <- strsplit(char, "")
    for (c in listChar) {
      if ((!isAlpha(c)) || !isDigit(c)) {
        getChar(c)
      } else {
        paste(tempStr, c, sep = "")
      }
    }
    getChar(tempStr)
  }
  if (char != " ") {
    if (isAlpha(char)) {
      lex(char, LETTER)
    }
    else if (isDigit(char)) {
      lex(char, DIGIT)
    } else {
      lex(char, UNKNOWN)
    }
  }
}

# lex - simple lexical analyzer for arithematic expressions
lex <- function(char, class) {
  if (class == LETTER) {
    print(paste("Next token is:", LETTER, "Next lexeme is:", char, sep = " "))
  } else if (class == DIGIT) {
    print(paste("Next token is:", DIGIT, "Next lexeme is:", char, sep = " "))
  } else {
    print(paste("Next token is:", lookup(char), "Next lexeme is:", char, sep = " "))
  }
}

main()
