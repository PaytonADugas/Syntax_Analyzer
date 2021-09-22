# Global Vars
charClass
lexeme
nextChar
lexLen
token
nextToken

# Func Declare
addChar <- function(x) {}
getChar <- function(x) {}
getNonBlank <- function(x) {}
lex

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
data <- toString(read.delim("inFile.txt", header = FALSE))

# Split string into a list of characters
listData <- strsplit(data, "")[[1]]

# Main Driver
main <- function() {
    if(data == null) {
        print("ERROR - cannot open inFile.txt")
    }   else {
        getChar()
    }  
    i <- 1
    while(i < listData.length) {
        lex(i)
        i <- i + 1
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
    return (token)
}

# isAlpha - checks if the character is a letter
isAlpha <- function(char) {
    !grepl("[^A-Za-z]", char)
}

# isDigit - checks if the character is a digit
isDigit <- function(char) {
    !grepl("\\D", char)
}

# addChar - add nextChar to lexeme
addChar <- function() {
    if (lexLen <= 98) {
        lexeme[lexLen <- lexLen + 1] = nextChar
        lexeme[lexLen] = 0
    }   else {
        "Error - lexeme is too long"
    }
}

# getChar - get the next character of input and determine its character class
getChar <- function() {
    for (char in listData) {
        if (char != " ") {
            if (isAlpha(char)) {
                lex(char, LETTER)
            }   else if (isdigit(char)) {
                lex(char, DIGIT)
            }   else {
                lex(char, UNKNOWN)
            }
        }
    }
}

# getNonBlank - calls getChar until it returns a non-whitespace character
## getNonBlank <- function() {
##    while(isspace())
##}

# lex - simple lexical analyzer for arithematic expressions
lex <- function(char, class) {
    lexLen <- 0
    if (class == LETTER) {
        print(paste("Next token is:",LETTER,"Next lexeme is:",char, sep = " "))
    }   else if (class == DIGIT) {
        print(paste("Next token is:",DIGIT,"Next lexeme is:",char, sep = " "))
    }   else {
        print(paste("Next token is:",lookup(char),"Next lexeme is:",char, sep = " "))
    }
}