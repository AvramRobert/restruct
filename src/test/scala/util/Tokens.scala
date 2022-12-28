package util

import data.{Pattern, Token}

val makerPattern: Pattern = Pattern.TokenPattern(Token.Maker)
val namePattern: Pattern = Pattern.TokenPattern(Token.Name)
val keyPattern: Pattern = Pattern.TokenPattern(Token.Key)
val tempoPattern: Pattern = Pattern.TokenPattern(Token.Tempo)
