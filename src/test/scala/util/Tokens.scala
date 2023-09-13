package util

import data.{Pattern, NamingToken}

val makerPattern: Pattern = Pattern.TokenPattern(NamingToken.MakerToken)
val namePattern: Pattern = Pattern.TokenPattern(NamingToken.NameToken)
val keyPattern: Pattern = Pattern.TokenPattern(NamingToken.KeyToken)
val tempoPattern: Pattern = Pattern.TokenPattern(NamingToken.TempoToken)
