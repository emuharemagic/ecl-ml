IMPORT * FROM $;

EXPORT Identity(UNSIGNED4 dimension) := Vec.ToDiag( Vec.From(dimension,1.0) );