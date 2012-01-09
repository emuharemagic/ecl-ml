// Utilities for the implementation of ML (rather than the interface to it)
IMPORT * FROM $;
IMPORT Std.Str;
EXPORT Utils := MODULE

EXPORT Pi := 3.1415926535897932384626433;

EXPORT REAL8 Fac(UNSIGNED2 i) := BEGINC++
  double accum = 1.0;
	for ( int j = 2; j <= i; j++ )
		accum *= (double)j;
	return accum;
  ENDC++;

// The 'double' factorial is defined for ODD n and is the product of all the odd numbers up to and including that number
// We are extending the meaning to even numbers to mean the product of the even numbers up to and including that number
// Thus DoubleFac(8) = 8*6*4*2
// We also defend against i < 2 (returning 1.0)
EXPORT REAL8 DoubleFac(INTEGER2 i) := BEGINC++
  if ( i < 2 )
		return 1.0;
  double accum = (double)i;
	for ( int j = i-2; j > 1; j -= 2 )
		accum *= (double)j;
	return accum;
  ENDC++;

// N Choose K - finds the number of combinations of K elements out of a possible N
// Should eventually do this in a way to avoid the intermediates (such as Fac(N)) exploding
EXPORT REAL8 NCK(INTEGER2 N, INTEGER2 K) := Fac(N)/(Fac(K)*Fac(N-k));

// In constrast to the matrix function thin
// Will take a potentially sparse file d and fill in the blanks with value v
EXPORT Fat(DATASET(Types.NumericField) d0,Types.t_FieldReal v=0) := FUNCTION
  dn := DISTRIBUTE(d0,HASH(id)); // all the values for a given ID now on one node
  seeds := TABLE(dn,{id,m := MAX(GROUP,number)},id,LOCAL); // get the list of ids on each node (and get 'max' number for free
	mn := MAX(seeds,m); // The number of fields to fill in
	Types.NumericField bv(seeds le,UNSIGNED C) := TRANSFORM
	  SELF.value := v;
		SELF.id := le.id;
		SELF.number := c;
	END;
	// turn n into a fully 'blank' matrix - distributed along with the 'real' data
	n := NORMALIZE(seeds,mn,bv(LEFT,COUNTER),LOCAL); 
	// subtract from 'n' those values that already exist
	n1 := JOIN(n,dn,LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,TRANSFORM(LEFT),LEFT ONLY,LOCAL);
	RETURN n1+dn;
END;
	
// Given a file which is sorted by INFIELD (and possibly other values), add sequence numbers within the range of each infield
// Slighly elaborate code is to avoid having to partition the data to one value of infield per node
EXPORT mac_SequenceInField(infile,infield,seq,outfile) := MACRO

#uniquename(add_rank)
TYPEOF(infile) %add_rank%(infile le,UNSIGNED c) := TRANSFORM
  SELF.seq := c;
	SELF := le;
  END;
	
#uniquename(P)
%P% := PROJECT(infile,%add_rank%(LEFT,COUNTER));

#uniquename(RS)
%RS% := RECORD
  __Seq := MIN(GROUP,%P%.seq);
  %P%.infield;
  END;

#uniquename(Splits)
%Splits% := TABLE(%P%,%RS%,infield,FEW);

#uniquename(to_1)
TYPEOF(infile) %to_1%(%P% le,%Splits% ri) := TRANSFORM
	SELF.Seq := 1+le.Seq - ri.__Seq;
	SELF := le;
  END;
	
outfile := JOIN(%P%,%Splits%,LEFT.InField=RIGHT.InField,%to_1%(LEFT,RIGHT),LOOKUP);

ENDMACRO;

// Shift the column-numbers of a file of discretefields so that the left-most column is now new_lowval
// Can move colums left or right (or not at all)
EXPORT RebaseDiscrete(DATASET(Types.DiscreteField) cl,Types.t_FieldNumber new_lowval) := FUNCTION
  CurrentBase := MIN(cl,number);
	INTEGER Delta := new_lowval-CurrentBase;
	RETURN PROJECT(cl,TRANSFORM(Types.DiscreteField,SELF.number := LEFT.number+Delta,SELF := LEFT));
  END;

EXPORT RebaseNumericField(DATASET(Types.NumericField) cl) := MODULE
  SHARED MapRec:=RECORD
		Types.t_FieldNumber old;
		Types.t_FieldNumber new;
	END;
  olds := TABLE(cl, {cl.number,COUNT(GROUP)}, number, FEW);	
	
	EXPORT Mapping(Types.t_FieldNumber new_lowval=1) := FUNCTION
	MapRec mapthem(olds le, UNSIGNED c) := TRANSFORM
		SELF.old := le.number;
		SELF.new := c-1+new_lowval;
	END;
		RETURN PROJECT(olds, mapthem(LEFT, COUNTER));
	END;
		
	EXPORT ToNew(DATASET(MapRec) MapTable) := FUNCTION
 		RETURN JOIN(cl,MapTable,LEFT.number=RIGHT.old,TRANSFORM(Types.NumericField, SELF.number := RIGHT.new, SELF:=LEFT),LOOKUP);
  END;	
	
	EXPORT ToOld(DATASET(Types.NumericField) cl, DATASET(MapRec) MapTable) := FUNCTION
 		RETURN JOIN(cl,MapTable,LEFT.number=RIGHT.new,TRANSFORM(Types.NumericField, SELF.number := RIGHT.old, SELF:=LEFT),LOOKUP);
  END;	
	
  END;	

// Service functions and support pattern
EXPORT	NotFirst(STRING S) := IF(Str.FindCount(S,' ')=0,'',S[Str.Find(S,' ',1)+1..]);
EXPORT	NotLast(STRING S) := IF(Str.FindCount(S,' ')=0,'',S[1..Str.Find(S,' ',Str.FindCount(S,' '))-1]);
EXPORT	NotNN(STRING S,UNSIGNED2 NN) := MAP( NN = 1 => NotFirst(S),
																						 NN = Str.WordCount(S) => NotLast(S),
                                             S[1..Str.Find(S,' ',NN-1)]+S[Str.Find(S,' ',NN)+1..] );
EXPORT  LastN(STRING S) := Str.GetNthWord(S,Str.WordCount(S));			

// Choose K (ascending element) permutations out of string of '1 2 3 ... N'  elements
// E.g. KoutofN(2,3) = '1 2', '2 3'
EXPORT  NchooseK(UNSIGNED1 N, UNSIGNED1 K) := FUNCTION
// generate string sample txt '1 2 3 ... N' to choose K elements from
rec := {UNSIGNED1 num};
seed := DATASET([{0}], rec);
txt := Str.CombineWords(SET(NORMALIZE(seed, N, TRANSFORM(rec, SELF.num := COUNTER)), (STRING2)num), ' ' );

R := RECORD
	STRING Kperm ;
	STRING From ;
END;
Init := DATASET([{'',txt}],R);
R Permutate(DATASET(R) infile) := FUNCTION
R TakeOne(R le, UNSIGNED1 c) := TRANSFORM
  SELF.Kperm := IF( (INTEGER1)Str.GetNthWord(le.from,c)> (INTEGER1)LastN(le.Kperm),le.Kperm + ' '+Str.GetNthWord(le.From, c),SKIP);
	SELF.From := NotNN(le.From,c);
END;
RETURN NORMALIZE(infile,Str.WordCount(LEFT.From),TakeOne(LEFT,COUNTER));
END;

RETURN TABLE(LOOP(Init,K,Permutate(ROWS(LEFT))), {Kperm});

END;
	

	
	
END;