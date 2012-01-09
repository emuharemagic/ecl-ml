IMPORT ML;
IMPORT ML.Docs AS Docs;
d11 := DATASET([                                                                                                                
{'In the beginning God created the heavens and the earth. '},
{'The earth was without form, and void; and darkness was[a] on the face of the deep. And the Spirit of God was hovering over the face of the waters.'},
{'Then God said, “Let there be light”; and there was light. '},
{'And God saw the light, that it was good; and God divided the light from the darkness. '},
{'God called the light Day, and the darkness He called Night. So the evening and the morning were the first day.'},
{'Then God said, “Let there be a firmament in the midst of the waters, and let it divide the waters from the waters.”'},
{'Thus God made the firmament, and divided the waters which were under the firmament from the waters which were above the firmament; and it was so. '},
{'And God called the firmament Heaven. So the evening and the morning were the second day.'},
{'Then God said, “Let the waters under the heavens be gathered together into one place, and let the dry land appear”; and it was so. '},
{'And God called the dry land Earth, and the gathering together of the waters He called Seas. And God saw that it was good. '}],
{STRING r});

d00 := DATASET([{'aa bb cc dd ee'},{'bb cc dd ee ff gg hh ii'},{'bb cc dd ee ff gg hh ii'}, {'dd ee ff'},{'bb dd ee'}],{string r});
d := d11;
d1 := PROJECT(d,TRANSFORM(Docs.Types.Raw,SELF.Txt := LEFT.r));
d2 := Docs.Tokenize.Enumerate(d1);
d3 := Docs.Tokenize.Clean(d2);
d4 := Docs.Tokenize.Split(d3);
lex := Docs.Tokenize.Lexicon(d4);
o1 := Docs.Tokenize.ToO(d4,lex);
o2 := Docs.Trans(O1).WordBag;
lex;
ForAssoc := PROJECT( o2, TRANSFORM(ML.Types.ItemElement,SELF.id := LEFT.id,
SELF.value := LEFT.word ));
ForAssoc;
ML.Associate(ForAssoc,2).Apriori1;
ML.Associate(ForAssoc,2).Apriori2;
ML.Associate(ForAssoc,2).Apriori3;
ML.Associate(ForAssoc,2).AprioriN(40);

//Added - not part of the doc.
ML.Associate(ForAssoc,2).EclatN(40);
