EXPORT Types := MODULE

EXPORT t_DocID := UNSIGNED; // Make unsigned4 if < 9B documents
EXPORT t_Line := STRING;
EXPORT t_Word := STRING;
EXPORT t_WordId := UNSIGNED4; // This would be a very large vocabulary
EXPORT t_Position := UNSIGNED4; // Allows up to 9B words in one document
EXPORT t_Count := UNSIGNED; // UNSIGNED4 if <9B of any one token in caucus
EXPORT t_Value := REAL;

EXPORT Raw := RECORD
	t_DocId id := 0;
  t_Line txt;
  END;
	
EXPORT WordElement := RECORD
  t_DocID id;
	t_Position pos;
	t_Word  word;
  END;
	
EXPORT OWordElement := RECORD
  t_DocID    id;
	t_Position pos;
	t_WordId   word;
	t_Count    total_words;
	t_Count    total_docs;
	t_Count    words_in_doc; // Could possibly make a little smaller
  END;

EXPORT LexiconElement := RECORD
	t_WordId   word_id;
	t_Count    total_words;
	t_Count    total_docs;
	t_Word     word;
  END;
  
END;
