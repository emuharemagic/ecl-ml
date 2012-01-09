export string FCN_PorterStemmer(string WORD) := FUNCTION

    //------ Regular expressions -------------------------
    // This is a consonant. Not "aiueo" and "y" only if preceded by a vowel
    c := '(?:[^aiueoy]|(?:(?<=[aiueo])y)|\\by)';
    c0 := '(?:[^aiueoy]|(?:(?<=[aiueo])y))';
      
    // This is a vowel. "aiueo" and "y" if preceded by a consonant
    v := '(?:[aiueo]|(?:(?<![aiueo])y))';
       
    // The re "/^(?:$c+)?(?:$v+$c+){m}(?:$v+)?$/" is [C](VC)**m[V] in perl
    // Matches if (m > 0)
    m_gt_0  :=  '^(?:' + c + '+)?(?:' + v + '+' + c + '+){1,}(?:' + v + '+)?$';
    
    // Matches if (m > 1)
    m_gt_1  :=  '^(?:' + c + '+)?(?:' + v + '+' + c + '+){2,}(?:' + v + '+)?$';
    
    // Matches if (m = 1)
    m_eq_1 := '^(?:' + c + '+)?(?:' + v + '+' + c + '+){1}(?:' + v + '+)?$';
    
    // Matches *o
    o := c + v + '(?:[^aiueowxy])$';
    
    // Matches *d
    d := '(' + c + ')\\1$';
    //---END Regular expressions -------------------------
    string stem( string WORD, string ENDING, string STEMEND, string precon1, string precon2='.') := FUNCTION
      string pre := regexreplace('^(.+)'+ENDING+'$', WORD, '$1');
      string stemmed0 := IF(length(STEMEND)=0, pre, pre+STEMEND);
      boolean precon := IF(
                           (length(precon1)>0) and (length(precon2)>0)
                           , regexfind(precon1, pre) and regexfind(precon2, pre)
                           , IF((length(precon1)>0) and (length(precon2)=0)
                                ,  regexfind(precon1, pre)
                                , false
                               )
                          );
      string stemmed := IF(precon, stemmed0, WORD);
    return stemmed;
    END;
    
    string stemMapForLetterA1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ational'  + '$', WORD) => stem( WORD, 'ational' , 'ate', m_gt_0, ''),
        regexfind('^.+' + 'tional'   + '$', WORD) => stem( WORD, 'tional'  , 'tion', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterC1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'enci'     + '$', WORD) => stem( WORD, 'enci'    , 'ence', m_gt_0, ''),
        regexfind('^.+' + 'anci'     + '$', WORD) => stem( WORD, 'anci'    , 'ance', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterE1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'izer'     + '$', WORD) => stem( WORD, 'izer'    , 'ize', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterG1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'logi'     + '$', WORD) => stem( WORD, 'logi'    , 'log', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterL1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'bli'      + '$', WORD) => stem( WORD, 'bli'     , 'ble', m_gt_0, ''),
        regexfind('^.+' + 'alli'     + '$', WORD) => stem( WORD, 'alli'    , 'al' , m_gt_0, ''),
        regexfind('^.+' + 'entli'    + '$', WORD) => stem( WORD, 'entli'   , 'ent', m_gt_0, ''),
        regexfind('^.+' + 'eli'      + '$', WORD) => stem( WORD, 'eli'     , 'e'  , m_gt_0, ''),
        regexfind('^.+' + 'ousli'    + '$', WORD) => stem( WORD, 'ousli'   , 'ous', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterO1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ization'  + '$', WORD) => stem( WORD, 'ization' , 'ize', m_gt_0, ''),
        regexfind('^.+' + 'ation'    + '$', WORD) => stem( WORD, 'ation'   , 'ate', m_gt_0, ''),
        regexfind('^.+' + 'ator'     + '$', WORD) => stem( WORD, 'ator'    , 'ate', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterS1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'alism'    + '$', WORD) => stem( WORD, 'alism'   , 'al' , m_gt_0, ''),
        regexfind('^.+' + 'iveness'  + '$', WORD) => stem( WORD, 'iveness' , 'ive', m_gt_0, ''),
        regexfind('^.+' + 'fulness'  + '$', WORD) => stem( WORD, 'fulness' , 'ful', m_gt_0, ''),
        regexfind('^.+' + 'ousness'  + '$', WORD) => stem( WORD, 'ousness' , 'ous', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterT1( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'aliti'    + '$', WORD) => stem( WORD, 'aliti'   , 'al' , m_gt_0, ''),
        regexfind('^.+' + 'iviti'    + '$', WORD) => stem( WORD, 'iviti'   , 'ive', m_gt_0, ''),
        regexfind('^.+' + 'biliti'   + '$', WORD) => stem( WORD, 'biliti'  , 'ble', m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterE2( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'icate'    + '$', WORD) => stem( WORD, 'icate'   , 'ic' , m_gt_0, ''),
        regexfind('^.+' + 'ative'    + '$', WORD) => stem( WORD, 'ative'   , ''   , m_gt_0, ''),
        regexfind('^.+' + 'alize'    + '$', WORD) => stem( WORD, 'alize'   , 'al' , m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterI2( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'iciti'    + '$', WORD) => stem( WORD, 'iciti'   , 'ic' , m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterL2( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ical'     + '$', WORD) => stem( WORD, 'ical'    , 'ic' , m_gt_0, ''),
        regexfind('^.+' + 'ful'      + '$', WORD) => stem( WORD, 'ful'     , ''   , m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterS2( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ness'     + '$', WORD) => stem( WORD, 'ness'    , ''   , m_gt_0, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterA3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'al'       + '$', WORD) => stem( WORD, 'al'      , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterC3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ance'     + '$', WORD) => stem( WORD, 'ance'    , ''   , m_gt_1, ''),
        regexfind('^.+' + 'ence'     + '$', WORD) => stem( WORD, 'ence'    , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterE3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'er'       + '$', WORD) => stem( WORD, 'er'      , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterI3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ic'       + '$', WORD) => stem( WORD, 'ic'      , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterL3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'able'     + '$', WORD) => stem( WORD, 'able'    , ''   , m_gt_1, ''),
        regexfind('^.+' + 'ible'     + '$', WORD) => stem( WORD, 'ible'    , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterN3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ant'      + '$', WORD) => stem( WORD, 'ant'     , ''   , m_gt_1, ''),
        regexfind('^.+' + 'ement'    + '$', WORD) => stem( WORD, 'ement'   , ''   , m_gt_1, ''),
        regexfind('^.+' + 'ment'     + '$', WORD) => stem( WORD, 'ment'    , ''   , m_gt_1, ''),
        regexfind('^.+' + 'ent'      + '$', WORD) => stem( WORD, 'ent'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterO3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ion'      + '$', WORD) => stem( WORD, 'ion'     , ''   , '[st]$', m_gt_1),
        regexfind('^.+' + 'ou'       + '$', WORD) => stem( WORD, 'ou'      , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterS3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ism'      + '$', WORD) => stem( WORD, 'ism'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterT3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ate'      + '$', WORD) => stem( WORD, 'ate'     , ''   , m_gt_1, ''),
        regexfind('^.+' + 'iti'      + '$', WORD) => stem( WORD, 'iti'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterU3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ous'      + '$', WORD) => stem( WORD, 'ous'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterV3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ive'      + '$', WORD) => stem( WORD, 'ive'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    string stemMapForLetterZ3( string WORD) := FUNCTION
      stemmed := map(
        regexfind('^.+' + 'ize'      + '$', WORD) => stem( WORD, 'ize'     , ''   , m_gt_1, ''),
        WORD
       );
      return stemmed;
    END;
    
    //STEP 1a
    word1a := 
       IF( regexfind('^.+sses$', WORD)
           , regexreplace('^(.+)sses$',WORD, '$1ss')
           , IF( regexfind('^.+ies$', WORD)
                 , regexreplace('^(.+)ies$',WORD, '$1i')
                 , IF( regexfind('^.+[^s]s$', WORD)
                       , regexreplace('^(.+[^s])s$',WORD, '$1')
                       , WORD
                     )
               )
         );

    //STEP 1b
    word1_eed := regexreplace('^(.+)eed$', word1a, '$1');
    word1_ed := regexreplace('^(.*[^e])ed$', word1a, '$1');
    word1_ing := regexreplace('^(.+)ing$', word1a, '$1');
    word1b :=
       IF( regexfind('^.+eed$', word1a) and regexfind(m_gt_0,word1_eed)
           , regexreplace('^(.+)eed$',word1a, '$1ee')
           , IF( regexfind('^.+[^e]ed$', word1a) and regexfind(v,word1_ed)
                 , regexreplace('^(.+)ed$',word1a, '$1')
                 , IF( regexfind('^.+ing$', word1a) and regexfind(v,word1_ing)
                      , regexreplace('^(.+)ing$',word1a, '$1')
                      , word1a
                     )
               )
         );

    extra := IF( ( regexfind('^.*[^e]ed$', word1a) and regexfind(v,word1_ed) )
                 or ( regexfind('^.+ing$', word1a) and regexfind(v,word1_ing) )
                 , true
                 , false
               );

    // If 2nd or 3rd of the previous rules was successful try the extra rules...
    wordextra :=
       IF( not extra, word1b
           , IF( regexfind('.+at$', word1b)
                 , regexreplace('(.+)at$',word1b, '$1ate')
                 , IF( regexfind('.+iz$', word1b)
                       , regexreplace('(.+)iz$',word1b, '$1ize')
                       , IF( regexfind('.+bl$', word1b)
                             , regexreplace('(.+)bl$',word1b, '$1ble')
                             , IF( regexfind(d, word1b) and not regexfind('[lsz]$', word1b)
                                   , word1b[ 1 .. (length(word1b)-1)]
                                   , IF( regexfind(m_eq_1, word1b) and regexfind(o, word1b)
                                        , word1b + 'e'
                                        , word1b
                                       )
                                 )
                           )
                     )
               )
         );
  
  //STEP 1c
  word1c := IF(regexfind('y$', wordextra),stem( wordextra, 'y' , 'i', v, ''), wordextra);

  //STEP 2
  letter2 := word1c[length(word1c)-1];
  word2 :=
     MAP(
         letter2='a' => stemMapForLetterA1(word1c),
         letter2='c' => stemMapForLetterC1(word1c),
         letter2='e' => stemMapForLetterE1(word1c),
         letter2='g' => stemMapForLetterG1(word1c),
         letter2='l' => stemMapForLetterL1(word1c),
         letter2='o' => stemMapForLetterO1(word1c),
         letter2='s' => stemMapForLetterS1(word1c),
         letter2='t' => stemMapForLetterT1(word1c),
         word1c
        );

  //STEP 3
  letter3 := word2[length(word2)-0];
  word3 :=
     MAP(
         letter3='e' => stemMapForLetterE2(word2),
         letter3='i' => stemMapForLetterI2(word2),
         letter3='l' => stemMapForLetterL2(word2),
         letter3='s' => stemMapForLetterS2(word2),
         word2
        );

  //STEP 4
  letter4 := word3[length(word3)-1];
  word4 :=
     MAP(
         letter4='a' => stemMapForLetterA3(word3),
         letter4='c' => stemMapForLetterC3(word3),
         letter4='e' => stemMapForLetterE3(word3),
         letter4='i' => stemMapForLetterI3(word3),
         letter4='l' => stemMapForLetterL3(word3),
         letter4='n' => stemMapForLetterN3(word3),
         letter4='o' => stemMapForLetterO3(word3),
         letter4='s' => stemMapForLetterS3(word3),
         letter4='t' => stemMapForLetterT3(word3),
         letter4='u' => stemMapForLetterU3(word3),
         letter4='v' => stemMapForLetterV3(word3),
         letter4='z' => stemMapForLetterZ3(word3),
         word3
        );
  

  string Step5a( string WORD) := FUNCTION
    string pre := regexreplace('^(.+)e$', WORD, '$1');
    string stemmed :=
        IF(regexfind(m_gt_1, pre) or ( regexfind(m_eq_1, pre) and not regexfind(o, pre)), pre, WORD);
  return stemmed;
  END;
    
  //STEP 5a
  word5a := Step5a( word4 );
 
  //STEP 5b
  word5b :=
     IF(regexfind('ll$', word5a) and regexfind(m_gt_1, word5a)
     , word5a[ 1 .. (length(word5a)-1)]
     , word5a);

/*
output(word1a,named('word1a'));    
output(word1b,named('word1b'));    
output(wordextra,named('wordextra'));    
output(word1c,named('word1c'));    
output(word2,named('word2'));    
output(word3,named('word3'));    
output(word4,named('word4'));    
output(word5a,named('word5a'));    
output(word5b,named('word5b')); 
*/   
return IF(length(WORD)<3, WORD,word5b);
END;
