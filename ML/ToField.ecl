//---------------------------------------------------------------------------
// Macro takes a matrix dataset, with each row contianing an ID and one or
// more axis fields containing numeric values, and expands it into the 
// NumericField format used by ML.
//   dIn       : The name of the input dataset
//   dOut      : The name of the resulting dataset
//   idfield   : [OPTIONAL] The name of the field that contains the UID for
//               each row.  If omitted, it is assumed to be the first field.
//   datafields: [OPTIONAL] A STRING contianing a comma-delimited list of the
//               fields to be treated as axes.  If omitted, all numeric
//               fields that are not the UID will be treated as axes.
//               NOTE: idfield defaults to the first field in the table, so
//               if that field is specified as an axis field, then the user
//               should be sure to specify a value in the idfield param.
//   dMap      : [OPTIONAL] If the user wants to keep a mapping between the
//               original table's fields and the numbers assigned to them
//               in the NumericField format, this is the name of the dataset
//               to contain that map.
//  Examples:
//    ML.ToField(dOrig,dMatrix);
//    ML.ToField(dOrig,dMatrix,myid,'field5,field7,field10',dMap);
//---------------------------------------------------------------------------
EXPORT ToField(dIn,dOut,idfield='',datafields='',dMap=''):=MACRO
  LOADXML('<xml/>');
  // Variable to contain the name of the field that maps to "id"
  #DECLARE(foundidfield) #SET(foundidfield,#TEXT(idfield))
  // Contains a comma-delimited list of the fields that will be used as axes,
  // beginning with "COUNTER" so it can be #EXPANDED into a CHOOSE call
  #DECLARE(fieldlist) #SET(fieldlist,'COUNTER')
  // Count of the fields that become axes
  #DECLARE(iNumberOfFields) #SET(iNumberOfFields,0)
  // A list of every field in the original table and the field number (or "ID")
  // to which it is mapped in the output.  "NA" indicates that the field was
  // not mapped.  The string is formatted so it can be easily #EXPANDED into 
  // the data portion of a DATASET assignment.
  #DECLARE(mapping) #SET(mapping,'')
  // Loop through the layout of the input table to pick the fields and
  // produce the mapping
  #DECLARE(iPivotLoop) #SET(iPivotLoop,0)
  #EXPORTXML(fields,RECORDOF(dIn))
  #FOR(fields)
    #FOR(Field)
      #IF(%'foundidfield'%='' AND %iPivotLoop%=0)
        #SET(foundidfield,%'{@label}'%);
        #APPEND(mapping,',{\''+%'{@label}'%+'\',\'ID\'}')
      #ELSE
        #IF(%'{@label}'%=#TEXT(idfield))
          #APPEND(mapping,',{\''+%'{@label}'%+'\',\'ID\'}')
        #ELSE
          #IF(REGEXREPLACE('[^a-z]',%'{@type}'%,'') IN ['unsigned','integer','real','decimal','udecimal'] #IF(#TEXT(datafields)!='') AND REGEXFIND(','+%'{@label}'%+',',','+datafields+',',NOCASE) #END)
            #APPEND(fieldlist,',(ML.Types.t_FieldReal)LEFT.'+%'{@label}'%)
            #SET(iNumberOfFields,%iNumberOfFields%+1)
            #APPEND(mapping,',{\''+%'{@label}'%+'\',\''+%'iNumberOfFields'%+'\'}')
          #ELSE
            #APPEND(mapping,',{\''+%'{@label}'%+'\',\'NA\'}')
          #END
        #END
      #END
      #SET(iPivotLoop,%iPivotLoop%+1)
    #END
  #END
  // Produce the output, with one row for every id/axis combination.
  dOut:=NORMALIZE(dIn,%iNumberOfFields%,TRANSFORM(ML.Types.NumericField,SELF.id:=LEFT.#EXPAND(%'foundidfield'%),SELF.number:=COUNTER;SELF.value:=CHOOSE(#EXPAND(%'fieldlist'%))));
  // If the user requested a mapping table produce the dataset that contains it
  #IF(#TEXT(dMap)!='')
    dMap:=#EXPAND('DATASET(['+%'mapping'%[2..]+'],{STRING orig_name;STRING assigned_name;})');
  #END
ENDMACRO;