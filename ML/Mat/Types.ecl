﻿EXPORT Types := MODULE

// Note - indices will start at 1; 0 is going to be used as a null
EXPORT t_Index := UNSIGNED4; // Supports matrices with up to 9B as the largest dimension
EXPORT t_value := REAL8;
EXPORT t_mu_no := UNSIGNED2; // Allow up to 64K matrices in one universe

EXPORT Element := RECORD
  t_Index x;
	t_Index y;
	t_value value;
END;

EXPORT MUElement := RECORD(Element)
	t_mu_no no; // The number of the matrix within the universe
END;

END;