      *  ERROR MESSAGES TABLE  *
       01  ERR-MSG-INFO.
           05  FILLER          PIC X(63)   VALUE 
       'CAMPGROUND NEEDS TO BE BUCK CREEK, HONEY CREEK, OR ISLAND VIEW'.
           05  FILLER          PIC X(63)   VALUE 
           'SITE MUST HAVE 1 LETTER AND 2 DIGITS'.
           05  FILLER          PIC X(63)   VALUE 
           'DIGITS IN A SITE MUST BE GREATER THAN 0'.
           05  FILLER          PIC X(63)   VALUE 
           'A STAY MUST BE NUMERIC'.
           05  FILLER          PIC X(63)   VALUE 
           'A STAY MUST BE GREATER THAN 1 DAY AND LESS THAN 12 DAYS'.
           05  FILLER          PIC X(63)   VALUE 'DATE MUST BE NUMERIC'.
           05  FILLER          PIC X(63)   VALUE 
           'MONTH MUST BE A NUMBER 1 THROUGH 12'.
           05  FILLER          PIC X(63)   VALUE 
           'YEAR CANNOT BE LESS THAN THE CURRENT YEAR'.
           05  FILLER          PIC X(63)   VALUE 
           '30 DAY MONTHS ARE 4, 6, 9, AND 11'.
           05  FILLER          PIC X(63)   VALUE 
           '31 DAY MONTHS ARE 1, 3, 5, 7, 8, 10, AND 12'.
           05  FILLER          PIC X(63)   VALUE 
           'MONTH 2 HAS 29 DAYS WHEN ON A LEAP YEAR'.
           05  FILLER          PIC X(63)   VALUE 
           'MONTH 2 HAS 28 DAYS WHEN NOT ON A LEAP YEAR'.
           05  FILLER          PIC X(63)   VALUE 
           'DATE MUST BE GREATER THAN THE CURRENT DATE'.
           05  FILLER          PIC X(63)   VALUE 
           'LAST NAME IS REQUIRED'.
           05  FILLER          PIC X(63)   VALUE 
           'FIRST NAME IS REQUIRED'.
           05  FILLER          PIC X(63)   VALUE 
           'THE AMOUNT ENTERED IS NOT NUMERIC'.
           05  FILLER          PIC X(63)   VALUE 
           'THE AMOUNT ENTERED IS INACCURATE FOR THAT SITE'.
           05  FILLER          PIC X(63)   VALUE 
           'CREDIT CARD TYPE MUST BE V, M, OR A'.
           05  FILLER          PIC X(63)   VALUE 
           'CREDIT CARD NUMBER MUST BE NUMERIC'.
           05  FILLER          PIC X(63)   VALUE 
           'EXPIRATION DATE MUST BE NUMERIC'.
           05  FILLER          PIC X(63)   VALUE 
           'THE MONTH OF EXPIRATION MUST BE A NUMBER 1 THROUGH 12'.
           05  FILLER          PIC X(63)   VALUE 
          'THE YEAR OF EXPIRATION CANNOT BE LESS THAN THE CURRENT YEAR'.
           05  FILLER          PIC X(63)   VALUE 
           '30 DAY MONTH EXPIRATION DATES ARE 4, 6, 9, AND 11'.
           05  FILLER          PIC X(63)   VALUE 
          '31 DAY MONTH EXPIRATION DATES ARE 1, 3, 5, 7, 8, 10, AND 12'.
           05  FILLER          PIC X(63)   VALUE 
           'EXPIRATION MONTHS WITH 2 HAVE 29 DAYS WHEN ON A LEAP YEAR'.
           05  FILLER          PIC X(63)   VALUE 
        'EXPIRATION MONTHS WITH 2 HAVE 28 DAYS WHEN NOT ON A LEAP YEAR'.
           05  FILLER          PIC X(63)   VALUE 
           'EXPIRATION DATE MUST BE GREATER THAN THE CURRENT DATE'.
          
       01  ERR-MSG-TABLE REDEFINES ERR-MSG-INFO.
           05  T-ERR-MSG       PIC X(63)   OCCURS 27.