       IDENTIFICATION DIVISION.  
       PROGRAM-ID.     COBLSC01.
       AUTHOR.         LUCAS CHAPMAN.
       DATE-WRITTEN.   3/3/2020.
       DATE-COMPILED.
      ******************************************************************
      *  VALIDATES RESERVATIONS FOR RESERVATIONS FOR RATHBUN LAKE      *
      *  CAMPGROUNDS.                                                  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TRAN-MASTER
               ASSIGN TO 
               'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\RESERVE.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT VALOUT
               ASSIGN TO 
               'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\CAMPRES.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROUT
               ASSIGN TO
               'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\ERR.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  TRAN-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS TRAN-REC
           RECORD CONTAINS 108 CHARACTERS.

      *  CPY FILE THAT CONATAINS TRAN-REC LAYOUT  *
           COPY 'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\TRAN.CPY'.

       FD  VALOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 136 CHARACTERS
           DATA RECORD IS VAL-REC
           LINAGE IS 60 WITH FOOTING AT 56.
               
      *  CPY FILE THAT CONATAINS VAL-REC LAYOUT  *
           COPY 'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\VAL.CPY'.

       FD  ERROUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERRLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  ERRLINE             PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  MORE-RECS       PIC X       VALUE 'Y'.
           05  C-ERR-CTR       PIC 99      VALUE 0.
           05  C-TOT-ERR-CTR   PIC 99      VALUE 0.
           05  FIRST-ERR-SW    PIC X.
           05  END-DATE-SW     PIC X.
           05  AMT-SW          PIC X.
           05  VAL-MM-SW       PIC X.
           05  DATE-TYPE       PIC X.
           05  C-LEAP-YYYY     PIC 99      VALUE 0.      
           05  C-END-YYYY      PIC 9(4).    
           05  C-END-MM        PIC 99.            
           05  C-END-DD        PIC 99.         
           05  C-AMT           PIC S9(3)V99.
           05  WS-CCNUM1ST     PIC X(4).
           05  WS-CCNUM2ND     PIC X(4).
           05  WS-CCNUM3RD     PIC X(4).
           05  WS-CCNUM4TH     PIC X(4).
           05  WS-CURR-DATE.
               10  WS-CURR-YY  PIC 9(4).
               10  WS-CURR-MM  PIC 99.
               10  WS-CURR-DD  PIC 99.
           05  ERR-PCTR        PIC 99      VALUE 0.

       01  CURRENT-DATE-AND-TIME.
           05  CURR-DATE.
               10  CURR-YY    PIC 9(4).
               10  CURR-MM    PIC 99.
               10  CURR-DD    PIC 99.
           05  CURR-TIME      PIC X(11).

      *  USED FOR VALIDATING DATES  *
       01  WS-DATE.
           05  WS-YYYY         PIC 9(4).
           05  WS-MM           PIC 99.
               88  VAL-MM                  VALUE 1 THRU 12.
               88  VAL-MM-31               VALUE 1 3 5 7 8 10 12.
               88  VAL-MM-30               VALUE 4 6 9 11.
           05  WS-DD           PIC 99.
               88  VAL-DD-31               VALUE 1 THRU 31.
               88  VAL-DD-30               VALUE 1 THRU 30.
               88  VAL-DD-29               VALUE 1 THRU 29.
               88  VAL-DD-28               VALUE 1 THRU 28.
       01  WS-DATE-NUM REDEFINES WS-DATE   PIC 9(8).

      *  SITES TABLE  *
       01  SITES-INFO.
           05  FILLER          PIC X(5)    VALUE 'A1000'.
           05  FILLER          PIC X(5)    VALUE 'B1000'.
           05  FILLER          PIC X(5)    VALUE 'C1000'.
           05  FILLER          PIC X(5)    VALUE 'D1200'.
           05  FILLER          PIC X(5)    VALUE 'E1200'.
           05  FILLER          PIC X(5)    VALUE 'F1200'.
           05  FILLER          PIC X(5)    VALUE 'G1200'.
           05  FILLER          PIC X(5)    VALUE 'H1200'.
           05  FILLER          PIC X(5)    VALUE 'I1200'.
           05  FILLER          PIC X(5)    VALUE 'J1200'.
           05  FILLER          PIC X(5)    VALUE 'K1200'.
           05  FILLER          PIC X(5)    VALUE 'L1200'.
           05  FILLER          PIC X(5)    VALUE 'M1200'.
           05  FILLER          PIC X(5)    VALUE 'N1400'.
           05  FILLER          PIC X(5)    VALUE 'O1400'.
           05  FILLER          PIC X(5)    VALUE 'P1400'.
           05  FILLER          PIC X(5)    VALUE 'Q1400'.
           05  FILLER          PIC X(5)    VALUE 'R1400'.
           05  FILLER          PIC X(5)    VALUE 'S1400'.
           05  FILLER          PIC X(5)    VALUE 'T1400'.
           05  FILLER          PIC X(5)    VALUE 'U1400'.
           05  FILLER          PIC X(5)    VALUE 'V1400'.
           05  FILLER          PIC X(5)    VALUE 'W1400'.
           05  FILLER          PIC X(5)    VALUE 'X1400'.
           05  FILLER          PIC X(5)    VALUE 'Y1400'.
           05  FILLER          PIC X(5)    VALUE 'Z1400'.

       01  SITES-TABLE REDEFINES SITES-INFO.
           05  T-SITE          OCCURS 26 INDEXED BY SITE-INDEX.
               10  T-LETTER    PIC X.
               10  T-PRICE     PIC S99V99.

       01  ERR-REPORT-TITLE.
           05  FILLER          PIC X(6)    VALUE 'DATE: '.
		   05  O-ERR-MM        PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-ERR-DD        PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-ERR-YY        PIC 9(4).
           05  FILLER          PIC X(44)   VALUE ' '.
           05  FILLER          PIC X(12)   VALUE 'ERROR REPORT'.
           05  FILLER          PIC X(52)   VALUE ' '.
           05  FILLER          PIC X(6)    VALUE 'PAGE: '.
           05  O-ERR-PCTR      PIC Z9.

       01  BLANK-LINE.
           05  FILLER          PIC X(132)  VALUE ' '.

      *  LINE THAT WRITES INVALID RECORDS TO ERR.PRT  *
       01  ERR-LINE.
           05  O-ERR-REC       PIC X(108).
           05  FILLER          PIC X(24)       VALUE ' '.

      *  CPY FILE THAT CONTAINS ALL ERROR MESSAGES AND IT'S TABLE  *
       COPY 'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC01\ERROR.CPY'.

       01  ERR-MSG-LINE.
           05  O-ERR-MSG       PIC X(63).
           05  FILLER          PIC X(69).
           
      *  DISPLAY THE TOTAL NUMBER OF INVALID RECORDS  *
       01  ERR-TOTAL-LINE-1.
           05  FILLER          PIC X(15)   VALUE 'TOTAL RECORDS: '.
           05  O-ERR-CTR       PIC Z9.
           05  FILLER          PIC X(115)  VALUE ' '.

      *  DISPLAYS THE TOTAL NUMBER OF ERRORS FOUND ON EACH RECORD  *
       01  ERR-TOTAL-LINE-2.
           05  FILLER          PIC X(14)   VALUE 'TOTAL ERRORS: '.
           05  O-TOT-ERR-CTR   PIC ZZ9.
           05  FILLER          PIC X(115)  VALUE ' '.

       PROCEDURE DIVISION.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L4-MAINLINE
               UNTIL MORE-RECS = 'N'.
           PERFORM L9-CLOSING.
       STOP RUN.

       L2-INIT.
           OPEN INPUT TRAN-MASTER.
           OPEN OUTPUT VALOUT.
           OPEN OUTPUT ERROUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CURR-DATE TO WS-CURR-DATE.
           MOVE CURR-YY TO O-ERR-YY.
           MOVE CURR-DD TO O-ERR-DD.
           MOVE CURR-MM TO O-ERR-MM.

           PERFORM L3-READ.

           ADD 1 TO ERR-PCTR.
           MOVE ERR-PCTR TO O-ERR-PCTR. 

           WRITE ERRLINE FROM ERR-REPORT-TITLE
               AFTER ADVANCING PAGE.

       L3-READ.
           READ TRAN-MASTER
               AT END
                   MOVE 'N' TO MORE-RECS.

       L4-MAINLINE.
           PERFORM L5-VALIDATION.
           IF TRAN-REC NOT= O-ERR-REC
               PERFORM L8-OUTPUT
               PERFORM L3-READ
           ELSE
               PERFORM L3-READ.

       L5-VALIDATION.
      *  PREVENTS ERR-REC-CTR FROM ADD EVERY TIME AN ERROR IS FOUND  *
           MOVE 'Y' TO FIRST-ERR-SW.
      *  SKIPS END-DATE CHECK IF RECORD IS INVALID  *
           MOVE 'N' TO END-DATE-SW.
      *  SKIPS AMOUNT CHECK IF A RECORD IN INVAILID  * 
           MOVE 'N' TO AMT-SW.

           IF NOT VAL-CAMP
	          MOVE T-ERR-MSG(1) TO O-ERR-MSG
              MOVE 'Y' TO END-DATE-SW
		      PERFORM L6-ERR-ROUT.

           IF I-SITE-LET NOT ALPHABETIC
               MOVE T-ERR-MSG(2) TO O-ERR-MSG
               MOVE 'Y' TO END-DATE-SW
               MOVE 'Y' TO AMT-SW
               PERFORM L6-ERR-ROUT
           ELSE
               IF I-SITE-LET = ' '
                   MOVE T-ERR-MSG(2) TO O-ERR-MSG
                   MOVE 'Y' TO END-DATE-SW
                   MOVE 'Y' TO AMT-SW
                   PERFORM L6-ERR-ROUT.

           IF I-SITE-NUM NOT NUMERIC
               MOVE T-ERR-MSG(2) TO O-ERR-MSG
               MOVE 'Y' TO END-DATE-SW
               MOVE 'Y' TO AMT-SW
               PERFORM L6-ERR-ROUT
           ELSE
               IF I-SITE-NUM = 0
                   MOVE T-ERR-MSG(3) TO O-ERR-MSG
                   MOVE 'Y' TO END-DATE-SW
                   MOVE 'Y' TO AMT-SW
                   PERFORM L6-ERR-ROUT.

           IF I-LEN-STAY NOT NUMERIC
               MOVE T-ERR-MSG(4) TO O-ERR-MSG
               MOVE 'Y' TO END-DATE-SW
               MOVE 'Y' TO AMT-SW
               PERFORM L6-ERR-ROUT
           ELSE
               IF NOT VAL-STAY
                   MOVE T-ERR-MSG(5) TO O-ERR-MSG
                   MOVE 'Y' TO END-DATE-SW
                   MOVE 'Y' TO AMT-SW
                   PERFORM L6-ERR-ROUT.

           MOVE I-DATE TO WS-DATE.
           MOVE 'R' TO DATE-TYPE.
           PERFORM L7-DATE-TYPE
               THRU L7-DATE-ROUT-EXIT.

           IF I-LNAME = ' '
               MOVE T-ERR-MSG(14) TO O-ERR-MSG
               PERFORM L6-ERR-ROUT.

           IF I-FNAME = ' '
               MOVE T-ERR-MSG(15) TO O-ERR-MSG
               PERFORM L6-ERR-ROUT.

           IF I-AMT NOT NUMERIC
               MOVE T-ERR-MSG(16) TO O-ERR-MSG
               PERFORM L6-ERR-ROUT
           ELSE
               IF AMT-SW = 'Y'
                   NEXT SENTENCE
               ELSE
                   SET SITE-INDEX TO 1
                   SEARCH T-SITE
                       WHEN T-LETTER(SITE-INDEX) = I-SITE-LET
                           MULTIPLY T-PRICE(SITE-INDEX) BY I-LEN-STAY 
                           GIVING C-AMT
                           IF C-AMT NOT = I-AMT
                               MOVE T-ERR-MSG(17) TO O-ERR-MSG
                               PERFORM L6-ERR-ROUT.

           IF NOT VAL-CCTYPE
               MOVE T-ERR-MSG(18) TO O-ERR-MSG
               PERFORM L6-ERR-ROUT.

           IF I-CCNUM NOT NUMERIC
               MOVE T-ERR-MSG(19) TO O-ERR-MSG
               PERFORM L6-ERR-ROUT.

           MOVE I-CCEXP TO WS-DATE.
           MOVE 'C' TO DATE-TYPE.
           PERFORM L7-DATE-TYPE
               THRU L7-DATE-ROUT-EXIT.

       L6-ERR-ROUT.
           IF FIRST-ERR-SW = 'Y'
               MOVE 'N' TO FIRST-ERR-SW
      *  ADDS ONE WHEN FIRST ERROR IS FOUND  *
               ADD 1 TO C-ERR-CTR
               MOVE TRAN-REC TO O-ERR-REC
               WRITE ERRLINE FROM ERR-LINE
                   AFTER ADVANCING 2 LINES
               WRITE ERRLINE FROM ERR-MSG-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           ADD 1 TO ERR-PCTR
                           MOVE ERR-PCTR TO O-ERR-PCTR
                           WRITE ERRLINE FROM ERR-REPORT-TITLE
                               AFTER ADVANCING PAGE
                           WRITE ERRLINE FROM BLANK-LINE
                               AFTER ADVANCING 1 LINE
           ELSE
               WRITE ERRLINE FROM ERR-MSG-LINE
                   AFTER ADVANCING 1 LINE.
      *  ALWAYS ADDS 1 *
               ADD 1 TO C-TOT-ERR-CTR.

       L7-DATE-TYPE.
      *  SWITCHES USED TO SKIP CERTAIN CHECKS *
           MOVE 'N' TO VAL-MM-SW.

           IF WS-DATE-NUM NOT NUMERIC
               IF DATE-TYPE = 'R'
                   MOVE T-ERR-MSG(6) TO O-ERR-MSG
               ELSE
                   MOVE T-ERR-MSG(20) TO O-ERR-MSG
               END-IF
               PERFORM L6-ERR-ROUT
               GO TO L7-DATE-ROUT-EXIT.

           IF NOT VAL-MM
               IF DATE-TYPE ='R'
                   MOVE T-ERR-MSG(7) TO O-ERR-MSG
               ELSE
                   MOVE T-ERR-MSG(21) TO O-ERR-MSG
               END-IF
               PERFORM L6-ERR-ROUT
               GO TO L7-DATE-ROUT-EXIT.

           IF WS-YYYY < CURR-YY
               IF DATE-TYPE ='R'
                   MOVE T-ERR-MSG(8) TO O-ERR-MSG
               ELSE
                   MOVE T-ERR-MSG(22) TO O-ERR-MSG
               END-IF
               PERFORM L6-ERR-ROUT
               GO TO L7-DATE-ROUT-EXIT.

           IF VAL-MM-30 AND NOT VAL-DD-30
               IF DATE-TYPE = 'R'
                   MOVE T-ERR-MSG(9) TO O-ERR-MSG
               ELSE
                   MOVE T-ERR-MSG(23) TO O-ERR-MSG
               END-IF
               MOVE 'Y' TO END-DATE-SW
               MOVE 'Y' TO VAL-MM-SW
               PERFORM L6-ERR-ROUT
           ELSE
               IF VAL-MM-31 AND NOT VAL-DD-31
                   IF DATE-TYPE = 'R'
                       MOVE T-ERR-MSG(10) TO O-ERR-MSG
                   ELSE
                       MOVE T-ERR-MSG(24) TO O-ERR-MSG
                   END-IF
                   MOVE 'Y' TO END-DATE-SW
                   MOVE 'Y' TO VAL-MM-SW
                   PERFORM L6-ERR-ROUT.

           IF WS-MM = 2
               DIVIDE WS-YYYY BY 4 GIVING C-LEAP-YYYY REMAINDER 
               C-LEAP-YYYY
               IF C-LEAP-YYYY = 0
                   IF NOT VAL-DD-29
                       IF DATE-TYPE = 'R'
                           MOVE T-ERR-MSG(11) TO O-ERR-MSG
                       ELSE
                           MOVE T-ERR-MSG(25) TO O-ERR-MSG
                       END-IF
                       MOVE 'Y' TO END-DATE-SW
                       MOVE 'Y' TO VAL-MM-SW
                       PERFORM L6-ERR-ROUT
                   ELSE
                       NEXT SENTENCE
                   END-IF
               ELSE
                   IF NOT VAL-DD-28
                       IF DATE-TYPE = 'R'
                           MOVE T-ERR-MSG(12) TO O-ERR-MSG
                       ELSE
                           MOVE T-ERR-MSG(26) TO O-ERR-MSG
                       END-IF
                       MOVE 'Y' TO END-DATE-SW
                       MOVE 'Y' TO VAL-MM-SW
                       PERFORM L6-ERR-ROUT.

      *  IS SKIPPED IF THE MONTH IS INVALID  *
           IF VAL-MM-SW = 'Y'
               NEXT SENTENCE
           ELSE
               IF WS-DATE-NUM <= WS-CURR-DATE
                   IF DATE-TYPE ='R'
                       MOVE T-ERR-MSG(13) TO O-ERR-MSG
                   ELSE
                       MOVE T-ERR-MSG(27) TO O-ERR-MSG
                   END-IF
                   MOVE 'Y' TO END-DATE-SW
                   PERFORM L6-ERR-ROUT.

      *  IS SKIPED IF ON EXPIRATION DATE OR AN ERROR IS FOUND  *
           IF DATE-TYPE = 'R' AND END-DATE-SW = 'N' 
               ADD WS-DD TO I-LEN-STAY GIVING C-END-DD
               MOVE WS-MM TO C-END-MM
               MOVE WS-YYYY TO C-END-YYYY

               EVALUATE C-END-DD
                   WHEN > 31 AND VAL-DD-31 AND VAL-MM-31
                       ADD 1 TO C-END-MM
                       SUBTRACT WS-DD FROM C-END-DD
                       MOVE C-END-DD TO O-END-DD
                       IF C-END-MM > 12 AND WS-MM = 12
                           MOVE 1 TO C-END-MM
                           ADD 1 TO C-END-YYYY
                       END-IF
                       MOVE C-END-MM TO O-END-MM
                       MOVE C-END-YYYY TO O-END-YYYY
                   WHEN > 30 AND VAL-DD-30 AND VAL-MM-30
                       ADD 1 TO C-END-MM
                       SUBTRACT WS-DD FROM C-END-DD
                       MOVE C-END-DD TO O-END-DD
                       MOVE C-END-MM TO O-END-MM
                       MOVE C-END-YYYY TO O-END-YYYY
                   WHEN > 28 AND VAL-DD-28 AND C-LEAP-YYYY > 0 AND 
                   WS-MM = 2
                       ADD 1 TO C-END-MM
                       SUBTRACT WS-DD FROM C-END-DD
                       MOVE C-END-DD TO O-END-DD
                       MOVE C-END-MM TO O-END-MM
                       MOVE C-END-YYYY TO O-END-YYYY
                   WHEN > 29 AND VAL-DD-29 AND C-LEAP-YYYY = 0 AND 
                   WS-MM = 2
                       ADD 1 TO C-END-MM
                       SUBTRACT WS-DD FROM C-END-DD
                       MOVE C-END-DD TO O-END-DD
                       MOVE C-END-MM TO O-END-MM
                       MOVE C-END-YYYY TO O-END-YYYY
                   WHEN OTHER
                       MOVE C-END-DD TO O-END-DD
                       MOVE C-END-MM TO O-END-MM
                       MOVE C-END-YYYY TO O-END-YYYY.

       L7-DATE-ROUT-EXIT.
           EXIT.

       L8-OUTPUT.
           MOVE I-CAMPGROUND TO O-CAMPGROUND.
           MOVE I-SITE TO O-SITE.
           MOVE I-LEN-STAY TO O-LEN-STAY.
           MOVE I-DATE TO O-DATE.
           MOVE I-CCNUM1ST TO WS-CCNUM1ST.
           MOVE I-CCNUM2ND TO WS-CCNUM2ND.
           MOVE I-CCNUM3RD TO WS-CCNUM3RD.
           MOVE I-CCNUM4TH TO WS-CCNUM4TH.
           MOVE I-AMT TO O-AMT.
           MOVE I-CCEXP TO O-CCEXP.

           IF I-CCTYPE = 'V'
               MOVE 'VISA' TO O-CCTYPE
           ELSE
               IF I-CCTYPE = 'M'
                   MOVE 'MASTER CARD' TO O-CCTYPE
               ELSE
                   MOVE 'AMERICAN EXPRESS' TO O-CCTYPE.

      *  CLEARS PREVIOUS VALUES IN O-NAME *
           MOVE ' ' TO O-NAME.
           STRING I-LNAME DELIMITED BY ' '
               ', ' DELIMITED BY SIZE
               I-FNAME DELIMITED BY ' '
           INTO O-NAME.

           STRING WS-CCNUM1ST DELIMITED BY ' '
               '-' DELIMITED BY SIZE
               WS-CCNUM2ND DELIMITED BY ' '
               '-' DELIMITED BY SIZE
               WS-CCNUM3RD DELIMITED BY ' '
               '-' DELIMITED BY SIZE
               WS-CCNUM4TH DELIMITED BY ' '
           INTO O-CCNUM.

      *  WRITES DATA TO VAL-REC  *
           WRITE VAL-REC.

       L9-CLOSING.
           MOVE C-ERR-CTR TO O-ERR-CTR.    
           MOVE C-TOT-ERR-CTR TO O-TOT-ERR-CTR.    

           WRITE ERRLINE FROM ERR-TOTAL-LINE-1
               AFTER ADVANCING 3 LINES.
           WRITE ERRLINE FROM ERR-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.

           CLOSE TRAN-MASTER. 
           CLOSE VALOUT. 
           CLOSE ERROUT.