	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. COBLSC04.
	   AUTHOR. LUCAS CHAPMAN.
	   DATE-WRITTEN. 4/14/2020.
	   DATE-COMPILED.
      ******************************************************************
      *  WRITES TWO REPORTS FOR JUST FITS FURNITURE EMPORIUM WITH THE  *
	  *  FIRST REPORT BEING A WEEKLY SALES REPORT OF 15 SALESPERSONS   *
	  *  USING TWO DIMESIONAL TABLES AND USES ONE DIMENSIONAL TABLES   *
      *  FOR THE WEEKLY SALES OF EACH SALESPERSON AND A GRAND TOTAL OF *
      *  ALL DAILY SALES. THE SECOND REPORT IS A SUMMARY REPORT OF THE *
      *  TOTAL SALES OF EACH FURNITURE TYPE WHICH USES ONLY A SINGLE   *
      *  ONE DIMENSIONAL TABLE.                                        *
      ******************************************************************
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

           SELECT SALES-MASTER
			   ASSIGN TO 
               'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC04\FURN.DAT'
			   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
		       ASSIGN TO 
			   'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC04\CBLSMRY.PRT '
		       ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.

       FD  SALES-MASTER
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS ST-REC
		   RECORD CONTAINS 42 CHARACTERS.
	   
		   01  ST-REC.
			   05  ST-SLM-NUM           PIC 99.
			   05  ST-DAY               PIC 9.
			   05  ST-AMOUNT            PIC S9(5)V99.
			   05  ST-FUR-CODE          PIC 9.
			   05  ST-SALES-DATE        PIC 9(6).
			   05  ST-SLM-NAME          PIC X(25).

       FD  PRTOUT
		   LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
		   DATA RECORD IS PRTLINE
		   LINAGE IS 60 WITH FOOTING AT 56.
		   
           01  PRTLINE                  PIC X(132).

	   WORKING-STORAGE SECTION.
       01  WORK-AREA.
		   05  MORE-RECS                PIC X      VALUE 'Y'.
		   05  C-PCTR                   PIC 99     VALUE 0.
      *  SWITCHES USED TO DETERMINE WHAT SHOULD BE PRINTED  *
		   05  PRINT-SW                 PIC X.
		   05  GT-SW                    PIC X      VALUE 'N'.
      *  SUBSCRIPTS USED TO MOVE AND PRINT DATA  *
		   05  FUR-SUB                  PIC 99     VALUE 0.
		   05  SALESPERSON-SUB          PIC 99     VALUE 0.
		   05  DAY-SUB                  PIC 9      VALUE 0.

	   01  CURRENT-DATE-AND-TIME.
		   05  I-DATE.
			   10  I-YYYY               PIC 9(4).
			   10  I-MM                 PIC 99.
       		   10  I-DD                 PIC 99.
		   05  I-TIME                   PIC X(11).

      *  TITLE LINE THAT IS USED IN BOTH REPORTS  *
	   01  COMPANY-TITLE-LINE.
	   	   05  FILLER                   PIC X(6)   VALUE 'DATE: '.
		   05  O-MM                     PIC XX.
		   05  FILLER                   PIC X      VALUE '/'.
		   05  O-DD                     PIC XX.
	   	   05  FILLER					PIC X	   VALUE '/'.
		   05  O-YYYY                   PIC X(4).
		   05  FILLER                   PIC X(36)  VALUE ' '.
		   05  FILLER                   PIC X(28)  VALUE 
                                        'JUST FITS FURNITURE EMPORIUM'.
           05  FILLER				    PIC X(44)  VALUE ' '.
		   05  FILLER                   PIC X(6)   VALUE 'PAGE: '.
		   05  O-PCTR                   PIC Z9.

      *  TITLE LINE FOR THE SALES REPORT  *
	   01  REPORT-TITLE-LINE.
		   05  FILLER                   PIC X(55)  VALUE ' '. 
 	       05  FILLER                   PIC X(22)  VALUE 
                                              'CHAPMAN''S SALES REPORT'.
		   05  FILLER                   PIC X(55)  VALUE ' '.

      *  COLUMN HEADINGS FOR THE SALES REPORT  *
       01  COL-HDGS-LINE.
		   05  FILLER                   PIC X(11)  VALUE 'SALESPERSON'.
		   05  FILLER                   PIC X(19)  VALUE ' '.
		   05  FILLER                   PIC X(6)   VALUE 'MONDAY'.
		   05  FILLER                   PIC X(5)   VALUE ' '.
		   05  FILLER                   PIC X(7)   VALUE 'TUESDAY'.
		   05  FILLER                   PIC X(5)   VALUE ' '.
		   05  FILLER                   PIC X(9)   VALUE'WEDNESDAY'.
		   05  FILLER                   PIC X(3)   VALUE ' '.
		   05  FILLER                   PIC X(8)   VALUE'THURSDAY'.
		   05  FILLER                   PIC X(4)   VALUE ' '.
		   05  FILLER                   PIC X(6)   VALUE 'FRIDAY'.
		   05  FILLER                   PIC X(6)   VALUE ' '.
		   05  FILLER                   PIC X(12)  VALUE 'SATURDAY    '.
		   05  FILLER                   PIC X(6)   VALUE 'SUNDAY'.
		   05  FILLER                   PIC X(9)   VALUE ' '.
		   05  FILLER                   PIC X(12)  VALUE 'WEEKLY TOTAL'.

	   01  BLANK-LINE.
		   05  FILLER                   PIC X(132) VALUE ' '.
		   
      *  CONTAINS ALL THE DATA FOR THE FURNITURE TABLE  *
	   01  T-GT-FURNITURE-INFO.
		   05  FILLER                   PIC X(33)  VALUE 
                                    'SOFAS/LOVESEATS       00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'CHAIRS                00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'COFFEE/END TABLES     00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'DINING ROOM TABLES    00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'DINING ROOM CHAIRS    00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'HUTCHES/CURIO CABINETS00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'LAMPS                 00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
                                    'MATRESS/BOX SPRING    00000000000'.
		   05  FILLER                   PIC X(33)  VALUE 
								    'BEDROOM FURNITURE     00000000000'.

      *  ONE DIMENSIIONAL TABLE FOR ALL FURNITURES AND THEIR TOTALS  *
	   01  T-GT-FURNITURE-DATA REDEFINES T-GT-FURNITURE-INFO.
		   05  T-GT-FURNITURE           OCCURS 9.
		       10  T-GT-FUR-NAME        PIC X(22).
			   10  T-GT-FUR-TOTAL       PIC S9(9)V99.

      *  TABLE THAT CONTAINS THE SALESPERSON AND THEIR DAILY SALES  *
	   01  T-SALES-DATA.
		   05  T-SALES-INFO             OCCURS 15.
		       10  T-SALESPERSON        PIC X(25).
			   10  T-DAILY-SALES-INFO   OCCURS 7.
				   15  T-DAILY-SALES    PIC S9(7)V99.

      *  TABLE THAT CONTAINS THE WEEKLY TOTALS OF ALL SALESPERSONS  *
	   01  T-ACC-WEEKLY-TOTALS-DATA.
		   05  T-ACC-WEEKLY-TOTALS-INFO OCCURS 15.
			   10  T-ACC-WEEKLY-TOTALS  PIC S9(9)V99.

      *  LINE THAT CONTAINS DETAILS USED IN THE SALES REPORT  * 
	   01  DETAIL-LINE                  OCCURS 15.
		   05  O-SALESPERSON-DATA.
		       10  O-SALESPERSON        PIC X(25).
		       10  FILLER               PIC X      VALUE ' '.
		   05  T-DAILY-SALES-DATA       OCCURS 7.
		       10  O-DAILY-SALES        PIC -ZZZZZZZ.99.
			   10  FILLER               PIC X      VALUE ' '.
		   05  FILLER                   PIC X      VALUE ' '.
		   05  O-ACC-WEEKLY-TOTALS      PIC -$$$$,$$$,$$$.99.

	  *  TABLE THAT CONTAINS THE DAILY TOTALS OF ALL SALES  *
       01  T-GT-DAILY-SALES-TOTALS-DATA.
		   05  T-GT-DAILY-TOTALS-INFO   OCCURS 7.
		       10  T-GT-DAILY-TOTALS    PIC S9(9)V99.

      *  DAILY TOTALS LINE FOR THE SALES REPORT  *
       01  GRAND-TOTAL-LINE.
		   05  FILLER                   PIC X(26)  VALUE 'DAILY TOTALS'.
		   05  T-DAILY-TOTALS-DATA      OCCURS 7.
		       10  O-GT-DAILY-TOTALS    PIC -ZZZZZZZ.99.
			   10  FILLER               PIC X      VALUE ' '.
		   05  FILLER                   PIC X(22)  VALUE ' '.

      *  TITLE LINE FOR THE SUMMARY REPORT  *
       01  GRAND-TOTALS-REPORT-TITLE.
		   05  FILLER                   PIC X(54)  VALUE ' '.
		   05  FILLER                   PIC X(24)  VALUE 
                                            'CHAPMAN''S SUMMARY REPORT'.
		   05  FILLER                   PIC X(54)  VALUE ' '.

      *  COLUMN HEADINGS FOR THE SUMMARY REPORT  *
	   01  GRAND-TOTALS-COL-HDGS-LINE.
		   05  FILLER                   PIC X(45)  VALUE ' '.
		   05  FILLER                   PIC X(14)  VALUE 
                                                   'FURNITURE NAME'.
	       05  FILLER                   PIC X(16)  VALUE ' '.
		   05  FILLER                   PIC X(12)  VALUE 'WEEKLY SALES'.
		   05  FILLER                   PIC X(45)  VALUE ' '.

      *  LINE THAT CONTAINS THE CONTENTS OF THE SUMMARY REPORT  *
	   01  GRAND-TOTALS-LINE.
		   05  FILLER                   PIC X(45)  VALUE ' '.
           05  O-GT-FUR-NAME            PIC X(25).
		   05  FILLER                   PIC X      VALUE ' '.
		   05  O-GT-FUR-TOTAL           PIC -$$$$,$$$,$$$.99.
		   05  FILLER                   PIC X(45)  VALUE ' '.

       PROCEDURE DIVISION.
       L1-MAIN.
		   PERFORM L2-INIT.
		   PERFORM L2-MAINLINE
			   UNTIL MORE-RECS = 'N'.
		   PERFORM L2-CLOSING.
	   STOP RUN.

       L2-INIT.
		   OPEN INPUT SALES-MASTER.
		   OPEN OUTPUT PRTOUT.

		   MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
		   MOVE I-YYYY TO O-YYYY.
		   MOVE I-MM TO O-MM.
		   MOVE I-DD TO O-DD.

		   PERFORM L9-READ.
		   PERFORM L9-HDGS.

      *  INITIALIZES ALL SOFT-CODED TABLES  *
		   INITIALIZE T-SALES-DATA.
		   INITIALIZE T-ACC-WEEKLY-TOTALS-DATA.
		   INITIALIZE T-GT-DAILY-SALES-TOTALS-DATA.

	   L2-MAINLINE.
		   PERFORM L3-CALCS.
		   PERFORM L9-READ.

	   L2-CLOSING.
		   PERFORM L4-SALESPERSONS
               VARYING SALESPERSON-SUB FROM 1 BY 1
				   UNTIL SALESPERSON-SUB > 15.

		   WRITE PRTLINE FROM GRAND-TOTAL-LINE
			   AFTER ADVANCING 2 LINES.

      *  PRINTS THE SUMMARY REPORT TITLES AND IT'S COLUMN HEADINGS  *
		   MOVE 'Y' TO GT-SW.
		   PERFORM L9-HDGS.

		   PERFORM L4-FURNITURE-TOTALS
			   VARYING FUR-SUB FROM 1 BY 1
				   UNTIL FUR-SUB > 9.

		   CLOSE SALES-MASTER.
		   CLOSE PRTOUT.

	   L3-CALCS.
      *  CALCULATES THE FURNITURE TOTALS  *
		   MOVE ST-SLM-NAME TO T-SALESPERSON(ST-SLM-NUM).
		   ADD ST-AMOUNT TO T-DAILY-SALES(ST-SLM-NUM, ST-DAY).
		   ADD ST-AMOUNT TO T-GT-FUR-TOTAL(ST-FUR-CODE).

      *  CALCULATES THE WEEKLY TOTALS  *
		   ADD ST-AMOUNT TO T-ACC-WEEKLY-TOTALS(ST-SLM-NUM).

      *  CALCULATES THE DAILY TOTALS  *
		   ADD ST-AMOUNT TO T-GT-DAILY-TOTALS(ST-DAY).

      *  MOVES AND PRINTS CONTENTS OF THE SALES REPORT  *
	   L4-SALESPERSONS.
      *  ALL PART OF ONE BIG LOOP  *
		   MOVE T-SALESPERSON(SALESPERSON-SUB) TO O-SALESPERSON(
			   SALESPERSON-SUB)
		   MOVE T-ACC-WEEKLY-TOTALS(SALESPERSON-SUB) TO 
               O-ACC-WEEKLY-TOTALS(SALESPERSON-SUB)
		   PERFORM L5-SALES 
	           VARYING DAY-SUB FROM 1 BY 1
			       UNTIL DAY-SUB > 7
		   PERFORM
			   VARYING DAY-SUB FROM 1 BY 1
			       UNTIL DAY-SUB > 7
			           MOVE T-GT-DAILY-TOTALS(DAY-SUB) TO 
                           O-GT-DAILY-TOTALS(DAY-SUB).

      *  ONLY PRINTS WHEN A SALESPERSON HAS SOLD SOMETHING  *
		   IF PRINT-SW = 'Y'
               WRITE PRTLINE FROM DETAIL-LINE(SALESPERSON-SUB)
                   AFTER ADVANCING 1 LINE.

      *  MOVES AND PRINTS CONTENTS OF THE SUMMARY REPORT  *
	   L4-FURNITURE-TOTALS.
		   MOVE T-GT-FUR-NAME(FUR-SUB) TO O-GT-FUR-NAME.
		   MOVE T-GT-FUR-TOTAL(FUR-SUB) TO O-GT-FUR-TOTAL.
		   WRITE PRTLINE FROM GRAND-TOTALS-LINE
			   AFTER ADVANCING 1 LINE.

      *  MOVES AND PRINTS DATA FOR THE SALES REPORTS  *
	   L5-SALES.
		   MOVE T-DAILY-SALES(SALESPERSON-SUB, DAY-SUB) TO 
			   O-DAILY-SALES(SALESPERSON-SUB, DAY-SUB).

      *  CHECKS IF THE SALESPERSON SOLD ANYTHING  *
		   IF T-ACC-WEEKLY-TOTALS(SALESPERSON-SUB) = 0
			   MOVE 'N' TO PRINT-SW
		   ELSE
			   MOVE 'Y' TO PRINT-SW
		   END-IF
		   MOVE T-ACC-WEEKLY-TOTALS(SALESPERSON-SUB) TO 
               O-ACC-WEEKLY-TOTALS(SALESPERSON-SUB).

	   L9-READ.
		   READ SALES-MASTER
			   AT END
				   MOVE 'N' TO MORE-RECS.

      *  PRINTS ALL TITLE LINES AND COLUMN HEADINGS FOR BOTH REPORTS  *
	   L9-HDGS.
		   ADD 1 TO C-PCTR.

		   IF GT-SW = 'N'
      *  PRINTS ALL TITLES AND HEADINGS OF THE SALES REPORT  *
			   MOVE C-PCTR TO O-PCTR
			   WRITE PRTLINE FROM COMPANY-TITLE-LINE
				   AFTER ADVANCING PAGE
			   WRITE PRTLINE FROM REPORT-TITLE-LINE
				   AFTER ADVANCING 1 LINE
			   WRITE PRTLINE FROM COL-HDGS-LINE
				   AFTER ADVANCING 2 LINES
			   WRITE PRTLINE FROM BLANK-LINE
				   AFTER ADVANCING 1 LINE
		   ELSE
      *  PRINTS ALL TITLES AND HEADINGS OF THE SUMMARY REPORT  *
			   MOVE C-PCTR TO O-PCTR
			   WRITE PRTLINE FROM COMPANY-TITLE-LINE
				   AFTER ADVANCING PAGE
			   WRITE PRTLINE FROM GRAND-TOTALS-REPORT-TITLE
				   AFTER ADVANCING 1 LINE
			   WRITE PRTLINE FROM GRAND-TOTALS-COL-HDGS-LINE
				   AFTER ADVANCING 2 LINES
               WRITE PRTLINE FROM BLANK-LINE
				   AFTER ADVANCING 1 LINE.