	   IDENTIFICATION DIVISION.
	   PROGRAM-ID.    COBLSC02.
	   AUTHOR.        LUCAS CHAPMAN.
	   DATE-WRITTEN.  3/22/2020.
	   DATE-COMPILED.
	  **************************************************************
	  *	 CREATES A DETAILED REPORT FOR WAKEBOARDS USING TABLES	   *
	  *	 WITH SEARCH STATEMENTS AND PERFORM VARYING BEING USED TO  *
	  *	 ACCESS TABLES.											   *
	  **************************************************************
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

		   SELECT BOARD-MASTER
			   ASSIGN TO
			   'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC02\BOARD.DAT'
			   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
			   ASSIGN TO
			   'C:\IHCC\SPRING TERM 2020\COBOL II\COBLSC02\BOARD.PRT'
			   ORGANIZATION IS RECORD SEQUENTIAL.

	   DATA DIVISION.
	   FILE SECTION.

	   FD  BOARD-MASTER
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS I-WAKEBOARD
		   RECORD CONTAINS 16 CHARACTERS.

       01  I-WAKEBOARD.
           05  I-UPC.
		       10  I-1ST-UPC        PIC XX.
			   10  I-2ND-UPC        PIC X(3).
			   10  I-3RD-UPC        PIC X(3).
			   10  I-4TH-UPC        PIC X(4).
		   05  I-BOARD              PIC 99.
		   05  I-LENGTH             PIC XX.

       FD  PRTOUT
		   LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  MORE-RECS            PIC X       VALUE 'Y'.
           05  C-PCTR               PIC 99      VALUE 0.
      *  TOTAL-SW USED TO PRINT GRAND TOTALS LINE ON A SEPERATE PAGE  *
		   05  TOTAL-SW             PIC X       VALUE 'N'.
      *  CALCUALTES THE TOTAL OF ALL SALES  *
		   05  C-GT-TOTAL           PIC 9(8)V99 VALUE 0.

	   01  CURRENT-DATE-AND-TIME.
		   05  I-DATE.
			   10  I-YYYY           PIC 9(4).
			   10  I-MM             PIC 99.
       		   10  I-DD             PIC 99.
		   05  I-TIME               PIC X(11).

       01  REPORT-TITLE.
		   05  FILLER               PIC X(6)    VALUE 'DATE: '.
		   05  O-MM                 PIC 99.
		   05  FILLER               PIC X       VALUE '/'.
		   05  O-DD                 PIC 99.
		   05  FILLER               PIC X       VALUE '/'.
		   05  O-YYYY               PIC 9(4).
           05  FILLER               PIC X(37)   VALUE ' '.
		   05  FILLER               PIC X(26)   VALUE 
                                          'CHAPMAN''S WAKEBOARD REPORT'.
           05  FILLER               PIC X(45)   VALUE ' '.
           05  FILLER               PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR               PIC Z9.

	   01  BLANK-LINE.
		   05  FILLER               PIC X(132)  VALUE ' '.

	   01  COLUMN-HEADINGS.
		   05  FILLER               PIC X(18)   VALUE ' '.
		   05  FILLER               PIC X(3)    VALUE 'UPC'.
		   05  FILLER               PIC X(22)   VALUE ' '.
		   05  FILLER               PIC X(10)   VALUE 'BOARD NAME'.
		   05  FILLER               PIC X(13)   VALUE ' '.
		   05  FILLER               PIC X(16)   VALUE 
                                                'BOARD SIZE RANGE'.
		   05  FILLER               PIC X(5)    VALUE ' '.
		   05  FILLER               PIC X(12)   VALUE 
                                                'BOARD NUMBER'.
		   05  FILLER               PIC X(5)    VALUE ' '.
		   05  FILLER               PIC X(11)   VALUE 'BOARD PRICE'.
		   05  FILLER               PIC X(17)   VALUE ' '.
		   
      *  SIZES TABLE  *
	   01  SIZES-INFO.
		   05  FILLER               PIC X(18)   VALUE 
                                                'XS111 - 117 CM0000'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'S 118 - 123 CM0000'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'M 124 - 133 CM0000'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'L 134 - 141 CM0000'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'XL142 - 146 CM0000'.

	   01  SIZES-TABLE REDEFINES SIZES-INFO.
		   05  T-BOARDSIZES         OCCURS 5    INDEXED BY SIZE-IDX.
			   10  T-SIZES          PIC XX.
			   10  T-RANGE          PIC X(12).
			   10  T-GT-SIZE-CTR    PIC 9(4).

      *  BOARD NAME AND PRICE TABLE  *
	   01  BOARD-INFO.
		   05  FILLER               PIC X(18)   VALUE 
                                                'SHANE HYBRID 52999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'WATSON HYBRID46999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'PEAK HYBRID  00899'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'FLX          64999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'SLAB         57999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'DELUXE HYBRID49900'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'B.O.B. GRIND 54999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'SUPER TRIP   49950'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'TEX          42999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'WITNESS      29999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'S4           34999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'NEMESIS      29999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'NEMESIS GRIND27999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'WING         54999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'ANGEL        33999'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'MELISSA GRIND49900'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'STAR         27899'.
		   05  FILLER               PIC X(18)   VALUE 
                                                'JETT GRIND   34999'.

	   01  BOARD-TABLE REDEFINES BOARD-INFO.
           05  T-BOARDS             OCCURS 18.
			   10  T-NAME           PIC X(13).
			   10  T-PRICE          PIC 9(3)V99.

	   01  DETAIL-LINE.
		   05  FILLER               PIC X(18)   VALUE ' '.
		   05  O-UPC                PIC X(15).
		   05  FILLER               PIC X(10)   VALUE ' '.
		   05  O-NAME               PIC X(13).
		   05  FILLER               PIC X(12)   VALUE ' '.
		   05  O-RANGE              PIC X(12).
		   05  FILLER               PIC X(12)   VALUE ' '.
		   05  O-BOARD              PIC Z9.
		   05  FILLER               PIC X(14)   VALUE ' '.
           05  O-PRICE              PIC $$$$.99.
		   05  FILLER               PIC X(17)   VALUE ' '.

      *  GRAND TOTALS SALES FOR ALL BOARDS  *
	   01  GRAND-TOTAL-LINE.
		   05  FILLER               PIC X(87)   VALUE ' '.   
		   05  FILLER               PIC X(14)   VALUE 'TOTAL SALES: '.
		   05  O-GT-TOTAL           PIC $$$,$$$,$$$.99.
		   05  FILLER               PIC X(17)   VALUE ' '. 

      *  GRAND TOTAL SALES FOR EACH BOARD THAT WAS SOLD  *
	   01  GRAND-TOTAL-SALES.
	       05  T-GT-TOTALS         PIC 9(7)V99	OCCURS 18.
      *  SUBSCRIPT FOR TOTAL SALES AND SIZE COUNTER  *
       01  SUB		                PIC 99.

       01  GRAND-TOTALS-TITLE.
		   05  FILLER               PIC X(6)    VALUE 'DATE: '.
		   05  O-GT-MM              PIC 99.
		   05  FILLER               PIC X       VALUE '/'.
		   05  O-GT-DD              PIC 99.
		   05  FILLER               PIC X       VALUE '/'.
		   05  O-GT-YYYY            PIC 9(4).
           05  FILLER               PIC X(38)   VALUE ' '.
		   05  FILLER               PIC X(24)   VALUE 
                                            'CHAPMAN''S SUMMARY REPORT'.
		   05  FILLER               PIC X(46)   VALUE ' '.
		   05  FILLER               PIC X(6)    VALUE 'PAGE: '.
		   05  O-GT-PCTR            PIC Z9.

	   01  GRAND-TOTALS-COLUMN-HEADINGS-1.
		   05  FILLER               PIC X(35)   VALUE ' '.
		   05  FILLER               PIC X(12)   VALUE 'BOARD NUMBER'.
		   05  FILLER               PIC X(13)   VALUE ' '.
		   05  FILLER               PIC X(10)   VALUE 'BOARD NAME'.
		   05  FILLER               PIC X(15)   VALUE ' '.
		   05  FILLER               PIC X(11)   VALUE 'TOTAL SALES'.
		   05  FILLER               PIC X(36)   VALUE ' '.

	   01  GRAND-TOTALS-LINE-1.
		   05  FILLER               PIC X(40)   VALUE ' '.
		   05  O-GT-BOARD           PIC Z9.
		   05  FILLER               PIC X(18)   VALUE ' '.
		   05  O-GT-NAME            PIC X(13).
		   05  FILLER               PIC X(10)   VALUE ' '.
		   05  O-GT-TOTALS          PIC $$,$$$,$$$.99.
		   05  FILLER               PIC X(38)   VALUE ' '.

	   01  GRAND-TOTALS-COLUMN-HEADINGS-2.
		   05  FILLER               PIC X(35)   VALUE ' '.
		   05  FILLER               PIC X(16)   VALUE 
                                                'BOARD SIZE RANGE'.
		   05  FILLER               PIC X(9)    VALUE ' '.
		   05  FILLER               PIC X(21)   VALUE 
                                                'NUMBER OF BOARDS SOLD'.
		   05  FILLER               PIC X(51)   VALUE ' '.

	   01  GRAND-TOTALS-LINE-2.
		   05  FILLER               PIC X(37)   VALUE ' '.
		   05  O-GT-RANGE           PIC X(12).
		   05  FILLER               PIC X(17)   VALUE ' '.
		   05  O-GT-SIZE-CTR        PIC Z,ZZ9.
		   05  FILLER               PIC X(61)   VALUE ' '.

	   PROCEDURE DIVISION.
	   L1-MAIN.
		   PERFORM L2-INIT.
		   PERFORM L2-MAINLINE
			   UNTIL MORE-RECS = 'N'.
		   PERFORM L2-CLOSING.
	   STOP RUN.

	   L2-INIT.
           OPEN INPUT BOARD-MASTER. 
	       OPEN OUTPUT PRTOUT. 

		   MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
		   MOVE I-YYYY TO O-YYYY.
		   MOVE I-YYYY TO O-GT-YYYY.
		   MOVE I-MM TO O-MM.
		   MOVE I-MM TO O-GT-MM.
		   MOVE I-DD TO O-DD.
		   MOVE I-DD TO O-GT-DD.

		   PERFORM L3-READ.
		   PERFORM L4-HEADINGS.

      *  INTIALIZES TOTAL SALES TABLE  *
		   PERFORM
			   VARYING SUB FROM 1 BY 1
				   UNTIL SUB > 18
					   MOVE 0 TO T-GT-TOTALS(SUB).

	   L2-MAINLINE.
		   PERFORM L3-CALCS.
		   PERFORM L3-OUTPUT.
		   PERFORM L3-READ.

	   L2-CLOSING.
      *  PRINTS THE GRAND TOTAL FOR ALL SALES  * 
		   MOVE C-GT-TOTAL TO O-GT-TOTAL.
		   WRITE PRTLINE FROM GRAND-TOTAL-LINE
       	  	   AFTER ADVANCING 3 LINES.

      *  PRINTS TITLE LINE FOR THE GRAND TOTALS PAGE  *
		   ADD 1 TO C-PCTR.
		   MOVE C-PCTR TO O-GT-PCTR.
		   WRITE PRTLINE FROM GRAND-TOTALS-TITLE
			   AFTER ADVANCING PAGE.

      *  PRINTS THE GRAND TOTAL SALES SECTION OF THE PAGE  *
		   WRITE PRTLINE FROM GRAND-TOTALS-COLUMN-HEADINGS-1
			   AFTER ADVANCING 3 LINES.
		   WRITE PRTLINE FROM BLANK-LINE
			   AFTER ADVANCING 1 LINE.
		   MOVE 'Y' TO TOTAL-SW.
		   PERFORM L3-GRAND-TOTALS
			   VARYING I-BOARD FROM 1 BY 1
				   UNTIL I-BOARD > 18.

      *  PRINTS THE GRAND TOTAL SIZE SECTION OF THE PAGE  *
		   WRITE PRTLINE FROM GRAND-TOTALS-COLUMN-HEADINGS-2
			   AFTER ADVANCING 3 LINES.
		   WRITE PRTLINE FROM BLANK-LINE
			   AFTER ADVANCING 1 LINE.
		   MOVE 'N' TO TOTAL-SW.
		   PERFORM L3-GRAND-TOTALS
		       VARYING SUB FROM 1 BY 1
			       UNTIL SUB > 5.

		   CLOSE BOARD-MASTER.
		   CLOSE PRTOUT.

	   L3-READ.
		   READ BOARD-MASTER
			   AT END
				   MOVE 'N' TO MORE-RECS.

       L3-CALCS.
           ADD T-PRICE(I-BOARD) TO T-GT-TOTALS(I-BOARD).
           ADD T-PRICE(I-BOARD) TO C-GT-TOTAL.

		   SET SIZE-IDX TO 1.
		   SEARCH T-BOARDSIZES
			   WHEN T-SIZES(SIZE-IDX) = I-LENGTH
				   MOVE T-RANGE(SIZE-IDX) TO O-RANGE
				   ADD 1 TO T-GT-SIZE-CTR(SIZE-IDX).

	   L3-OUTPUT.
      *  CLEARS VALUES IN O-UPC  *
		   MOVE ' ' TO O-UPC.
		   STRING I-1ST-UPC DELIMITED BY ' '
               '/' DELIMITED BY SIZE
			   I-2ND-UPC DELIMITED BY ' '
			   '-' DELIMITED BY SIZE
			   I-3RD-UPC DELIMITED BY ' '
			   '-' DELIMITED BY SIZE
			   I-4TH-UPC DELIMITED BY ' '
           INTO O-UPC.

		   MOVE I-BOARD TO O-BOARD.
		   MOVE T-PRICE(I-BOARD) TO O-PRICE.
		   MOVE T-NAME(I-BOARD) TO O-NAME.

		   WRITE PRTLINE FROM DETAIL-LINE
			   AFTER ADVANCING 2 LINES
				   AT EOP
					   PERFORM L4-HEADINGS.

      *  PRINTS THE TOTAL SALES AND SIZE COUNTER SECTIONS  *
	   L3-GRAND-TOTALS.
      *  PRINTS TOTAL SALES SECTION  *
           IF TOTAL-SW = 'Y'
			   MOVE I-BOARD TO O-GT-BOARD
			   MOVE T-NAME(I-BOARD) TO O-GT-NAME
			   MOVE T-GT-TOTALS(I-BOARD) TO O-GT-TOTALS
			   WRITE PRTLINE FROM GRAND-TOTALS-LINE-1
				   AFTER ADVANCING 1 LINE
           ELSE
      *  PRINTS SIZE COUNTER SECTION  *
				MOVE T-RANGE(SUB) TO O-GT-RANGE
				MOVE T-GT-SIZE-CTR(SUB) TO O-GT-SIZE-CTR
				WRITE PRTLINE FROM GRAND-TOTALS-LINE-2
				    AFTER ADVANCING 1 LINE.

	   L4-HEADINGS.
		   ADD 1 TO C-PCTR.
		   MOVE C-PCTR TO O-PCTR.

		   WRITE PRTLINE FROM REPORT-TITLE
			   AFTER ADVANCING PAGE.
		   WRITE PRTLINE FROM COLUMN-HEADINGS
			   AFTER ADVANCING 2 LINES.