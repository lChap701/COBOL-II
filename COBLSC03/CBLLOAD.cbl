       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLLOAD.
	   DATE-WRITTEN. 4/29/2020.
	   DATE-COMPILED.
	  ******************************************************************
	  *	 THE SUBROUTINE THAT IS CALLED BY ANOTHER PROGRAM THAT HOLDS A *
	  *	 TABLE TO BE USED TO INTIALIZE ANOTHER TABLE.                  *
	  ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *  CONTAINS ALL THE DATA FOR THE FURNITURE TABLE  *
	   01  T-GT-FURNITURE-INFO.
		   05  FILLER              PIC X(22)  VALUE 
                                              'SOFAS/LOVESEATS       '.
		   05  FILLER              PIC X(22)  VALUE 
								              'CHAIRS                '.
		   05  FILLER              PIC X(22)  VALUE 
								              'COFFEE/END TABLES     '.
		   05  FILLER              PIC X(22)  VALUE 
								              'DINING ROOM TABLES    '.
		   05  FILLER              PIC X(22)  VALUE 
								              'DINING ROOM CHAIRS    '.
		   05  FILLER              PIC X(22)  VALUE 
								              'HUTCHES/CURIO CABINETS'.
		   05  FILLER              PIC X(22)  VALUE 
								              'LAMPS                 '.
		   05  FILLER              PIC X(22)  VALUE 
                                              'MATRESS/BOX SPRING    '.
		   05  FILLER              PIC X(22)  VALUE 
								              'BEDROOM FURNITURE     '.

      *  ONE DIMENSIONAL TABLE FOR ALL FURNITURES  *
	   01  T-GT-FURNITURE-DATA REDEFINES T-GT-FURNITURE-INFO.
		   05  T-GT-FURNITURE      OCCURS 9.
		       10  T-GT-FUR-NAME   PIC X(22).

      *  SECTION USED TO BE ABLE TO USE THE "USING" WITH PASSED-DATA  *
	   LINKAGE SECTION.
      *  USED TO PASS DATA TO COBLSC03  *    
	   01  PASSED-DATA.
		   05  PD-FURN-CODE        PIC 9.
		   05  PD-FURN-DESC        PIC X(22).

      *  MOVES THE DESCRIPTION TO ANOTHER PROGRAM  *
       PROCEDURE DIVISION USING PASSED-DATA.
	      MOVE T-GT-FUR-NAME(PD-FURN-CODE) TO PD-FURN-DESC.
	  *	 GOBACK USED TO EXIT THE SUBROUTINE	 *
		  GOBACK.