//KC03A09C JOB  1,LUCAS,NOTIFY=&SYSUID,MSGCLASS=H                 
//COB1     EXEC IGYWCLG,                                          
//            PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM'         
//COBOL.SYSUT3 DD UNIT=SYSALLDA,SPACE=(CYL,(1,2))                 
//COBOL.SYSIN DD DSN=KC03A09.SOURCE.COBOL(COBLSC01),DISP=SHR      
//GO.INFILE   DD DSN=KC03A09.TEST.PAINTEST.DAT,DISP=SHR           
//GO.OUTFILE  DD DSN=KC03A09.COBOL.OUTPUT,DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(2,2)),                      
//             DCB=(DSORG=PS,LRECL=133,RECFM=FBA,BLKSIZE=1330)    
//GO.SYSOUT DD SYSOUT=*                                           
//                                                                