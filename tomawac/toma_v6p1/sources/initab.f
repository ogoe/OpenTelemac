!                    *****************
                     SUBROUTINE INITAB
!                    *****************
!
     & (IBOR1,IFABOR1,NELEM2_DIM,PART)
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    INITIALISES USEFUL ARRAYS.
!
!history  F.MARCOS (LNH)
!+        23/05/96
!+        V1P2
!+
!
!history  DC
!+
!+
!+   ADDED ARG NPOIN2 TO DIMENSION THE ARRAYS
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Modification for direct coupling with TELEMAC
!+   Initialisation of the variabel BETA
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IBOR1          |<--| WORK TABLE
!| IFABOR1        |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| NELEM2_DIM     |---| NUMBER OF ELEMENTS IN 2D
!| PART           |-->| FLAG FOR DIRECT COUPLING WITH TELEMAC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
!
      DOUBLE PRECISION DEGRAD,Z,C
!
!$DC$ : NPOIN2 -> NPOIN2_DIM TO DIMENSION ARRAYS
!
!     INTEGER IBOR1(NELEM2,7),IFABOR1(NELEM2,3)
      INTEGER NELEM2_DIM
      INTEGER IBOR1(NELEM2_DIM,7),IFABOR1(NELEM2_DIM,3)
      INTEGER          IPLAN, IPOIN, IELEM2, IFREQ
      DOUBLE PRECISION AUXI
!
!GM V6P1 - COUPLING WITH TELEMAC
      INTEGER PART
!GM Fin
!-----------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
         COSTET(IPLAN) = COS(TETA(IPLAN))
         SINTET(IPLAN) = SIN(TETA(IPLAN))
         IF (ABS(COSTET(IPLAN)).LT.1.D-10) COSTET(IPLAN)=0.D0
         IF (ABS(SINTET(IPLAN)).LT.1.D-10) SINTET(IPLAN)=0.D0
         IF (IPLAN.LT.NPLAN) THEN
            ETAP1(IPLAN)=IPLAN+1
         ELSE
            ETAP1(IPLAN)=1
         ENDIF
      ENDDO
!
       AUXI=(RAISF-1.D0)/2.D0
       DFREQ(1)=AUXI*FREQ(1)
       DFREQ(NF)=AUXI*FREQ(NF-1)
       DO IFREQ = 2,NF-1
         DFREQ(IFREQ) = AUXI*(FREQ(IFREQ)+FREQ(IFREQ-1))
         DO IPOIN=1,NPOIN2
           B(IPOIN+(IFREQ-1)*NPOIN2)=0.D0
         ENDDO
       ENDDO
!
      IF (SPHE) THEN
         DEGRAD=1.745329252D-2
         DO 30 IPOIN=1,NPOIN2
           COSF(IPOIN)=COS(Y(IPOIN)*DEGRAD)
           TGF(IPOIN)=TAN(Y(IPOIN)*DEGRAD)
30       CONTINUE
      ENDIF
!
      DO 40 IELEM2=1,NELEM2
         IBOR1(IELEM2,1)=IFABOR1(IELEM2,1)
         IBOR1(IELEM2,2)=IFABOR1(IELEM2,2)
         IBOR1(IELEM2,3)=IFABOR1(IELEM2,3)
         IBOR1(IELEM2,4)=1
         IBOR1(IELEM2,5)=1
         IBOR1(IELEM2,6)=1
         IBOR1(IELEM2,7)=1
40    CONTINUE
!
! INITIALISES THE VARIABLE BETA
      DO IPOIN=1,NPOIN2
        BETA(IPOIN)=0.
      ENDDO
!
! INITIALISES THE GRADIENTS OF DEPTH, U AND V
!
! W1 ( EX MASKEL) IS SET TO 1 FOR GRADF
!
      CALL OV ( 'X=C     ' , SW1%R, ST1%R, ST2%R,
     &                       1.D0 , NELEM2 )
!
      IF (.NOT.PROINF)
     &CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
!GM V6P1 - COUPLING WITH TELEMAC
      IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
      CALL VECTOR(ST2,'=','GRADF          X',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST3,'=','GRADF          X',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
      ENDIF
!
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          IF (.NOT.PROINF) CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST4,2,MESH)
!GM V6P1 - COUPLING WITH TELEMAC
          IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
            CALL PARCOM(ST2,2,MESH)
            CALL PARCOM(ST3,2,MESH)
          ENDIF
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IF (.NOT.PROINF)
     & CALL OV('X=Y/Z   ',SDZX%R,ST1%R,ST4%R,C,NPOIN2)
!GM V6P1 - COUPLING WITH TELEMAC
      IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
       CALL OV('X=Y/Z   ',SDUX%R,ST2%R,ST4%R,C,NPOIN2)
       CALL OV('X=Y/Z   ',SDVX%R,ST3%R,ST4%R,C,NPOIN2)
      ENDIF
!
      IF (.NOT.PROINF)
     & CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     &  ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
!GM V6P1 - COUPLING WITH TELEMAC
      IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
       CALL VECTOR(ST2,'=','GRADF          Y',IELM2,1.D0,SUC,
     &  ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST3,'=','GRADF          Y',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
      ENDIF
!
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          IF (.NOT.PROINF) CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST4,2,MESH)
!GM V6P1 - COUPLING WITH TELEMAC
          IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
            CALL PARCOM(ST2,2,MESH)
            CALL PARCOM(ST3,2,MESH)
          ENDIF
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IF (.NOT.PROINF)
     & CALL OV('X=Y/Z   ',SDZY%R,ST1%R,ST4%R,C,NPOIN2)
!GM V6P1 - COUPLING WITH TELEMAC
      IF (COURAN.OR.PART.EQ.0) THEN
!GM Fin
       CALL OV('X=Y/Z   ',SDUY%R,ST2%R,ST4%R,C,NPOIN2)
       CALL OV('X=Y/Z   ',SDVY%R,ST3%R,ST4%R,C,NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
      RETURN
      END
