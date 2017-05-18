!                    *****************
                     SUBROUTINE COUUTI
!                    *****************
!
     &(X,Y,NPOIN,NCOU,FMTCOU,NBOR,NPTFR,AT,DDC,TC1,TC2,UC1,VC1,UC2,VC2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    READS IN THE CURRENTS USING A USER-DEFINED FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TC1 AND TC2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELC,YRELC,UR,VR OR DIRECTLY FROM THE CURRENT FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE CURRENTS READ FROM THE FILE INTO THE
!+        ARRAYS UC1,VC1 UC2,VC2.
!+
!+    INTERPOLATION SUBROUTINE FASP :
!+
!+    CALL FASP(X,Y,UC1,NPOIN,XRELC,YRELC,UR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    CALL FASP(X,Y,VC1,NPOIN,XRELC,YRELC,VR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    THE CODE WILL INTERPOLATE THE CURRENT AUTOMATICALLY BETWEEN THESE
!+        2 TIME STEPS.
!+
!+    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TC2).
!+        IN THIS CASE TC2,UC2,VC2 ONLY ARE TO BE COMPUTED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE CURRENT FILE
!
!history  F.MARCOS (LNH)
!+        30/08/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        23/11/20012
!+        V6P3
!+   XRELC, YRELC, UR, VR removed, must be declared locally
!+   if necessary.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FMTCOU         |-->| CURRENTS FILE BINARY FORMAT
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOU           |-->| LOGICAL UNIT NUMBER OF THE CURRENTS FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TC1            |-->| TIME T1 IN THE CURRENT FILE
!| TC2            |-->| TIME T2 IN THE CURRENT FILE
!| UC1,VC1        |<->| CURRENTS VALUES AT TIME T1 IN THE MESH
!| UC2,VC2        |<->| CURRENTS VALUES AT TIME T2 IN THE MESH
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_COUUTI => COUUTI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NCOU,NPOIN,NPTFR
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: UC1(NPOIN),VC1(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: UC2(NPOIN),VC2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC,TC1,TC2
      CHARACTER(LEN=8), INTENT(IN)    :: FMTCOU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE COUUTI    '
        WRITE(LU,*) '    (FORMAT DU FICHIER DES COURANTS = 3)     '
        WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
        WRITE(LU,*) '*********************************************'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '       YOU CALL THE SUBROUTINE COUUTI        '
        WRITE(LU,*) '        (CURRENTS FILE FORMAT = 3)           '
        WRITE(LU,*) '       BUT YOU DID NOT MODIFIED IT           '
        WRITE(LU,*) '*********************************************'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
