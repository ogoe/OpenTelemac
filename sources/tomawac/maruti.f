!                    *****************
                     SUBROUTINE MARUTI
!                    *****************
!
     &(X,Y,NPOIN,NMAR,FMTMAR,NBOR,NPTFR,AT,DDC,TV1,TV2,Z1,Z2)
!
!***********************************************************************
! TOMAWAC   V6P3                                  21/06/2011
!***********************************************************************
!
!brief    READS THE TIDES IN A USER-DEFINED FILE FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE TIDE FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE TIDES READ FROM THE FILE INTO THE
!+        ARRAYS U1,V1 U2,V2.
!+
!+    INTERPOLATION SUBROUTINE FASP :
!+
!+    CALL FASP(X,Y,Z1,NPOIN,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    THE CODE WILL INTERPOLATE THE TIDE AUTOMATICALLY BETWEEN THESE
!+        2 TIME STEPS.
!+
!+    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TV2).
!+        IN THIS CASE TV2,Z2 ONLY ARE TO BE COMPUTED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE TIDE FILE
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        23/11/20012
!+        V6P3
!+   XRELV, YRELV, ZR removed, must be declared locally
!+   if necessary.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FMTMAR         |-->| TIDAL WATER LEVEL FILE FORMAT
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NMAR           |-->| LOGICAL UNIT NUMBER OF TIDAL WATER LEVEL FILE
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TV1            |-->| TIME T1 IN THE TIDAL WATER LEVEL FILE
!| TV2            |-->| TIME T2 IN THE TIDAL WATER LEVEL FILE
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| Z1             |<->| TIDAL LEVEL AT TV1 IN TIDAL WATER LEVEL FILE
!| Z2             |<->| TIDAL LEVEL AT TV2 IN TIDAL WATER LEVEL FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_MARUTI => MARUTI
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NMAR,NPOIN,NPTFR
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: Z1(NPOIN),Z2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC,TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FMTMAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE MARUTI    '
        WRITE(LU,*) '    (FORMAT DU FICHIER DES MAREES = 4)       '
        WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
        WRITE(LU,*) '*********************************************'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '      YOU ARE CALLING SUBROUTINE MARUTI      '
        WRITE(LU,*) '            (TIDE FILE FORMAT = 4)           '
        WRITE(LU,*) '           BUT YOU DID NOT MODIFY IT         '
        WRITE(LU,*) '*********************************************'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
