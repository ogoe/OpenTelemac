!                    *****************
                     SUBROUTINE VENUTI
!                    *****************
!
     &(X,Y,NPOIN,NVEN,FMTVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,U1,V1,U2,V2)
!
!***********************************************************************
! TOMAWAC   V6P3                                  29/06/2011
!***********************************************************************
!
!brief    READS THE WINDS FROM A USED-DEFINED FILE FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE WIND FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE WINDS READ FROM THE FILE INTO THE
!+        ARRAYS U1,V1 U2,V2.
!+
!+    INTERPOLATION SUBROUTINE FASP :
!+
!+    CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    THE CODE WILL INTERPOLATE THE WIND AUTOMATICALLY BETWEEN THESE
!+        2 TIME STEPS.
!+
!+    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TV2).
!+        IN THIS CASE TV2,U2,V2 ONLY ARE TO BE COMPUTED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE WIND FILE
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
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        23/11/20012
!+        V6P3
!+   XRELV, YRELV, UR, VR, NPMAX, NP removed, must be declared locally
!+   if necessary.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FMTVEN         |-->| WIND FILE FORMAT
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVEN           |-->| LOGICAL UNIT NUMBER OF THE WIND DATA FILE
!| TV1            |-->| TIME T1 IN THE WIND FILE
!| TV2            |-->| TIME T2 IN THE WIND FILE
!| U1,V1          |<->| WIND VALUES AT TIME T1 IN THE CURRENTS FILE
!| U2,V2          |<->| WIND VALUES AT TIME T2 IN THE CURRENTS FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_VENUTI => VENUTI
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NVEN,NPOIN,NPTFR
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U1(NPOIN),V1(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U2(NPOIN),V2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FMTVEN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE VENUTI    '
        WRITE(LU,*) '    (FORMAT DU FICHIER DES VENTS = 4)        '
        WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
        WRITE(LU,*) '*********************************************'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '*********************************************'
        WRITE(LU,*) '      YOU ARE CALLING SUBROUTINE VENUTI      '
        WRITE(LU,*) '            (WINDS FILE FORMAT = 4)          '
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
