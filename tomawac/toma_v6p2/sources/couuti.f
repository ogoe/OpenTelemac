!                    *****************
                     SUBROUTINE COUUTI
!                    *****************
!
     &(X,Y,NPOIN,NCOU, BINCOU,NBOR,NPTFR,AT,DDC,TC1,TC2,
     & NP,XRELC,YRELC,UR,VR,UC1,VC1,UC2,VC2,NPMAX)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINCOU         |-->| CURRENTS FILE BINARY
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOU           |-->| LOGICAL UNIT NUMBER OF THE CURRENTS FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TC1            |-->| TIME T1 IN THE CURRENT FILE
!| TC2            |-->| TIME T2 IN THE CURRENT FILE
!| UC1,VC1        |<->| CURRENTS VALUES AT TIME T1 IN THE CURRENTS FILE
!| UC2,VC2        |<->| CURRENTS VALUES AT TIME T2 IN THE CURRENTS FILE
!| UR,VR          |<->| TABLE OF THE VALUES READ IN THE CURRENT FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XRELC          |<->| TABLE OF THE ABSCISSES OF CURRENTS FILE POINTS
!| YRELC          |<->| TABLE OF THE ORDINATES OF CURRENTS FILE POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_COUUTI => COUUTI
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NCOU,NPOIN,NPMAX,NP,NPTFR,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELC(NPMAX),YRELC(NPMAX), UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION UC1(NPMAX),VC1(NPMAX),UC2(NPMAX),VC2(NPMAX)
      DOUBLE PRECISION AT,DDC,TC1,TC2
!
      CHARACTER*3 BINCOU
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '*********************************************'
         WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE COUUTI    '
         WRITE(LU,*) '    (FORMAT DU FICHIER DES COURANTS = 3)     '
         WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
         WRITE(LU,*) '*********************************************'
      ELSE
         WRITE(LU,*) '*********************************************'
         WRITE(LU,*) '       YOU CALL THE SUBROUTINE COUUTI        '
         WRITE(LU,*) '        (CURRENTS FILE FORMAT = 3)           '
         WRITE(LU,*) '       BUT YOU DID NOT MODIFIED IT           '
         WRITE(LU,*) '*********************************************'
      ENDIF
      CALL PLANTE(0)
      RETURN
      END
