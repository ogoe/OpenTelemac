!                    *****************
                     SUBROUTINE COUUTI
!                    *****************
!
     &(X,Y,NPOIN,NCOU, BINCOU,NBOR,NPTFR,AT,DDC,TC1,TC2,
     & NP,XRELC,YRELC,UR,VR,UC1,VC1,UC2,VC2,NPMAX)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| BINCOU         |-->| BINAIRE DU FICHIER DES COURANTS
!| DDC            |-->| DATE DE DEBUT DU CALCUL
!| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
!| NCOU           |-->| NO D'UNITE LOGIQUE DU FICHIER DES COURANTS
!| NP             |-->| NOMBRE DE POINTS RELEVES
!| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD
!| TC1            |-->| DATE CORRESPONDANT AU COURANT (UC1,VC1)
!| TC2            |-->| DATE CORRESPONDANT AU COURANT (UC2,VC2)
!| UC1,VC1        |<->| TABLEAU DES COURANTS RELEVES AU TEMPS 1
!| UC2,VC2        |<->| TABLEAU DES COURANTS RELEVES AU TEMPS 2
!| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELC          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELC          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
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