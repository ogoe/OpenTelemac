!                    *****************
                     SUBROUTINE VENUTI
!                    *****************
!
     &(X,Y,NPOIN,NVEN, BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     & NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    READS THE WINDS FROM A USED-DEFINED FILE FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE WIND FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE TIDES READ FROM THE FILE INTO THE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS
!| DDC            |-->| DATE DE DEBUT DU CALCUL
!| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
!| NP             |-->| NOMBRE DE POINTS RELEVES
!| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD
!| NVEN           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES VENTS
!| TV1            |-->| DATE CORRESPONDANT AU CHAMP DE VENT U1,V1
!| TV2            |-->| DATE CORRESPONDANT AU CHAMP DE VENT U2,V2
!| U1,V1          |<->| TABLEAU DES VENTS RELEVES AU TEMPS 1
!| U2,V2          |<->| TABLEAU DES VENTS RELEVES AU TEMPS 2
!| UR,VR          |<->| TABLEAU DES VENTS RELEVES
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NVEN,NPOIN,NPMAX,NP,NPTFR,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX), UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION U1(NPMAX),V1(NPMAX),U2(NPMAX),V2(NPMAX)
      DOUBLE PRECISION AT,DDC,TV1,TV2
!
      CHARACTER*3 BINVEN
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE VENUTI    '
      WRITE(LU,*) '    (FORMAT DU FICHIER DES VENTS = 4)        '
      WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(0)
      RETURN
      END