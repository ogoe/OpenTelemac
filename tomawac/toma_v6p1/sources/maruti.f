!                    *****************
                     SUBROUTINE MARUTI
!                    *****************
!
     &(X,Y,NPOIN,NMAR, BINMAR,NBOR,NPTFR,AT,DDC,TV1,TV2,
     & NP,XRELV,YRELV,ZR,Z1,Z2,NPMAX)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| BINMAR         |-->| BINAIRE DU FICHIER DES MAREES
!| DDC            |-->| DATE DE DEBUT DU CALCUL
!| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
!| NMAR           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES MAREES
!| NP             |-->| NOMBRE DE POINTS RELEVES
!| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD
!| TV1            |-->| DATE CORRESPONDANT A LA MAREE Z1
!| TV2            |-->| DATE CORRESPONDANT A LA MAREE Z2
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
!| Z1             |<->| TABLEAU DES MAREES RELEVEES AU TEMPS 1
!| Z2             |<->| TABLEAU DES MAREES RELEVEES AU TEMPS 2
!| ZR             |<->| TABLEAU DES MAREES RELEVEES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NMAR,NPOIN,NPMAX,NP,NPTFR,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX), ZR(NPMAX)
      DOUBLE PRECISION Z1(NPMAX),Z2(NPMAX)
      DOUBLE PRECISION AT,DDC,TV1,TV2
!
      CHARACTER*3 BINMAR
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE MARUTI    '
      WRITE(LU,*) '    (FORMAT DU FICHIER DES MAREES = 3)     '
      WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(0)
      RETURN
      END