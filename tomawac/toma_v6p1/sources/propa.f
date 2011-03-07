!                    ****************
                     SUBROUTINE PROPA
!                    ****************
!
     &   (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,FRE,IKLE2,ETAP1,
     &    NPOIN3,NPOIN2,NELEM2,NPLAN,NF,COURAN,TRA01,TRA02)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ADVECTION STEP.
!+                INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        05/12/95
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
!| B              |-->| FACTEUR B
!| COURAN         |-->| LOGIQUE INDIQUANT SI IL Y A DU COURANT
!| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!|                |   | NOEUD.
!| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!| ETAP1          |<->| TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
!|                |   | L'ETAGE SUPERIEUR
!| F              |<->| FONCTION A CONVECTER
!| FRE            |<->| NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
!| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!|                |   | ET GLOBALE DU MAILLAGE 2D.
!| NELEM2         |-->| NOMBRE D ELEMENTS DU MAILLAGE 2D
!| NF             |-->| NOMBRE DE FREQUENCES
!| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
!| SHF            |<->| COORDONNEES BARYCENTRIQUES SUIVANT F DES
!|                |   | NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
!| SHP1           |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!|                |   | COURBES CARACTERISTIQUES.
!| SHP2           |---|
!| SHP3           |---|
!| SHZ            |---|
!| TRA01          |<->| TABLEAU DE TRAVAIL
!| TRA02          |<->| TABLEAU DE TRAVAIL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PROPA => PROPA
      USE TOMAWAC_MPI
!
      IMPLICIT NONE
!
      INTEGER NPOIN3,NPOIN2,NELEM2,NPLAN,NF
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)
      DOUBLE PRECISION B(NPOIN2,NF)
      DOUBLE PRECISION TRA01(NPOIN3,8),TRA02(NPOIN2,NPLAN,NF)
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF),FRE(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),ETAP1(NPLAN)
      LOGICAL COURAN
      REAL WW(1)
!
      DOUBLE PRECISION X(1)
      INTEGER IFF,I,ISTAT,LU, IB(1)
      CHARACTER*3 CAR
!
!----------------------------------------------------------------------
!
      LU=6
!
        IF (.NOT.COURAN) THEN
!
         DO 300 IFF=1,NF
!
            IFREQ = IFF
            CALL INTERP_TOMAWAC
     &        (F(1,1,IFF),B(1,IFF),SHP1(1,IFF),SHP2(1,IFF),
     &             SHP3(1,IFF),SHZ(1,IFF),ELT(1,IFF),ETA(1,IFF),IKLE2,
     &         ETAP1,NPOIN2,NELEM2,NPLAN,TRA01)
!
300      CONTINUE
!
        ELSE
!
            CALL INTER4D
     &       (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,
     &        FRE,IKLE2,ETAP1,NPOIN2,NELEM2,NPLAN,NF,TRA02)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END