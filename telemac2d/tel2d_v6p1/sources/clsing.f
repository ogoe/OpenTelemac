!                    *****************
                     SUBROUTINE CLSING
!                    *****************
!
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,X,Y,ZF,CHESTR,NKFROT,KARMAN,
     & ZDIG,PHIDIG,NBOR,H,T,NTRAC,IOPTAN,UNORM,
     & UBOR,VBOR,TBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,GRAV)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE COMPUTATION OF DISCHARGES AND
!+                DETERMINES BOUNDARY CONDITIONS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+        
!+   
!
!history  J.-M. HERVOUET (LNH)
!+        23/11/2005
!+        V5P6
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
!| CHESTR         |-->| COEFFICIENTS DE FROTTEMENT SUR LE FOND.
!| GRAV           |-->| GRAVITE.
!| H              |-->| HAUTEUR AU PAS DE TEMPS COURANT.
!| IOPTAN         |-->| OPTION DE CALCUL DES VITESSES TANGENTIELLES.
!| KARMAN         |-->| CONSTANTE DE KARMAN.
!| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD.
!| NKFROT         |---| 
!| NPSMAX         |-->| NOMBRE MAXIMUM DE POINTS POUR UN COTE D'UNE
!|                |   | SINGULARITE.
!| NTRAC          |---| 
!| NWEIRS         |-->| NOMBRE DE SINGULARITES LINEIQUES.
!| T              |-->| TRACEUR AU PAS DE TEMPS COURANT.
!| X,Y            |-->| COORDONNEES DES NOUEDS.
!| ZF             |-->| COTE DU FOND.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CLSING => CLSING
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NWEIRS,NPSMAX,IOPTAN
      INTEGER, INTENT(IN) :: NKFROT(*),NBOR(*)
      INTEGER, INTENT(IN) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(INOUT) :: LIUBOR(*),LIVBOR(*),LIHBOR(*)
      INTEGER, INTENT(IN) :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: PHIDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN) :: ZDIG(NWEIRS,NPSMAX),H(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),ZF(*),CHESTR(*)
      DOUBLE PRECISION, INTENT(IN) :: KARMAN,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UNORM(*)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,IA,IB,NA,NB
!
      DOUBLE PRECISION HMIN,PHI,QAB,YAA,YBB,YDEN,YS
!
!-----------------------------------------------------------------------
!
      HMIN=1.D-3
!
!     COMPUTES UNIT DISCHARGES
!
      DO 10 N=1,NWEIRS
      DO 20 I=1,NPSING(N)
        IA=NUMDIG(1,N,I)
        IB=NUMDIG(2,N,I)
        NA=NBOR(IA)
        NB=NBOR(IB)
        YAA=H(NA)+ZF(NA)
        YBB=H(NB)+ZF(NB)
        YS=ZDIG(N,I)
        PHI=PHIDIG(N,I)
!
        IF(YAA.GT.YBB) THEN
!         CASE WHERE A IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YAA/3.D0
          IF(YBB.LT.YDEN) THEN
            CALL LOIDEN(YAA,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YAA,YBB,YS,PHI,QAB,GRAV)
          ENDIF
        ELSE
!         CASE WHERE B IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YBB/3.D0
          IF(YAA.LT.YDEN) THEN
            CALL LOIDEN(YBB,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YBB,YAA,YS,PHI,QAB,GRAV)
          ENDIF
          QAB=-QAB
        ENDIF
!
! COMPUTES THE NORMAL DISCHARGE
!
        IF(H(NA).LE.HMIN) THEN
          UNORM(IA)=0.D0
        ELSE
          UNORM(IA)=-QAB/H(NA)
        ENDIF
!
        IF(H(NB).LE.HMIN) THEN
          UNORM(IB)=0.D0
        ELSE
          UNORM(IB)=-QAB/H(NB)
        ENDIF
!
20    CONTINUE
10    CONTINUE
!
!     DETERMINES THE NUMERICAL VALUE
!     OF THE BOUNDARY CONDITIONS:
!
      CALL CLHUVT(NWEIRS,NPSING,NPSMAX,NUMDIG,ZDIG,X,Y,ZF,
     &            IOPTAN,UNORM,CHESTR,NKFROT,KARMAN,T,NTRAC,H,
     &            UBOR,VBOR,TBOR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END