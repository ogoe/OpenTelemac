!                    *****************
                     SUBROUTINE KEPCL3
!                    *****************
!
     &(KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     & KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,RUGOL,
     & KBORS,EBORS,LIKBOS,LIEBOS,LIUBOS,
     & DISBOR,AK,U,V,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     & DNUVIH,DNUVIV,KARMAN,CMU,LISRUF,LISRUL,
     & VIRT,KMIN,KMAX,EMIN,EMAX,KENT,KENTU,KSORT,KADH,KLOG,
     & UETCAR,UETCAL)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS K-EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)     ; V. BOYER UMIST
!+        04/01/2010
!+        V6P0
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
!| AK             |-->| ENERGIE TURBULENTE
!| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
!| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
!| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
!| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
!| EBORF          |---|
!| EBORL          |---|
!| EBORS          |---|
!| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!| H              |-->| HAUTEUR D'EAU AU TEMPS N
!| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
!| KARMAN         |-->| CONSTANTE DE KARMAN
!| KBORF          |---|
!| KBORL          |---|
!| KBORS          |---|
!| KENT           |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
!| KENTU          |---|
!| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
!| KMIN,KMAX      |-->| K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!| KSORT          |-->| CONVENTION POUR UN POINT A VALEUR LIBRE
!| LIEBOF         |---|
!| LIEBOL         |---|
!| LIEBOS         |---|
!| LIKBOF         |---|
!| LIKBOL         |---|
!| LIKBOS         |---|
!| LISRUF         |-->| REGIME DE TURBULENCE 1: LISSE
!|                |   | POUR LE FOND         2: RUGUEUX
!|                |   | 3: RUGUEUX (CHEZY)
!| LISRUL         |-->| REGIME DE TURBULENCE 1: LISSE
!|                |   | DES BORDS            2: RUGUEUX
!|                |   | 3: RUGUEUX (CHEZY)
!| LIUBOF         |---|
!| LIUBOL         |---|
!| LIUBOS         |---|
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!| RUGOL          |---|
!| U              |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
!| UETCAL         |---|
!| UETCAR         |---|
!| V              |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
!| VIRT           |-->| ORIGIN VIRTUEL POUR EPSILON (TELEMAC 3D)
!| Z              |-->| COTES DES POINTS DU MAILLAGE 3D REEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT,AEBORF,BEBORF,SIGMAE
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2,KENTU
      INTEGER, INTENT(IN) :: LISRUF,LISRUL,KENT,KSORT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN), AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2)      , UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: KBORF(NPOIN2), KBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: KBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORF(NPOIN2), EBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN)    :: DISBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: RUGOL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: VIRT, DNUVIH, DNUVIV, KARMAN
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: KMIN, KMAX, EMIN, EMAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,IPOIN3,ITER,IP,IBOT
!
      DOUBLE PRECISION EFOND, KFOND, ESURF, HAUT, DENOM
      DOUBLE PRECISION SSQCMU, UTANG, UETUTA, DIST, PROPNU
      DOUBLE PRECISION YPLUS, DISTFOND
!                                    VINCENT BOYER'S CHOICE
      DOUBLE PRECISION, PARAMETER :: FICTIFUET = 2.D0
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
      DOUBLE PRECISION, PARAMETER :: NIVTURB = 0.005D0
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      SSQCMU = 1.D0 /SQRT(CMU)
      PROPNU = (2*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!     BOTTOM
!=======================================================================
!
!     THIS IS DONE IN LIMI3D
!     AEBORF%TYPR='0'
!     BEBORF%TYPR='0'
!     AKBORF%TYPR='0'
!     BKBORF%TYPR='0'
!
      DO IPOIN2=1,NPOIN2
!
        IF(IPBOT%I(IPOIN2).EQ.0) THEN
!         NORMAL CASE
          DIST =(Z(IPOIN2,2)-Z(IPOIN2,1))/FICTIFUET
          IF(LIEBOF(IPOIN2).EQ.KENT) THEN
            EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*SQRT(UETCAR(IPOIN2))
     &                                             /(KARMAN*DIST),EMIN)
          ENDIF
          IF(LIKBOF(IPOIN2).EQ.KENT) THEN
            KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
          ENDIF
        ELSE
!         RISK OF SMASHED PLANES OR TIDAL FLATS
          IPLAN=IPBOT%I(IPOIN2)+1
          IF(IPLAN.EQ.NPLAN) THEN
!           CASE OF TIDAL FLATS
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=EMIN
            ENDIF
!           IN THIS CASE KBORF COMPUTED ABOVE MAY YIELD
!           ABNORMAL VALUES OF VISCOSITY
            IF(LIKBOF(IPOIN2).EQ.KENT) KBORF(IPOIN2)=KMIN
          ELSE
!           CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
            DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN))/FICTIFUET
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*
     &                         SQRT(UETCAR(IPOIN2))/(KARMAN*DIST),EMIN)
            ENDIF
            IF(LIKBOF(IPOIN2).EQ.KENT) THEN
              KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!=======================================================================
!     FREE SURFACE
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
!       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!
!         NEZU & NAKAGAWA: TURBULENCE IN OPEN CHANNEL FLOWS
!
!         SEE KEPICL: THIS VALUE IS NOT USED IF CONDITION
!                     IS NOT SET TO KENT ON EPSILON
          HAUT = MAX(H(IPOIN2),1.D-7)
          ESURF = SQRT(AK(IPOIN2,NPLAN)**3) / (0.18D0*HAUT)
!
!         5.9
!         ESURF = CMU**0.75D0 / KARMAN
!    &          * AK(IPOIN2,NPLAN)**1.5D0 / (VIRT*HAUT)
          EBORS(IPOIN2) = MAX(ESURF,EMIN)
!
        ENDIF
!
      ENDDO
!
!=======================================================================
!     LATERAL BOUNDARIES
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
         IPOIN2 = NBOR(IPTFR)
         DIST   = DISBOR(IPTFR) / FICTIFUET
         HAUT   = MAX(H(IPOIN2),1.D-7)
!
         DO IPLAN=1,NPLAN
!
            IP=MAX(IPLAN,2)
            IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
!           DISTANCE TO BOTTOM (WILL BE 0 WITH TIDAL FLATS)
            DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
!
!           DIRICHLET ON K
!           ---------------
!
            IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!              ************************************
!
!            THEORY BY VINCENT BOYER (SEE ALSO KEPINI)
!
!            KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN)**2,KMIN)
!
!            NO TURBULENCE
!
             KBORL(IPTFR,IPLAN) = KMIN
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &                LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
!                WALL
!
!                KBORL(IPTFR,IPLAN)=MAX(SSQCMU*UETCAL(IPTFR,IPLAN),KMIN)
                 KBORL(IPTFR,IPLAN)=KMIN
!
!              ****
               ELSE
!              ****
!
                 IF (LNG.EQ.1) WRITE(LU,111) IPTFR,LIUBOL(IPTFR,IPLAN)
                 IF (LNG.EQ.2) WRITE(LU,112) IPTFR,LIUBOL(IPTFR,IPLAN)
                 CALL PLANTE(1)
                 STOP
!
!              *****
               ENDIF
!              *****
!
            ENDIF
!           -----
!
!           DIRICHLET ON EPSILON
!           ---------------------
!
            IF(LIEBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!              ************************************
!
!                 COMING IN THE DOMAIN: TURBULENCE DUE TO THE
!                 BOTTOM AS IN KEPINI; COMPUTES EBORL ACCORDING
!                 TO KBORL AT THE BOTTOM
!
!                 EBORL(IPTFR,IPLAN)=CMU**0.75*SQRT(KBORL(IPTFR,1)**3)
!    &                              /KARMAN/MAX(DISTFOND,1.D-6)
!                 EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
                  EBORL(IPTFR,IPLAN)=EMIN
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &                LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
!                 WALL
!
!                 EBORL(IPTFR,IPLAN) =
!    &            MAX(UETCAL(IPTFR,IPLAN)*SQRT(UETCAL(IPTFR,IPLAN))/
!    &            (KARMAN*DIST*FICTIFUET/FICTIFEPS),EMIN)
                  EBORL(IPTFR,IPLAN)=EMIN
!
!              ****
               ELSE
!              ****
!
!                 OTHER
!
                  IF (LNG.EQ.1) WRITE(LU,121) IPTFR,LIUBOL(IPTFR,IPLAN)
                  IF (LNG.EQ.2) WRITE(LU,122) IPTFR,LIUBOL(IPTFR,IPLAN)
                  CALL PLANTE(1)
                  STOP
!
!              *****
               ENDIF
!              *****
!
            ENDIF
!           -----
!
         ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
101   FORMAT(' KEPCL3 : REGIME DE TURBULENCE INCONNU : ',I6)
102   FORMAT(' KEPCL3 : UNKNOWN TURBULENCE MODEL : ',I6)
111   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR KBOR : LIUBOR =',I6)
112   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
121   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR EBOR : LIUBOR =',I6)
122   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END