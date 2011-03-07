!                    *****************
                     SUBROUTINE KOMCL3
!                    *****************
!
     & (KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     &  KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,RUGOL,
     &  KBORS,EBORS,LIKBOS,LIEBOS,LIUBOS,
     &  DISBOR,AK,EP,U,V,W,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     &  DNUVIH,DNUVIV,KARMAN, ALPHA,BETA,BETAS,OMSTAR,SCHMIT,LISRUF,
     &  LISRUL,VIRT,GRAV,KMIN,KMAX,EMIN,EMAX,KENTU,KENT,KSORT,KADH,KLOG,
     &  UETCAR,UETCAL)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS K-OMEGA.
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        **/02/2001
!+        
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
!| ALPHA          |---| 
!| BETA           |---| 
!| BETAS          |---| 
!| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
!| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
!| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
!| EBORF          |---| 
!| EBORL          |---| 
!| EBORS          |---| 
!| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!| EP             |---| 
!| GRAV           |-->| ACCELERATION DE LA PESANTEUR
!| H              |-->| HAUTEUR D'EAU AU TEMPS N
!| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
!| KARMAN         |-->| CONSTANTE DE KARMAN
!| KBORF          |---| 
!| KBORL          |---| 
!| KBORS          |---| 
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
!| OMSTAR         |---| 
!| RUGOL          |---| 
!| SCHMIT         |-->| CONSTANTE DU MODELE K-EPSILON
!| U              |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
!| UETCAL         |---| 
!| UETCAR         |---| 
!| V              |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
!| VIRT           |-->| ORIGIN VIRTUEL POUR EPSILON (TELEMAC 3D)
!| W              |---| 
!| Z              |-->| COTES DES POINTS DU MAILLAGE 3D REEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN) :: LISRUF, LISRUL,KENTU,KENT,KSORT,KADH,KLOG
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
      DOUBLE PRECISION, INTENT(IN) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: AK(NPOIN2,NPLAN),EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2),UETCAR(NPOIN2)
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
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA,BETAS,OMSTAR
      DOUBLE PRECISION, INTENT(IN) :: SCHMIT,GRAV
      DOUBLE PRECISION, INTENT(IN) :: KMIN, KMAX, EMIN, EMAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION KRPLUS, SR
      DOUBLE PRECISION YPLUS, DISTFOND
      DOUBLE  PRECISION, PARAMETER :: FICTIFUET = 3.7D0
      DOUBLE  PRECISION, PARAMETER :: FICTIFOM  = 2.D0
      DOUBLE  PRECISION, PARAMETER :: NIVTURB = 0.02D0
      DOUBLE  PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE  PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR, IPLAN, IPOIN2, ITER
!
      DOUBLE PRECISION EFOND, KFOND, ESURF, HAUT, DENOM, CEPS
      DOUBLE PRECISION SBETAS,UTANG, UETUTA, DIST, PROPNU
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      SBETAS = 1.D0/SQRT(BETAS)
      PROPNU = (2.D0*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!
!                        /* LOOP ON THE BOTTOM */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
!        DIRICHLET ON K
!        ---------------
!
         IF(LIKBOF(IPOIN2).EQ.KENT) THEN
!        -----------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
            KBORF(IPOIN2) = MAX(SBETAS*UETCAR(IPOIN2),KMIN)
          ELSE
            KBORF(IPOIN2) = KMIN
          ENDIF
!
         ENDIF
!
!
!        DIRICHLET ON EPSILON
!        ---------------------
!
         IF(LIEBOF(IPOIN2).EQ.KENT) THEN
!        -------------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
!
!           VINCENT BOYER CHOSE 3.7D0
            DIST  = (Z(IPOIN2,2)-Z(IPOIN2,1)) / 3.7D0
            EBORF(IPOIN2) = MAX(SBETAS*SQRT(UETCAR(IPOIN2))
     &                                 /(KARMAN*DIST),EMIN)
!
          ENDIF
!
         ENDIF
!
      ENDDO
!
!=======================================================================
!
!                        /* LOOP ON THE SURFACE */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
         HAUT = MAX(H(IPOIN2),1.D-7)
!
!        DIRICHLET ON EPSILON
!        ---------------------
!
         IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!        -------------------------------
!
           EBORS(IPOIN2) = OMSTAR*AK(IPOIN2,NPLAN)**0.5D0/HAUT
!
         ENDIF
!
      ENDDO
!
!=======================================================================
!
!                        /* LOOP ON THE LATERAL BOUNDARIES */
!
!
!  COMPUTES KBOR,EBOR, AND AUBOR
!
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
         IPOIN2 = NBOR(IPTFR)
         DIST   = DISBOR(IPTFR) / FICTIFUET
         HAUT   = MAX( H(IPOIN2), 1.D-7 )
!
         DO IPLAN=1,NPLAN
!
!BOY COMPUTES THE DISTANCE TO THE BOTTOM
            IF (IPLAN .EQ. 1) THEN
              DISTFOND =  (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
            ELSE
              DISTFOND =  (Z(IPOIN2,IPLAN)-Z(IPOIN2,1))
            ENDIF
! COMPUTES THE TANGENTIAL SPEED
            UTANG = SQRT(U(IPOIN2,IPLAN)**2+V(IPOIN2,IPLAN)**2)
!
!           DIRICHLET ON K
!           ---------------
!
            IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!              ************************************
!
!              COMING INTO THE DOMAIN
!
               KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN),KMIN)
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
!              ****************************************
!
!                WALL
!
                 KBORL(IPTFR,IPLAN)=MAX(SBETAS*UETCAL(IPTFR,IPLAN),KMIN)
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
                  KBORL(IPTFR,IPLAN) = KMIN
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
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!              ************************************
!
!                 COMING INTO THE DOMAIN : TURBULENCE DUE TO BOTTOM
!
! BOUNDARY CONDITIONS COMING RFOM THE K-EPSILON MODEL:
!                  EBORL(IPTFR,IPLAN) = CMU**0.75*KBORL(IPTFR,1)**1.5
!     &                      /KARMAN/DISTFOND
! AUXILIARY RELATION EPSILON=BETAS*K*OMEGA LEADS TO:
!
                  EBORL(IPTFR,IPLAN)=BETAS**(-0.25)*SQRT(KBORL(IPTFR,1))
     &                              /KARMAN/DISTFOND
                  EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
!              ****************************************
!
!               WALL
!
                EBORL(IPTFR,IPLAN) =
     &          MAX(SBETAS*SQRT(UETCAL(IPTFR,IPLAN))/(KARMAN*DIST),EMIN)
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
!HOL OMEGA = USTAR**2*SR/NUE ...
!
                 EBORL(IPTFR,IPLAN) = 2500.D0
!HOL
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
101   FORMAT(' KOMCL3 : REGIME DE TURBULENCE INCONNU : ',I6)
102   FORMAT(' KOMCL3 : UNKNOWN TURBULENCE MODEL : ',I6)
111   FORMAT(' KOMCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR KBOR : LIUBOR =',I6)
112   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
121   FORMAT(' KOMCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR EBOR : LIUBOR =',I6)
122   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE KOMCL3