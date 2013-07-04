!                 **********************************
                  DOUBLE PRECISION FUNCTION CVSP_ALT
!                 **********************************
!
     &(J, FORMULA)
!
!***********************************************************************
! SISYPHE   V6P3                                   12/03/2013
!***********************************************************************
!
!brief   CALCULATES A DYNAMIC ACTIVE LAYER THICKNESS
!+        ACCORDING TO 1 OF A COUPLE OF FORMULAS
!
!
!history  UWE MERKEL
!+        2011-07-20
!+
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| FORMULA        |<--| WHICH FORMULA TO USE TO CALCULATE THE ALT
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID, NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      INTEGER, INTENT(IN) :: J
      INTEGER, INTENT(IN) :: FORMULA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  I,K
      INTEGER LLT, LTT, JG
      DOUBLE PRECISION RHO, RHO_S, G, D50, D90, DMAX, TAUC, TAUB, D
      DOUBLE PRECISION A1, A2, A3, A4, A5, A6, RHOCR, PON, SUMME
      DOUBLE PRECISION AT, DSTAR
      LOGICAL DB
!
!----------------------------------------------------------------------- 
!
! ATTENTION !
!
!----------------------------------------------------------------------- 
!
! EXPECTS GRAIN CLASSES TO BE SORTED IN ASCENDING ORDER 
! UNLIKE SISYPHE!!!
! IMPROVE IT!!!
!
!----------------------------------------------------------------------- 
!
!
!-----------------------------------------------------------------------      
! CHECK ASCENDING ORDER OF CLASSES D50
!-----------------------------------------------------------------------
!
      DO I=1,NSICLA-1
        IF(FDM(I).GE.FDM(I+1)) THEN
          WRITE(LU,*) 'STOPPING!!!! GRAIN CLASSES HAVE TO BE',
     &                ' IN ASCENDING ORDER!!! FOR DYNAMIC ALT'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------      
! BASICS
!-----------------------------------------------------------------------
!
      G = GRAV
      RHO = XMVE
      RHO_S = XMVS
      PON = XKV
!
!-----------------------------------------------------------------------      
! CHARACTERISTIC GRAIN DIAMETERS FOR SURFACE
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------      
! D50 - RAW APROXIMATION OVER LAYER 1 (ACTUALLY IT SHOULD BE SOLVED RECURSSIVE)
!-----------------------------------------------------------------------
!
      D50 = 0.D0
      DO I=1,NSICLA
        D50 =  D50 + FDM(I)*AVAIL(J,1,I)
      ENDDO
!     D50 = ACLADM%R(J)
!     
!-----------------------------------------------------------------------            
! DMAX - FIRST APPROXIMATION
!-----------------------------------------------------------------------
!     
      DO I=1,NSICLA
        IF(AVAIL(J,1,I).GE.0.01D0) DMAX = FDM(NSICLA)
      ENDDO
!
!-----------------------------------------------------------------------            
! D90 - ONLY FIRST APPROXIMATION! 
!-----------------------------------------------------------------------
!
      SUMME = AVAIL(J,1,1)
      D90 = 0.D0
      DO I=2,NSICLA
         SUMME = AVAIL(J,1,I) + SUMME
         IF(SUMME.GE.0.9D0.AND.D90.EQ.0.D0) THEN
            D90 = (0.9D0 - (SUMME-AVAIL(J,1,I)))/AVAIL(J,1,I)*
     &           (FDM(I)-FDM(I-1)) + FDM(I-1)
         ENDIF
      ENDDO
            
      D = D50
!
!-----------------------------------------------------------------------            
! SHEAR PARAMETERS
!
! HERE ARE ENOUGH POSSIBILITIES FOR IMPROVEMENT
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------      
! NON-DIMENSION PARTICLE PARAMETER
!-----------------------------------------------------------------------
!
      DSTAR = D50*((XMVS/XMVE-1.D0)*G/(VCE**2))**(1./3.)
!
!-----------------------------------------------------------------------      
! SHIELDS PARAMETER
!-----------------------------------------------------------------------
!
      IF(DSTAR <= 4.D0) THEN
        RHOCR = 0.24D0*DSTAR**(-1.D0)
      ELSEIF (DSTAR <= 10.D0)THEN
        RHOCR = 0.14D0*DSTAR**(-0.64D0)
      ELSEIF (DSTAR <= 20.D0)THEN
        RHOCR = 0.04D0*DSTAR**(-0.1D0)
      ELSEIF (DSTAR <= 150.D0)THEN
        RHOCR = 0.013D0*DSTAR**(0.29D0)
      ELSE
        RHOCR = 0.055D0
      ENDIF
!      
      TAUC = RHOCR*((XMVS-XMVE)*G*D50)
      TAUB = TOB%R(J)
!     
!-----------------------------------------------------------------------            
! NEW ACTIVE LAYER THICKNESS
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------            
! HUNZIKER & G�NTHER
!-----------------------------------------------------------------------
!
      CVSP_ALT = 5.D0 * DMAX
      A1 = CVSP_ALT
!
!-----------------------------------------------------------------------      
! FREDSOE & DEIGAARD 1992
!-----------------------------------------------------------------------
!
      CVSP_ALT = 2.D0 * TAUB / (G*(RHO_S-RHO))
     &     / TAN(PHISED/180.0D0*PI) / (1.D0-PON)
      A2 = CVSP_ALT
!     
!-----------------------------------------------------------------------            
! VAN RIJN 1993
!-----------------------------------------------------------------------
!
      IF(TAUB.LT.TAUC) THEN
         CVSP_ALT = 0.D0
      ELSE
         CVSP_ALT = 0.3D0*(DSTAR**0.7D0)*((TAUB-TAUC)/TAUC)**0.5*D50
      ENDIF
!      
      A3 = CVSP_ALT
!     
!-----------------------------------------------------------------------      
! WONG 2006
!-----------------------------------------------------------------------
!
      IF((TAUB/(RHO_S-RHO)/G/D50).LT.0.0549D0) THEN
        CVSP_ALT = 0.D0
      ELSE
        CVSP_ALT=5.0D0*D50*((TAUB/(RHO_S-RHO)/G/D50)
     &        -0.0549D0)**0.56D0
      ENDIF
      A4 = CVSP_ALT
!
!-----------------------------------------------------------------------            
! MALCHEREK 2003
!-----------------------------------------------------------------------
!
      CVSP_ALT = D90 / (1.D0-PON) * MAX(1.D0,(TAUB/TAUC))
      A5 = CVSP_ALT
!
!-----------------------------------------------------------------------                  
! SISYPHE
!-----------------------------------------------------------------------
!     
      CVSP_ALT = 3.D0 * D50
      A6 = CVSP_ALT
!     
!-----------------------------------------------------------------------      
! CONSTANT FROM CAS FILE
!-----------------------------------------------------------------------
!
      CVSP_ALT = ELAY0
!
!-----------------------------------------------------------------------      
      
      IF(FORMULA == 0) CVSP_ALT = ELAY0
      IF(FORMULA == 1) CVSP_ALT = A1
      IF(FORMULA == 2) CVSP_ALT = A2
      IF(FORMULA == 3) CVSP_ALT = A3
      IF(FORMULA == 4) CVSP_ALT = A4
      IF(FORMULA == 5) CVSP_ALT = A5
      IF(FORMULA == 6) CVSP_ALT = A6
!
!-----------------------------------------------------------------------            
! CHECK FOR ERRORS
!-----------------------------------------------------------------------
!
      IF(CVSP_ALT.LE.FDM(1)) THEN
        CVSP_ALT = FDM(1)
      ENDIF
!     
!----------------------------------------------------------------------- 
!
      RETURN
      END FUNCTION CVSP_ALT
