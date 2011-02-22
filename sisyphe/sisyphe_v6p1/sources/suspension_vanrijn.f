      ! ***************************** !
        SUBROUTINE SUSPENSION_VANRIJN ! (_IMP_)
      ! ***************************** !

     &  (ACLADM, TAUP, NPOIN, GRAV, 
     &   XMVE, XMVS, ZERO, AC, CSTAEQ,ZREF)
c nh attention ajout VCE, Zref

C**********************************************************************C
C SISYPHE VERSION 5.6  04/01/05  F. HUVELIN                            C
C SISYPHE VERSION 5.5  14/04/04  C. VILLARET  01 30 87 83 28           C
C**********************************************************************C


         ! ==================================================== !
         !   Reference concentration calculation at z= 2*d50    !
         ! thanks to the formula of Zyserman and Fredsoe (1994) !
         ! ==================================================== !

C
C
C 13/06/2008 : JMH : OPTIMISATION FORMULE AVEC AUX
C
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
C**********************************************************************C
C                                                                      C
C                 SSSS I   SSSS Y   Y PPPP  H   H EEEEE                C
C                S     I  S      Y Y  P   P H   H E                    C
C                 SSS  I   SSS    Y   PPPP  HHHHH EEEE                 C
C                    S I      S   Y   P     H   H E                    C
C                SSSS  I  SSSS    Y   P     H   H EEEEE                C
C                                                                      C
C----------------------------------------------------------------------C
C                             ARGUMENTS                                C
C .________________.____.______________________________________________C
C |      NOM       |MODE|                   ROLE                       C
C |________________|____|______________________________________________C
C |   ACLADM       | => |
C |   CF           | => |
C |   TOB          | => |
C |   HCLIP        | => |
C |   AVA          | => |
C |   NPOIN        | => |
C |   CHARR        | => |
C |   KSPRATIO     | => |
C |   HMIN         | => |
C |   GRAV         | => |
C |   XMVE         | => |
C |   XMVS         | => |
C |   AC           | <=>|
C |   FLUER        | <= |
C !________________|____|______________________________________________C
C                    <=  Can't be change by the user                   C
C                    =>  Can be changed by the user                    C 
C ---------------------------------------------------------------------C
!                                                                      !
! CALLED BY SUSPENSION_FLUX                                            !
!                                                                      !
! CALL      ------                                                     !
!                                                                      !
!======================================================================!
!======================================================================!
!                    DECLARATION DES TYPES ET DIMENSIONS               !
!======================================================================!
!======================================================================!

      ! 1/ MODULES
      ! ----------
c      USE INTERFACE_SISYPHE,EX_SUSPENSION_FREDSOE => SUSPENSION_FREDSOE
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : VCE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, TAUP,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TAUC,AUX,DSTAR,DENS
      INTRINSIC MAX

!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!

      ! ******************************** !
      !    I - CRITICAL SHIELD PARAMETER ! 
      ! ******************************** !    
!
      DO I=1,NPOIN

         ! ****************** !
         ! II - SKIN FRICTION ! 
         ! ****************** !

          TAUC = AC * (GRAV*(XMVS-XMVE)*ACLADM%R(I))
          DENS  = (XMVS - XMVE )/ XMVE

          DSTAR = ACLADM%R(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0) 

         ! ***************** !
         ! IV - EROSION FLUX ! (_IMP_)
         ! ***************** !
         ! Concentration increased by AVA because it is assumed 
         ! that it is computed only with one class of sediment
           IF(DSTAR.LE. ZERO) THEN
           	PRINT*, 'ERROR SUSPENSION_VANRIJN'
           	CALL PLANTE(1)    
           ENDIF
           AUX=(TAUP%R(I)-TAUC)/TAUC
           IF(AUX.GT.ZERO) THEN          
              CSTAEQ%R(I)=0.015*ACLADM%R(I)*SQRT(AUX**3.D0)/
     *                (ZREF%R(I)*DSTAR**0.3D0)     
      
           ELSE
             CSTAEQ%R(I) = 0.D0
           ENDIF
       
c      
      ENDDO
      
!======================================================================!
!======================================================================!

      RETURN      
      END SUBROUTINE SUSPENSION_VANRIJN
