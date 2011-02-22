!                       ******************************
                        SUBROUTINE COMPLETE_EBE_FLUINT
!                       ******************************
!
     &(EBE_FLUINT,NELEM2,NPLAN)
!
!=======================================================================
! TELEMAC 3D VERSION 6.1  26/08/2010  J-M HERVOUET (LNHE) 01 30 87 80 18    
!
!=======================================================================
!
! FUNCTION: COMPLETING ELEMENT BY ELEMENT FLUINT AT THE FREE SURFACE
!           VALUE OF UPPER PLANE AS MINUS THE SUM OF ALL COEFFICIENTS
!           ON THE VERTICAL
!
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! | VARIABLE NAME  |MODE|  FUNCTION                                    | 
! |________________|____|______________________________________________| 
! | EBE_FLUINT     |<-->| INTERIOR FLUXES TO BE COMPLETED  
! | NELEM2         | -->| NUMBER OF 2D ELEMENTS
! | NPLAN          | -->| NUMBER OF PLANES ON THE VERTICAL
! |________________|____|______________________________________________|
! MODE: -->(VARIABLE NOT MODIFIED), <--(RESULT), <-->(MODIFIED VARIABLE)
!     
! SUBROUTINE CALLED BY: TELEMAC3D
! SUBROUTINE CALLS:
!======================================================================
!    
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: EBE_FLUINT(NELEM2,NPLAN-1,6)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ILAYER,NLAYER,IELEM2
!
!-----------------------------------------------------------------------
!
      NLAYER=NPLAN-1
!
!-----------------------------------------------------------------------
!      
!     UPPER LAYER: INITIALISING WITH COEFFICIENTS OF LOWER PLANE
!
      DO IELEM2=1,NELEM2
        EBE_FLUINT(IELEM2,NLAYER,4)=-EBE_FLUINT(IELEM2,NLAYER,1)
        EBE_FLUINT(IELEM2,NLAYER,5)=-EBE_FLUINT(IELEM2,NLAYER,2)
        EBE_FLUINT(IELEM2,NLAYER,6)=-EBE_FLUINT(IELEM2,NLAYER,3)
      ENDDO
!
!     OTHER LAYERS
!
      IF(NLAYER.GE.2) THEN
        DO ILAYER=1,NLAYER-1
          DO IELEM2=1,NELEM2
            EBE_FLUINT(IELEM2,NLAYER,4)=EBE_FLUINT(IELEM2,NLAYER,4)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,1)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,4)
            EBE_FLUINT(IELEM2,NLAYER,5)=EBE_FLUINT(IELEM2,NLAYER,5)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,2)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,5)
            EBE_FLUINT(IELEM2,NLAYER,6)=EBE_FLUINT(IELEM2,NLAYER,6)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,3)
     *                                 -EBE_FLUINT(IELEM2,ILAYER,6)
          ENDDO          
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
! 
      RETURN
      END
