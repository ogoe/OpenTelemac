!                       *****************
                        SUBROUTINE TOM_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : COWADIS           26/07/99           F.MARCOS
!***********************************************************************
!
!  USER SUBROUTINE TOM_CORFON
!
!  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
!
      USE DECLARATIONS_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!MB--------------------------------------Modif debut
      INTEGER IP
!
      DO IP=1,NPOIN2
        ZF(IP)=-200.D0
      ENDDO
!MB--------------------------------------Modif fin
!
      RETURN
      END

