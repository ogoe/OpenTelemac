!                       *****************
                        SUBROUTINE CORPOR
!                       *****************
!
     &(POROS)
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORPOR
!
!  FUNCTION  : MODIFICATION OF THE POROSITY OF ELEMENTS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      POROS     |<-->| POROSITY TO BE MODIFIED.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1,POR
      INTEGER NSOM,IELEM
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
!
!     Surface de 20 x 40 centree sur (0,0)
!
      NSOM = 4
      XSOM(1) = -20.D0
      XSOM(2) =  20.D0
      XSOM(3) =  20.D0
      XSOM(4) = -20.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
!
!-----------------------------------------------------------------------
!
      CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!
!--------------------------------------------------------------
!
      POR=19.D0/20.D0
!
      DO IELEM = 1 , NELEM
!
        XX1 = (  X(IKLE%I(IELEM)          )+
     &           X(IKLE%I(IELEM+NELMAX)   )+
     &           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
        YY1 = (  Y(IKLE%I(IELEM)          )+
     &           Y(IKLE%I(IELEM+NELMAX)   )+
     &           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!
        IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
          IF(XX1.GE.-10.D0.AND.XX1.LE.10.D0) THEN
            POROS%R(IELEM) = POR
          ELSEIF(XX1.LT.-10.D0) THEN
            POROS%R(IELEM) = POR - (1.D0-POR) * (XX1+10.D0) / 10.D0
          ELSEIF(XX1.GT.10.D0) THEN
            POROS%R(IELEM) = POR + (1.D0-POR) * (XX1-10.D0) / 10.D0
          ENDIF
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

