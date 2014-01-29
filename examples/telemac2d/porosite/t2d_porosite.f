C                       *****************
                        SUBROUTINE CORPOR
C                       *****************
C
     *(POROS)
C
C***********************************************************************
C PROGICIEL : TELEMAC-2D 5.3          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORPOR
C
C  FUNCTION  : MODIFICATION OF THE POROSITY OF ELEMENTS
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      POROS     |<-->| POROSITY TO BE MODIFIED.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1
      INTEGER NSOM,IELEM
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : POROSITY IS SET TO 0.5 ON A RECTANGLE
C
      NSOM = 4
      XSOM(1) = -50.D0
      XSOM(2) =  50.D0
      XSOM(3) =  50.D0
      XSOM(4) = -50.D0
      YSOM(1) =-21.D0
      YSOM(2) =-21.D0
      YSOM(3) = 21.D0
      YSOM(4) = 21.D0
C                                                                       
C-----------------------------------------------------------------------
C                                                                  
      CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
C                                                                       
C--------------------------------------------------------------
C
      DO IELEM = 1 , NELEM                                         
C                                                                            
        XX1 = (  X(IKLE%I(IELEM)          )+
     *           X(IKLE%I(IELEM+NELMAX)   )+
     *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0 
        YY1 = (  Y(IKLE%I(IELEM)          )+
     *           Y(IKLE%I(IELEM+NELMAX)   )+
     *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
C
        IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
          POROS%R(IELEM) = 0.5D0 * ( 1.D0 + ABS(XX1/50.D0) )
        ENDIF
C                                      
      ENDDO                                                     
C
C-----------------------------------------------------------------------
C
      RETURN
      END
