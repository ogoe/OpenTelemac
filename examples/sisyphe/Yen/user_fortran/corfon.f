!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
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
      DOUBLE PRECISION L, MPZ
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      MPZ = 9.964433629385642D0
!
      DO I=1,NPOIN
!
! part1: straight flume left
! at the end (11.5 m, z = 9.977)
        IF (X(I).LE. 101.1819D0.AND.y(i).LE.42.6371D0) THEN
          ZF%R(I) = 10.D0 - ABS(Y(I)-31.1371D0)*0.002D0
        ENDIF
!
! part II: bend
!
! midpoint is at x 101.1819 and 104.6819 + 4.0
! the length of the circle part is l_circle_part = (pi*4.0)/2.0
! l_circle_part = 6.283185307179586 (1/4 of the circle)
! z at the start of the circle part = 9.977
! z at the middle of the circle part = 9.964433629385642
! z at the end of the circle part = 9.951867258771284
! radius is 4.0
!
        IF (Y(I).GT.42.6371D0) THEN
           L=3.141592654D0*4.0D0*
     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
     &     3.141592654D0
!
          IF(X(I).LT.101.1819D0) THEN
              ZF%R(I) = L*(0.002D0) + MPZ
          ENDIF
!
          IF(X(I).GE.101.1819D0) THEN
            ZF%R(I) = L*(-0.002D0) + MPZ
          ENDIF
        ENDIF
!
! part III: straight part right
! starts with ~ 9.961192852465999
        IF (x(I).GE.101.1819D0.AND.Y(I).LE.42.6371D0) THEN
          ZF%R(I)= 9.951867258771284D0-(42.6371D0-Y(I))*(0.002D0)
        ENDIF
!
      END DO


! Change of  Bottom slope
!      DO I=1,NPOIN
!        IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.6371) THEN
!
!        ENDIF
!        IF (Y(I).GT.42.6371D0) THEN
!!         IF (((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2).GE.12.0409D0
!!     *  .AND.((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!!     *  .LE.20.5209D0) THEN
! Calculating the length of a circle section
!          IF (X(I).LT.101.1819D0) THEN
!!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!            L=3.141592654D0*4.0D0*
!     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
!     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
!     &     3.141592654D0
!            ZF%R(I) = L*(0.0016D0) + 9.97072D0
!          ENDIF
!          IF (X(I).GT.101.1819D0) THEN
!!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!            L=3.141592654D0*4.D0*
!     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
!     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
!     &     3.141592654D0
!            ZF%R(I) = L*(-0.0016D0) + 9.97072D0
!          ENDIF
!!         ENDIF
!        ENDIF
!        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6371D0) THEN
!          ZF%R(I) = (ABS(Y(I)-42.6371D0))*(-0.0016D0) + 9.95954D0
!        ENDIF
!        IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.8D0.AND.Y(I).GE.42.5D0)
!     &  THEN
!          ZF%R(I) = 9.98145D0
!        ENDIF
!        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6D0.AND.Y(I).GE.42.45D0)
!     &  THEN
!          ZF%R(I) = 9.960055D0
!        ENDIF
!      END DO
!
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

