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
!
      DOUBLE PRECISION PI,L0,LO,HDUNE,LM
      INTEGER I
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  Bosse a t=0
!

!      Lo=1.5D0
      DO I=1,NPOIN
        ZF%R(I) = 0.D0

        IF (Y(I) .GE. 8138.D0) THEN
!         <20
          ZF%R(I)=0.000904*(Y(I)-8138)+10.26
          PRINT *,Y(I),ZF%R(I)
        ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN
!        20-40
          ZF%R(I)=0.000491*(Y(I)-7474)+9.94

        ELSEIF((Y(I).LE.7474).AND.(Y(I).GE.6922.D0)) THEN
!        40-60
          ZF%R(I)=0.000392*(Y(I)-6922)+9.72

        ELSEIF((Y(I).LE.6922).AND.(Y(I).GE.6378.D0)) THEN
!        60-80
          ZF%R(I)=0.000807*(Y(I)-6378)+9.28
        ELSEIF((Y(I).LE.6378).AND.(Y(I).GE.5766.D0)) THEN
!        80-100
          ZF%R(I)=0.00119*(Y(I)-5766)+8.55
        ELSEIF((Y(I).LE.5766).AND.(Y(I).GE.5256.D0)) THEN
!        100-120
          ZF%R(I)=0.00061*(Y(I)-5256)+8.24
        ELSEIF((Y(I).LE.5256).AND.(Y(I).GE.4708.D0)) THEN
!        120-140
          ZF%R(I)=0.00059*(Y(I)-4708)+7.92
        ELSEIF((Y(I).LE.4708).AND.(Y(I).GE.4064.D0)) THEN
!        140-160
          ZF%R(I)=0.000364*(Y(I)-4064)+7.68
        ELSEIF(Y(I).LE.4064) THEN
!        140-160
          ZF%R(I)=0.000716*(Y(I)-3042)+6.95

        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END

