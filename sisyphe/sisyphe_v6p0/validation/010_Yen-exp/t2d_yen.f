C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
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
      INTEGER LNG,LU, i
      DOUBLE PRECISION L
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
!SW Änderung des Anfangsgefälles des Bodens
        DO i=1,npoin
         IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.6371) THEN
           ZF%R(i) = (ABS(Y(i)-42.6371D0))*(0.0016D0) + 9.98191D0
         ENDIF
        IF (Y(i).GT.42.6371D0) THEN
!         IF (((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2).GE.12.0409D0
!     *  .AND.((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!     *  .LE.20.5209D0) THEN
!Berechen der Länge eines Kreisausschnittes
          IF (X(I).LT.101.1819D0) THEN
!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
          L=3.141592654D0*4.0D0*
     *     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
     *     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
     *     3.141592654D0
          ZF%R(i) = L*(0.0016D0) + 9.97072D0
          ENDIF
          IF (X(I).GT.101.1819D0) THEN
!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
          L=3.141592654D0*4.D0*
     *     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
     *     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
     *     3.141592654D0
          ZF%R(i) = L*(-0.0016D0) + 9.97072D0
          ENDIF
!         ENDIF
        ENDIF
         IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6371D0) THEN
         ZF%R(i) = (ABS(Y(i)-42.6371D0))*(-0.0016D0) + 9.95954D0
         ENDIF
        IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.8D0.AND.Y(I).GE.42.5D0)
     *  THEN  
        ZF%R(i) = 9.98145D0
        ENDIF
        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6D0.AND.Y(I).GE.42.45D0)
     *  THEN
        ZF%R(i) = 9.960055D0
        ENDIF
        END DO
C
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
