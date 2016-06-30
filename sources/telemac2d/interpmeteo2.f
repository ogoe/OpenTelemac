!                    ***********************
                     SUBROUTINE INTERPMETEO2
!                    ***********************
!
     &(WW,WINDX,WINDY,TAIR,PATM,NEBU,RAINFALL,PVAP,RAY3,AT,NFO)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    READS AND INTERPOLATES VARIABLES IN AN ASCII FILE
!+
!
!history  R. SAMIE, E. RAZAFINDRAKOTO, C.-T. PHAM (EDF-LNHE)
!+        09/07/2014
!+        V7P0
!+    First version
!
!history  J-M HERVOUET (EDF-LNHE)
!+        17/09/2015
!+        V7P1
!+    Avoiding an allocation of array by the compiler during a READ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF TIME STEP
!| C14_ATM        |<--|
!| HREL           |<--| RELATIVE HUMIDITY
!| HT0_ATM        |<--|
!| I0             |<--|
!| NEBU           |<--| NEBULOSITY
!| NFO            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| PATM           |<--| ATMOSPHERIC PRESSURE
!| RAINFALL       |<--| RAINFALL
!| RAY3           |<--|
!| TAIR           |<--| AIR TEMPERATURE
!| TVAP           |<--|
!| WINDX          |<--| WIND ALONG X
!| WINDY          |<--| WIND ALONG Y
!| WW             |<--| MAGNITUDE OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_INTERPMETEO2 => INTERPMETEO2
      USE DECLARATIONS_TELEMAC2D, ONLY: DEJA_IPM2,TABENT_IPM2,
     &                                  POSTAB_IPM2,NBENR_IPM2
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NFO
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: WW,WINDX,WINDY,TAIR,PATM
      DOUBLE PRECISION, INTENT(INOUT) :: NEBU,RAINFALL,PVAP,RAY3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!     NUMBER OF VARIABLES IN THE ASCII FILE NFO
!       INTEGER, PARAMETER :: NINPUTVAR = 8
      INTEGER, PARAMETER :: NINPUTVAR = 9
!     DESCRIPTION OF THE METEO FILE (GENERAL CASE)
!      1-TIME 2-TAIR 3-PVAP 4- WIND MAGN 5- WIND DIR
!      6 NEBULO 7-RAY3 8-PATM 9-RAIN
!
      INTEGER, PARAMETER :: NLINESTOSKIP = 2
!
      DOUBLE PRECISION,PARAMETER :: EPS=1.D-6
      DOUBLE PRECISION DELTAT,ALPHA
      DOUBLE PRECISION DTR,DUMMY
!
!-----------------------------------------------------------------------
!
      DTR = ATAN(1.D0)/45.D0
!
!-----------------------------------------------------------------------
!
!  READS INPUT DATA FILE
!  AT THE FIRST TIME STEP AND FILLS IN TABENT_IPM2 ARRAY
!
      IF (.NOT.DEJA_IPM2) THEN
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'====================================='
          WRITE(LU,*)'INTERPMETEO2.f :                     '
          WRITE(LU,*)'DEBUT DE LECTURE DU FICHIER D''ENTREE'
          WRITE(LU,*)'====================================='
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)'=================================='
          WRITE(LU,*)'INTERPMETEO2.f :                  '
          WRITE(LU,*)'BEGINNING OF READING OF INPUT FILE'
          WRITE(LU,*)'=================================='
        ENDIF
!
        REWIND NFO
!
!  READS THE HEADLINE OF THE DATA FILE
!
        DO I=1,NLINESTOSKIP
          READ(NFO,*)
        ENDDO
!
        NBENR_IPM2 = 0
!
!  READS VARIABLES AND FILLS IN TABENT_IPM2 ARRAY
!
 100    READ(NFO,*,END=20) DUMMY
        NBENR_IPM2 = NBENR_IPM2 + 1
        GO TO 100
!
 20     CONTINUE
!
        IF(NBENR_IPM2.EQ.0)THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'====================================='
            WRITE(LU,*)'FICHIER METEO VIDE  !                '
            WRITE(LU,*)'====================================='
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'=================================='
            WRITE(LU,*)' METEO FILE EMPTY   !             '
            WRITE(LU,*)'=================================='
          ENDIF
          CALL PLANTE(1)
          STOP
        ELSE
!        +2 TO CONSIDER WINDX AND WINDY
          ALLOCATE(TABENT_IPM2(NBENR_IPM2,NINPUTVAR+2))
          DEJA_IPM2 = .TRUE.
        ENDIF
!
!-----------------------------------------------------------------------
!
        REWIND NFO
!
!  READS THE HEADLINE OF THE DATA FILE
!
        DO I=1,NLINESTOSKIP
          READ(NFO,*)
        ENDDO
!
!  READS VARIABLES AND FILLS IN TABENT_IPM2 ARRAY
!
        DO I=1,NBENR_IPM2
          READ(NFO,*) (TABENT_IPM2(I,J),J=1,NINPUTVAR)
        ENDDO
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'======================================='
          WRITE(LU,*)'  FIN DE LECTURE DU FICHIER D''ENTREE  '
          WRITE(LU,*)'     IL Y A ',NBENR_IPM2,' ENREGISTREMENTS  '
          WRITE(LU,*)'  DE T = ',TABENT_IPM2(1,1), ' A  = ',
     &               TABENT_IPM2(NBENR_IPM2,1),' SECONDES '            
          WRITE(LU,*)'======================================='
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)'======================================='
          WRITE(LU,*)'  END OF READING OF INPUT DATA         '
          WRITE(LU,*)'  THERE ARE ',NBENR_IPM2,' RECORDS          '
          WRITE(LU,*)'  FROM T = ',TABENT_IPM2(1,1), ' TO = ',
     &               TABENT_IPM2(NBENR_IPM2,1),' SECONDS '            
          WRITE(LU,*)'======================================='
        ENDIF
!
!  FILLS IN WITH X AND Y COMPONENTS OF WIND VELOCITY
!
        DO I=1,NBENR_IPM2
          TABENT_IPM2(I,NINPUTVAR+1) = -TABENT_IPM2(I,4)
     &                                  *SIN(TABENT_IPM2(I,5)*DTR)
          TABENT_IPM2(I,NINPUTVAR+2) = -TABENT_IPM2(I,4)
     &                                  *COS(TABENT_IPM2(I,5)*DTR)
        ENDDO
!
!  END TO FILL IN TABENT_IPM2 ARRAY
!  INITIALISATION OF THE POINTER POSTAB_IPM2
!
        POSTAB_IPM2 = 1
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATES DATA AT EACH TIME STEP
!  POINTER POSTAB_IPM2 POSITIONNED AT EACH TIME STEP
!
120   IF(AT.LT.TABENT_IPM2(POSTAB_IPM2,1).OR.
     &   AT.GE.TABENT_IPM2(POSTAB_IPM2+1,1)) THEN
        IF(AT.LT.TABENT_IPM2(POSTAB_IPM2,1)) THEN
          POSTAB_IPM2 = POSTAB_IPM2 - 1
        ENDIF
        IF(AT.GE.TABENT_IPM2(POSTAB_IPM2+1,1)) THEN
          POSTAB_IPM2 = POSTAB_IPM2 + 1
        ENDIF
        IF(POSTAB_IPM2.GT.NBENR_IPM2) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'ATTENTION : LE TEMPS DU CALCUL AT = ', AT
            WRITE(LU,*)'EST SUPERIEUR AU TEMPS MAXIMUM DE VOTRE FICHIER'
            WRITE(LU,*)'DE DONNEES D''ENTREE T = ', 
     &                  TABENT_IPM2(NBENR_IPM2,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
            WRITE(LU,*)'IS BIGGER THAN MAXIMUM TIME IN YOUR INPUT DATA '
            WRITE(LU,*)'FILE T = ', TABENT_IPM2(NBENR_IPM2,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          CALL PLANTE(1)
        ELSEIF(POSTAB_IPM2.LT.1) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'ATTENTION : LE TEMPS DU CALCUL AT = ', AT
            WRITE(LU,*)'EST INFERIEUR AU TEMPS MINIMUM DE VOTRE FICHIER'
            WRITE(LU,*)'DE DONNEES D''ENTREE T = ', TABENT_IPM2(1,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
            WRITE(LU,*)'IS LOWER THAN MINIMUM TIME IN YOUR INPUT DATA '
            WRITE(LU,*)'FILE T = ', TABENT_IPM2(1,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          CALL PLANTE(1)
        ENDIF
        GO TO 120
      ENDIF
!
      DELTAT = TABENT_IPM2(POSTAB_IPM2+1,1)-TABENT_IPM2(POSTAB_IPM2,1)
      IF(DELTAT.GT.1.E-6)THEN
        ALPHA = (AT-TABENT_IPM2(POSTAB_IPM2,1))/DELTAT
      ELSE
        ALPHA = 0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!     TAIR
      TAIR  =  TABENT_IPM2(POSTAB_IPM2,2)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,2)
     &         -TABENT_IPM2(POSTAB_IPM2,2))*ALPHA
!     PVAP
      PVAP  =  TABENT_IPM2(POSTAB_IPM2,3)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,3)
     &         -TABENT_IPM2(POSTAB_IPM2,3))*ALPHA
!     WIND MAGNITUDE
      WW    =  TABENT_IPM2(POSTAB_IPM2,4)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,4)
     &         -TABENT_IPM2(POSTAB_IPM2,4))*ALPHA
!    WIND DIRECTION
!      WW    =  TABENT_IPM2(POSTAB_IPM2,2)
!     &      + (TABENT_IPM2(POSTAB_IPM2+1,2)-TABENT_IPM2(POSTAB_IPM2,2))*ALPHA
!     WINDX AND WINDY
      WINDX =  TABENT_IPM2(POSTAB_IPM2,NINPUTVAR+1)
     &        +(TABENT_IPM2(POSTAB_IPM2+1,NINPUTVAR+1)
     &        -TABENT_IPM2(POSTAB_IPM2,NINPUTVAR+1))*ALPHA
      WINDY =  TABENT_IPM2(POSTAB_IPM2,NINPUTVAR+2)
     &        +(TABENT_IPM2(POSTAB_IPM2+1,NINPUTVAR+2)
     &        -TABENT_IPM2(POSTAB_IPM2,NINPUTVAR+2))*ALPHA
!     NEBULOSITY
      NEBU  =  TABENT_IPM2(POSTAB_IPM2,6)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,6)
     &         -TABENT_IPM2(POSTAB_IPM2,6))*ALPHA
!     RAY3
      RAY3  =  TABENT_IPM2(POSTAB_IPM2,7)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,7)
     &         -TABENT_IPM2(POSTAB_IPM2,7))*ALPHA
!     PATM
      PATM  =  TABENT_IPM2(POSTAB_IPM2,8)
     &      + (TABENT_IPM2(POSTAB_IPM2+1,8)
     &         -TABENT_IPM2(POSTAB_IPM2,8))*ALPHA
!     RAINFALL
      RAINFALL = TABENT_IPM2(POSTAB_IPM2+1,9)/MAX(DELTAT,EPS)
! !     I0
!       I0  =  TABENT_IPM2(POSTAB_IPM2,10)
!      &      + (TABENT_IPM2(POSTAB_IPM2+1,10)-TABENT_IPM2(POSTAB_IPM2,10))*ALPHA
! !     PCO2
!       PCO2  =  TABENT_IPM2(POSTAB_IPM2,11)
!      &      + (TABENT_IPM2(POSTAB_IPM2+1,11)-TABENT_IPM2(POSTAB_IPM2,11))*ALPHA
!
! !     RELATIVE HUMIDITY (MOISTURE)
!       HREL  =  TABENT_IPM2(POSTAB_IPM2,12)
!      &      + (TABENT_IPM2(POSTAB_IPM2+1,12)-TABENT_IPM2(POSTAB_IPM2,12))*ALPHA
!
!
! !     C14_ATM
!       C14_ATM  =  TABENT_IPM2(POSTAB_IPM2,13)
!      &      + (TABENT_IPM2(POSTAB_IPM2+1,13)-TABENT_IPM2(POSTAB_IPM2,13))*ALPHA
! !     HT0_ATM
!       HTO_ATM =  TABENT_IPM2(POSTAB_IPM2,14)
!      &      + (TABENT_IPM2(POSTAB_IPM2+1,14)-TABENT_IPM2(POSTAB_IPM2,14))*ALPHA
!
!-----------------------------------------------------------------------
!
      RETURN
      END

