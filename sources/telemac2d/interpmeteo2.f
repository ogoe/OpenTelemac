!                    **********************
                     SUBROUTINE INTERPMETEO2
!                    **********************
!
     &(WW,WINDX,WINDY,TAIR,PATM,NEBU,RAINFALL,
     & PVAP,RAY3,AT,NFO)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   09/07/2014
!***********************************************************************
!
!brief    READS AND INTERPOLATES VARIABLES IN AN ASCII FILE
!+
!
!history  R. SAMIE, E. RAZAFINDRAKOTO, C.-T. PHAM (EDF-LNHE)
!+        09/07/2014
!+        V7P0
!+        FROM LAPLUIE AND LECENT
!+
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
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NFO
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: WW,WINDX,WINDY,TAIR,PATM
      DOUBLE PRECISION, INTENT(INOUT) :: NEBU,RAINFALL,PVAP,RAY3
!
!      DOUBLE PRECISION, INTENT(INOUT) :: I0,PCO2,C14_ATM,HT0_ATM,HREL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER POSTAB,I,NBENR
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
      DOUBLE PRECISION, ALLOCATABLE :: TABENT(:,:)
!
      LOGICAL DEJA
      DATA    DEJA /.FALSE./
!
      SAVE
!
!-----------------------------------------------------------------------
!
      DTR = ATAN(1.D0)/45.D0
!
!-----------------------------------------------------------------------
!
!  READS INPUT DATA FILE
!  AT THE FIRST TIME STEP AND FILLS IN TABENT ARRAY
!
      IF (.NOT.DEJA) THEN
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
        NBENR = 0
!
!  READS VARIABLES AND FILLS IN TABENT ARRAY
!
 100    READ(NFO,*,END=20) DUMMY
        NBENR = NBENR + 1
        GO TO 100
!
 20     CONTINUE
!
        IF(.NOT.DEJA) THEN
          IF(NBENR.EQ.0)THEN
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
!          +2 TO CONSIDER WINDX AND WINDY
            ALLOCATE(TABENT(NBENR,NINPUTVAR+2))
            DEJA = .TRUE.
          ENDIF
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
!  READS VARIABLES AND FILLS IN TABENT ARRAY
!
        DO I=1,NBENR
          READ(NFO,*)TABENT(I,1:NINPUTVAR)
        ENDDO
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'======================================='
          WRITE(LU,*)'  FIN DE LECTURE DU FICHIER D''ENTREE  '
          WRITE(LU,*)'     IL Y A ',NBENR,' ENREGISTREMENTS  '
          WRITE(LU,*)'  DE T = ',TABENT(1,1), ' A  = ',
     &               TABENT(NBENR,1),' SECONDES '
          WRITE(LU,*)'======================================='
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)'======================================='
          WRITE(LU,*)'  END OF READING OF INPUT DATA         '
          WRITE(LU,*)'  THERE ARE ',NBENR,' RECORDS          '
          WRITE(LU,*)'  FROM T = ',TABENT(1,1), ' TO = ',
     &               TABENT(NBENR,1),' SECONDS '
          WRITE(LU,*)'======================================='
        ENDIF
!
!  FILLS IN WITH X AND Y COMPONENTS OF WIND VELOCITY
!
        DO I=1,NBENR
          TABENT(I,NINPUTVAR+1) = -TABENT(I,4)*SIN(TABENT(I,5)*DTR)
          TABENT(I,NINPUTVAR+2) = -TABENT(I,4)*COS(TABENT(I,5)*DTR)
        ENDDO
!
!  END TO FILL IN TABENT ARRAY
!  INITIALISATION OF THE POINTER POSTAB
!
        POSTAB = 1
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATES DATA AT EACH TIME STEP
!  POINTER POSTAB POSITIONNED AT EACH TIME STEP
!
120   IF(AT.LT.TABENT(POSTAB,1).OR.AT.GE.TABENT(POSTAB+1,1)) THEN
        IF(AT.LT.TABENT(POSTAB,1))   POSTAB = POSTAB - 1
        IF(AT.GE.TABENT(POSTAB+1,1)) POSTAB = POSTAB + 1
        IF(POSTAB.GT.NBENR) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'ATTENTION : LE TEMPS DU CALCUL AT = ', AT
            WRITE(LU,*)'EST SUPERIEUR AU TEMPS MAXIMUM DE VOTRE FICHIER'
            WRITE(LU,*)'DE DONNEES D''ENTREE T = ', TABENT(NBENR,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
            WRITE(LU,*)'IS BIGGER THAN MAXIMUM TIME IN YOUR INPUT DATA '
            WRITE(LU,*)'FILE T = ', TABENT(NBENR,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          CALL PLANTE(1)
        ELSEIF(POSTAB.LT.1) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'ATTENTION : LE TEMPS DU CALCUL AT = ', AT
            WRITE(LU,*)'EST INFERIEUR AU TEMPS MINIMUM DE VOTRE FICHIER'
            WRITE(LU,*)'DE DONNEES D''ENTREE T = ', TABENT(1,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'==============================================='
            WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
            WRITE(LU,*)'IS LOWER THAN MINIMUM TIME IN YOUR INPUT DATA '
            WRITE(LU,*)'FILE T = ', TABENT(1,1)
            WRITE(LU,*)'==============================================='
          ENDIF
          CALL PLANTE(1)
        ENDIF
        GO TO 120
      ENDIF
!
      DELTAT = TABENT(POSTAB+1,1)-TABENT(POSTAB,1)
      IF(DELTAT.GT.1.E-6)THEN
        ALPHA = (AT-TABENT(POSTAB,1))/DELTAT
      ELSE
        ALPHA = 0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!     TAIR
      TAIR  =  TABENT(POSTAB,2)
     &      + (TABENT(POSTAB+1,2)-TABENT(POSTAB,2))*ALPHA
!     PVAP
      PVAP  =  TABENT(POSTAB,3)
     &      + (TABENT(POSTAB+1,3)-TABENT(POSTAB,3))*ALPHA
!     WIND MAGNITUDE
      WW    =  TABENT(POSTAB,4)
     &      + (TABENT(POSTAB+1,4)-TABENT(POSTAB,4))*ALPHA
!    WIND DIRECTION
!      WW    =  TABENT(POSTAB,2)
!     &      + (TABENT(POSTAB+1,2)-TABENT(POSTAB,2))*ALPHA
!     WINDX AND WINDY
      WINDX =  TABENT(POSTAB,NINPUTVAR+1)+(TABENT(POSTAB+1,NINPUTVAR+1)
     &        -TABENT(POSTAB,NINPUTVAR+1))*ALPHA
      WINDY =  TABENT(POSTAB,NINPUTVAR+2)+(TABENT(POSTAB+1,NINPUTVAR+2)
     &        -TABENT(POSTAB,NINPUTVAR+2))*ALPHA
!     NEBULOSITY
      NEBU  =  TABENT(POSTAB,6)
     &      + (TABENT(POSTAB+1,6)-TABENT(POSTAB,6))*ALPHA
!     RAY3
      RAY3  =  TABENT(POSTAB,7)
     &      + (TABENT(POSTAB+1,7)-TABENT(POSTAB,7))*ALPHA
!     PATM
      PATM  =  TABENT(POSTAB,8)
     &      + (TABENT(POSTAB+1,8)-TABENT(POSTAB,8))*ALPHA
!     RAINFALL
      RAINFALL = TABENT(POSTAB+1,9)/MAX(DELTAT,EPS)
! !     I0
!       I0  =  TABENT(POSTAB,10)
!      &      + (TABENT(POSTAB+1,10)-TABENT(POSTAB,10))*ALPHA
! !     PCO2
!       PCO2  =  TABENT(POSTAB,11)
!      &      + (TABENT(POSTAB+1,11)-TABENT(POSTAB,11))*ALPHA
!
! !     RELATIVE HUMIDITY (MOISTURE)
!       HREL  =  TABENT(POSTAB,12)
!      &      + (TABENT(POSTAB+1,12)-TABENT(POSTAB,12))*ALPHA
!
!
! !     C14_ATM
!       C14_ATM  =  TABENT(POSTAB,13)
!      &      + (TABENT(POSTAB+1,13)-TABENT(POSTAB,13))*ALPHA
! !     HT0_ATM
!       HTO_ATM =  TABENT(POSTAB,14)
!      &      + (TABENT(POSTAB+1,14)-TABENT(POSTAB,14))*ALPHA
!
!-----------------------------------------------------------------------
!
      RETURN
      END
