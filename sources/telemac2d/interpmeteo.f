!                    **********************
                     SUBROUTINE INTERPMETEO
!                    **********************
!
     &(WW,WINDX,WINDY,TAIR,PATM,HREL,NEBU,RAINFALL,AT,NFO)
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
!| HREL           |<--| RELATIVE HUMIDITY
!| NEBU           |<--| NEBULOSITY
!| NFO            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| PATM           |<--| ATMOSPHERIC PRESSURE
!| RAINFALL       |<--| RAINFALL
!| TAIR           |<--| AIR TEMPERATURE
!| WINDX          |<--| WIND ALONG X
!| WINDY          |<--| WIND ALONG Y
!| WW             |<--| MAGNITUDE OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NFO
      DOUBLE PRECISION, INTENT(IN)  :: AT
      DOUBLE PRECISION, INTENT(OUT) :: WW,WINDX,WINDY,TAIR,PATM,HREL
      DOUBLE PRECISION, INTENT(OUT) :: NEBU,RAINFALL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER POSTAB,I,NBENR
!     NUMBER OF VARIABLES IN THE ASCII FILE NFO
      INTEGER, PARAMETER :: NINPUTVAR = 8
      INTEGER, PARAMETER :: NLINESTOSKIP = 2
!
      DOUBLE PRECISION DELTAT,ALPHA
      DOUBLE PRECISION DTR
      DOUBLE PRECISION, ALLOCATABLE :: TABENT(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: DUMMY(:)
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
          WRITE(LU,*)'DEBUT DE LECTURE DU FICHIER D''ENTREE'
          WRITE(LU,*)'====================================='
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)'=================================='
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
        IF(.NOT.DEJA) ALLOCATE(DUMMY(NINPUTVAR))
!
!  READS VARIABLES AND FILLS IN TABENT ARRAY
!
 100    READ(NFO,*,END=20) DUMMY(1:NINPUTVAR)
        NBENR = NBENR + 1
        GO TO 100
!
 20     CONTINUE
!
        IF(.NOT.DEJA) THEN
!  +2 TO STORE 2 EXTRA DATA (THE X AND Y COMPONENTS OF WIND VELOCITY)
          ALLOCATE(TABENT(NBENR,NINPUTVAR+2))
          DEJA = .TRUE.
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
          TABENT(I,9)  = -TABENT(I,2)*SIN(TABENT(I,3)*DTR)
          TABENT(I,10) = -TABENT(I,2)*COS(TABENT(I,3)*DTR)
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
      ALPHA  = (AT-TABENT(POSTAB,1))/DELTAT
!
!-----------------------------------------------------------------------
!
      WW    =  TABENT(POSTAB,2)
     &      + (TABENT(POSTAB+1,2)-TABENT(POSTAB,2))*ALPHA
!
      WINDX =  TABENT(POSTAB,9)
     &      + (TABENT(POSTAB+1,9)-TABENT(POSTAB,9))*ALPHA
      WINDY =  TABENT(POSTAB,10)
     &      + (TABENT(POSTAB+1,10)-TABENT(POSTAB,10))*ALPHA
!
      TAIR  =  TABENT(POSTAB,4)
     &      + (TABENT(POSTAB+1,4)-TABENT(POSTAB,4))*ALPHA
      PATM  =  TABENT(POSTAB,5)
     &      + (TABENT(POSTAB+1,5)-TABENT(POSTAB,5))*ALPHA
      HREL  =  TABENT(POSTAB,6)
     &      + (TABENT(POSTAB+1,6)-TABENT(POSTAB,6))*ALPHA

      NEBU  =  TABENT(POSTAB,7)
     &      + (TABENT(POSTAB+1,7)-TABENT(POSTAB,7))*ALPHA
!
      RAINFALL = TABENT(POSTAB+1,8)/DELTAT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
