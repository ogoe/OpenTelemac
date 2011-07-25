!                    ***********************************
                     DOUBLE PRECISION FUNCTION T3D_TRSCE
!                    ***********************************
!
     &( TIME , I , ITRAC )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE TRACER AT THE SOURCES
!+               (CAN BE A FUNCTION OF TIME).
!
!history  J-M HERVOUET (LNHE)
!+        04/04/08
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| SOURCE RANK
!| ITRAC          |-->| TRACER RANK
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: AT,INFOGR,NTRAC,TASCE,NSCE,
     &                                  T3D_FILES,T3DVEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER N,IRANK
      LOGICAL DEJA,OK(99)  ! 99.GE.NSCE*NTRAC
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, INITIALISES OK TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        IF(NSCE*NTRAC.GT.99) THEN
          WRITE(LU,*) 'CHANGE DIMENSION OF OK IN TRSCE, ',NSCE*NTRAC,
     &                ' REQUIRED'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO N=1,NSCE*NTRAC
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF SOURCE FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
!     IRANK CORRESPONDS TO TELEMAC-3D DOCUMENTATION FOR
!     VALUES OF TRACERS AT SOURCES
      IRANK=ITRAC+NTRAC*(I-1)
      IF(OK(IRANK).AND.T3D_FILES(T3DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE T(1), T(2), ETC, T(99), DEPENDING ON I AND ITRAC
        FCT='TR(     '
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:5)=')'
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 DATA'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(T3D_TRSCE,FCT,AT,T3D_FILES(T3DVEF)%LU,
     &                        INFOGR,OK(IRANK))
!
      ENDIF
!
!     BEWARE: AN ERROR IN THE SOURCES FILE MAY REMAIN UNNOTICED
!     BECAUSE RESORTS HERE TO THE STEERING FILE
!
      IF(.NOT.OK(I).OR.T3D_FILES(T3DVEF)%NAME(1:1).EQ.' ') THEN
!
!       PROGRAMMABLE PART
!       TASCE IS TAKEN FROM THE STEERING FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-3D
        T3D_TRSCE = TASCE(I,ITRAC)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
