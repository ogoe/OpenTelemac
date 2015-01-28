!                    ***********************************
                     DOUBLE PRECISION FUNCTION T3D_TRSCE
!                    ***********************************
!
     &( TIME , I , ITRAC )
!
!***********************************************************************
! TELEMAC3D   V6P2                                   08/11/2011
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
!history  C. COULET (ARTELIA GROUP)
!+        07/10/2011
!+        V6P2
!+   Modification size FCT and OK due to modification of TRACER
!+    numbering TRACER is now identified by 2 values (Isource, Itracer)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| SOURCE RANK
!| ITRAC          |-->| TRACER RANK
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: AT,INFOGR,NTRAC,TASCE,NSCE,
     &                                  T3D_FILES,T3DVEF,MAXSCE,MAXTRA,
     &                                  MAXFRO
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
      CHARACTER*9 FCT
      INTEGER N,M,IRANK
      LOGICAL,SAVE :: DEJA=.FALSE.
      LOGICAL, DIMENSION(MAXFRO,MAXTRA), SAVE :: OK
!
!     FIRST CALL, INITIALISES OK TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        IF(NSCE.GT.MAXSCE.OR.NTRAC.GT.MAXTRA) THEN
          WRITE(LU,*) 'CHANGE DIMENSION OF OK IN TRSCE, ',NSCE,',',
     &                NTRAC,' REQUIRED'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO M=1,NTRAC
           DO N=1,NSCE
              OK(N,M)=.TRUE.
           ENDDO
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF SOURCE FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OK(I,ITRAC).AND.T3D_FILES(T3DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE TR(I,ITRAC) DEPENDING ON I AND ITRAC
        FCT='TR(      '
        IRANK=4
        IF(I.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') I
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=','
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') I
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=','
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 SOURCES'
          CALL PLANTE(1)
          STOP
        ENDIF
        IRANK=IRANK+1
        IF(ITRAC.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') ITRAC
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=')'
        ELSEIF(ITRAC.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') ITRAC
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=')'
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 TRACERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(T3D_TRSCE,FCT,AT,T3D_FILES(T3DVEF)%LU,
     &                        INFOGR,OK(I,ITRAC))
!
      ENDIF
!
!     BEWARE: AN ERROR IN THE SOURCES FILE MAY REMAIN UNNOTICED
!     BECAUSE RESORTS HERE TO THE STEERING FILE
!
      IF(.NOT.OK(I,ITRAC).OR.T3D_FILES(T3DVEF)%NAME(1:1).EQ.' ') THEN
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
