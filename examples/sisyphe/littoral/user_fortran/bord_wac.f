!                  *******************
                   SUBROUTINE BORD_WAC
!                  *******************
!
     &(F,NPLAN,NF,NPOIN2,IP)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    F           ! <->!  DENSITE SPECTRALE                           !
! !    NPLAN       ! -->!  NOMBRE DE DIRECTIONS                        !
! !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
! !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : LIMWAC
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH, NCSIZE
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
!
      INTEGER IFF,IPLAN
      INTEGER IP, IMIL, IDRO, IGAU
      DOUBLE PRECISION DUMMY(100,100)
      DOUBLE PRECISION P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
!
!***********************************************************************
!
      IMIL=1117+IP-1
      IF (IMIL.EQ.1156) IMIL=116
      IGAU=180-IP+1
      IDRO= 52+IP-1
!
      IMIL=GLOBAL_TO_LOCAL_POINT(IMIL,MESH)
      IF(IMIL.EQ.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
            DUMMY(IPLAN,IFF)=0.D0
          ENDDO
        ENDDO
      ELSE
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
            DUMMY(IPLAN,IFF)=F(IMIL,IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
!
      DO IFF=1,NF
        DO IPLAN=1,NPLAN
          DUMMY(IPLAN,IFF) = P_DMAX(DUMMY(IPLAN,IFF))+
     &                       P_DMIN(DUMMY(IPLAN,IFF))
        ENDDO
      ENDDO
!
      IGAU=GLOBAL_TO_LOCAL_POINT(IGAU,MESH)
      IDRO=GLOBAL_TO_LOCAL_POINT(IDRO,MESH)
      IF(IGAU.GT.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           F(IGAU,IPLAN,IFF) = DUMMY(IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
      IF(IDRO.GT.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           F(IDRO,IPLAN,IFF) = DUMMY(IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END

