!                    ********************
                     SUBROUTINE TASSEMENT
!                    ********************
!
     &(ZF,NPOIN,DTS,ELAY,DZF_TASS,T2,LT,AVAIL,NSICLA,ES,XMVS,
     & XKV,TRANS_MASS,CONC_VASE,NCOUCH_TASS,MS_SABLE,MS_VASE)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| AVAIL          |---|
!| CONC_VASE      |---|
!| DTS            |---|
!| DZF_TASS       |---|
!| ELAY           |---|
!| ES             |---|
!| LT             |---|
!| MS_SABLE       |---|
!| MS_VASE        |---|
!| NCOUCH_TASS    |---|
!| NPOIN          |---|
!| NSICLA         |---|
!| T2             |---|
!| TRANS_MASS     |---|
!| XKV            |---|
!| XMVS           |---|
!| ZF             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN,NSICLA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ZF,ELAY,T2
      DOUBLE PRECISION, INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      INTEGER, INTENT(IN)             :: LT,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: TRANS_MASS(10),CONC_VASE(10)
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XKV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
      DOUBLE PRECISION CONC_SABLE
!
      DOUBLE PRECISION TAUX(10),TRANSFERT_MASSE_VASE(10)
      DOUBLE PRECISION TRANSFERT_MASSE_SABLE(10)
      DOUBLE PRECISION EPAI_SABLE(10),EPAI_VASE(10)
!
! COMPUTES THE TOTAL SEDIMENT THICKNESS (SAND + MUD) BEFORE CONSOLIDATION
!
      CONC_SABLE=XMVS/XKV
!
! T2: MUD THICKNESS BEFORE CONSOLIDATION
!
      DO I=1,NPOIN
!
        T2%R(I)=0.D0
        DO J=1,NCOUCH_TASS
          EPAI_VASE(J)=MS_VASE(I,J)/CONC_VASE(J)
          ES(I,J)=EPAI_VASE(J)
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J)=MS_SABLE(I,J)/XMVS
            ES(I,J)=EPAI_VASE(J)+EPAI_SABLE(J)
          ENDIF
          T2%R(I)=T2%R(I)+ES(I,J)
        ENDDO
!
        DO J=1,NCOUCH_TASS
          IF(MS_VASE(I,J).GE.1.D-6) THEN
            TRANSFERT_MASSE_VASE(J)=MIN(MS_VASE(I,J),
     &              MS_VASE(I,J)*DTS*TRANS_MASS(J))
            IF(NSICLA.GT.1) THEN
              TAUX(J)=TRANSFERT_MASSE_VASE(J)/MS_VASE(I,J)
              TRANSFERT_MASSE_SABLE(J)=TAUX(J)*MS_SABLE(I,J)
            ENDIF
          ELSE
            TRANSFERT_MASSE_VASE(J)=0.D0
            IF(NSICLA.GT.1) TRANSFERT_MASSE_SABLE(J)=0.D0
          ENDIF
!**************ARRET DE TASSEMENT SI LA VASE A REMPLI LES INTERSTICES
!**************   ENTRE LES GRAINS DE SABLE
          IF(NSICLA.GT.1.AND.EPAI_SABLE(J).GE.ES(I,J)) THEN
            TRANSFERT_MASSE_VASE(J) =0.D0
            TRANSFERT_MASSE_SABLE(J)=0.D0
          ENDIF
        ENDDO
!
        DO J=1,NCOUCH_TASS
          IF(J.EQ.NCOUCH_TASS) THEN
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            +TRANSFERT_MASSE_VASE(J-1))
             IF(NSICLA.GT.1) THEN
                MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &                                  +TRANSFERT_MASSE_SABLE(J-1))
             ENDIF
          ELSEIF(J.EQ.1) THEN
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            -TRANSFERT_MASSE_VASE(J))
            IF(NSICLA.GT.1) THEN
              MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &            -TRANSFERT_MASSE_SABLE(J))
            ENDIF
          ELSE
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            +TRANSFERT_MASSE_VASE(J-1)-TRANSFERT_MASSE_VASE(J))
             IF(NSICLA.GT.1) THEN
               MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &         +TRANSFERT_MASSE_SABLE(J-1)-TRANSFERT_MASSE_SABLE(J))
             ENDIF
          ENDIF
        ENDDO
!
        ELAY%R(I)=0.D0
!
        DO J=1,NCOUCH_TASS
          EPAI_VASE(J)=MS_VASE(I,J)/CONC_VASE(J)
          ES(I,J) = EPAI_VASE (J)
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J)=MS_SABLE(I,J)/XMVS
            ES(I,J)=EPAI_VASE(J)+EPAI_SABLE(J)
          ENDIF
          ELAY%R(I)=ELAY%R(I) + ES(I,J)
        ENDDO
!
!       BED EVOLUTION DUE TO CONSOLIDATION
!
        DZF_TASS%R(I)=ELAY%R(I)-T2%R(I)
!
! NOTE JMH : I HAS UNDERSTOOD THAT CLASS 1 = MUD
!            AND FROM 2 ON: SAND; WHAT ARE WE DOING HERE ??
!
        IF(NSICLA.GT.1) THEN
          DO J=1,NCOUCH_TASS
           IF(ES(I,J).GE.1.D-6) THEN
             AVAIL(I,J,1)=MS_SABLE(I,J)/XMVS/ES(I,J)
             AVAIL(I,J,2)=MS_VASE(I,J)/CONC_VASE(J)/ES(I,J)
           ELSE
             AVAIL(I,J,1)=0.D0
             AVAIL(I,J,2)=0.D0
           ENDIF
          ENDDO
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END