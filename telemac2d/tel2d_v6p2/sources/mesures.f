!                    ******************
                     SUBROUTINE MESURES
!                    ******************
!
     &(ITER,TT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS MEASURED H, U AND V AT TIME AT.
!+                GIVES THE CORRESPONDING WEIGHTS ALPHA1, ALPHA2 AND ALPHA3.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET (LNHE)
!+        17/08/2001
!+        V5P2
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
!| ITER           |-->| ITERATION WHERE TO LOOK FOR THE MEASUREMENTS
!| TT             |-->| CORRESPONDING TIME (TO CHECK)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITER
      DOUBLE PRECISION, INTENT(IN) :: TT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TPS,C
      LOGICAL OKH,OKU,OKV
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF(T2D_FILES(T2DREF)%NAME(1:1).NE.' ') THEN
!
!-----------------------------------------------------------------------
!
!       WHEN MEASUREMENTS ARE IN A SELAFIN FILE
!
        CALL FIND_IN_SEL(HD,TEXTE(4)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKH,RECORD=ITER,TIME=TPS)
        CALL FIND_IN_SEL(UD,TEXTE(1)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKU,RECORD=ITER,TIME=TPS)
        CALL FIND_IN_SEL(VD,TEXTE(2)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKV,RECORD=ITER,TIME=TPS)
!
        IF(.NOT.OKH.OR..NOT.OKU.OR..NOT.OKV) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MESURES : PROBLEME DE LECTURE DE HD, UD OU VD'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND HD, UD, OR VD'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ABS(TT-TPS).GT.1.D-3) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MESURES : PROBLEME DE LECTURE DU TEMPS'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND TIME'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!       UD AND VD MAY BE QUASI-BUBBLE
!       (BUT ALPHA2 AND ALPHA3 WILL BE SET TO 0)
        IF(UD%ELM.EQ.12) THEN
          CALL CHGDIS(UD,11,12,MESH)
          CALL CHGDIS(VD,11,12,MESH)
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
!      CASE TO BE IMPLEMENTED BY USER HERE (OTHER FILE FORMAT, ETC.)
!
       IF(LNG.EQ.1) WRITE(LU,*) 'MESURES A PROGRAMMER DANS MESURES'
       IF(LNG.EQ.2) WRITE(LU,*) 'MEASUREMENTS TO IMPLEMENT IN MESURES'
       CALL PLANTE(1)
       STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WEIGHT FUNCTIONS FOR ALL THE TIMESTEPS
!
      CALL VECTOR(T1,'=','MASBAS          ',
     &            HD%ELM,1.D0,T3,T3,T3,T3,T3,
     &            T3,MESH,MSK,MASKEL)
      CALL OS( 'X=Y     ' , ALPHA1 , T1 , T1 , C )
!
!     CASE OF QUASI-BUBBLE ELEMENT FOR UD
      IF(HD%ELM.NE.UD%ELM) THEN
        CALL VECTOR(T1,'=','MASBAS          ',
     &              UD%ELM,1.D0,T3,T3,T3,T3,T3,
     &              T3,MESH,MSK,MASKEL)
      ENDIF
!
      CALL OS( 'X=Y     ' , ALPHA2 , T1 , T1 , C )
      CALL OS( 'X=Y     ' , ALPHA3 , T1 , T1 , C )
!
!     CANCELS WEIGHTS FOR QUASI-BUBBLE POINTS
!
      IF(UD%ELM.EQ.12) THEN
        DO I=NPOIN+1,NPOIN+NELEM
          ALPHA2%R(I)=0.D0
          ALPHA3%R(I)=0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
