!                    *****************
                     SUBROUTINE FRICTI
!                    *****************
!
     &(FU_IMP,FV_IMP,FUDRAG,FVDRAG,UN,VN,HN,CF,MESH,T1,T2,VERTIC,
     & UNSV2D,MSK,MASKEL,HFROT)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FRICTION TERMS IN THEIR IMPLICIT FORM.
!+                THEY WILL BE ADDED TO MATRICES AM2 AND AM3 IN PROCU3.
!
!history
!+        31/07/2009
!+
!+   POINTER HHN TO AVOID A COPY
!
!history  J-M HERVOUET (LNHE)
!+        03/08/2009
!+        V6P0
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
!| CF             |-->| COEFFICIENT DE FROTTEMENT VARIABLE EN ESPACE
!| FUDRAG         |---|
!| FU_IMP         |---|
!| FVDRAG         |---|
!| FV_IMP         |---|
!| HFROT          |---|
!| HN             |-->| HAUTEURS D'EAU A TN
!| MASKEL         |---|
!| MESH           |---|
!| MSK            |---|
!| T1             |---|
!| T2             |---|
!| UNSV2D         |---|
!| VERTIC         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FRICTI => FRICTI
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)                 :: VERTIC,MSK
      INTEGER, INTENT(IN)                 :: HFROT
      TYPE(BIEF_OBJ),  INTENT(IN)         :: UN,VN,CF,UNSV2D,MASKEL
      TYPE(BIEF_OBJ),  INTENT(IN), TARGET :: HN
      TYPE(BIEF_OBJ),  INTENT(INOUT)      :: FU_IMP,FV_IMP,FUDRAG,FVDRAG
      TYPE(BIEF_OBJ),  INTENT(INOUT), TARGET :: T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)      :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,IELMU,IELMH,IELMS
!
      DOUBLE PRECISION UNORM,H,C,HO
!
      INTRINSIC SQRT,MAX
!
      TYPE(BIEF_OBJ), POINTER :: HHN
!
!-----------------------------------------------------------------------
!
      IELMU=UN%ELM
      IELMH=HN%ELM
      IELMS=CF%ELM
!
!     SETUP THE WATE DEPTH WITH THE SAME DISCRETIZATION
!
      IF(IELMH.NE.IELMU) THEN
        CALL OS( 'X=Y     ' , X=T1 , Y=HN )
        CALL CHGDIS( T1 , IELMH , IELMU , MESH )
        HHN=>T1
      ELSE
        HHN=>HN
      ENDIF
!
      IF(IELMS.NE.IELMU) THEN
        IF (LNG.EQ.1) WRITE(LU,200) IELMS,IELMU
        IF (LNG.EQ.2) WRITE(LU,201) IELMS,IELMU
200     FORMAT(1X,'FRICTI : DISCRETISATION DU FROTTEMENT : ',1I6,/,
     &         1X,'DIFFERENTE DE CELLE DE U : ',1I6)
201     FORMAT(1X,'FRICTI: DISCRETIZATION OF FRICTION:',1I6,/,
     &         1X,'DIFFERENT FROM U: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FU_IMP AND FV_IMP ARE WORKING ARRAYS
!
      CALL CPSTVC(UN,FU_IMP)
      CALL CPSTVC(VN,FV_IMP)
!
!-----------------------------------------------------------------------
!
!     BEWARE: HO HIDDEN PARAMETER
!
      HO = 3.D-2
!
      IF(HFROT.EQ.1) THEN
        DO N=1,UN%DIM1
          UNORM = SQRT(UN%R(N)**2+VN%R(N)**2)
          H = MAX(HHN%R(N),1.D-9)
!         MODIFICATION BY JMH ON 06/08/04
!         FOLLOWING LINE TO KEEP A FRICTION ON TIDAL FLATS, IF UNORM=0
!         IDEA : IF TOO SMALL, UNORM PROGRESSIVELY REPLACED BY SQRT(G*H)
!         WHEN H TENDS TO 0. LITTLE CHANGE, BUT BIG EFFECT ON UNORM/H
          IF(H.LT.HO) UNORM=MAX(UNORM,SQRT(9.81*(HO-H)*H/HO))
          FU_IMP%R(N) = - 0.5D0 * CF%R(N) * UNORM / H
          FV_IMP%R(N) = FU_IMP%R(N)
        ENDDO
      ELSEIF(HFROT.EQ.2) THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELMH,
     &              1.D0,HHN,HHN,HHN,HHN,HHN,HHN,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
        DO N=1,UN%DIM1
          UNORM = SQRT(UN%R(N)**2+VN%R(N)**2)
!         SMOOTHED OR AVERAGE DEPTH
          H = MAX(T2%R(N)*UNSV2D%R(N),1.D-9)
          IF(H.LT.HO) UNORM=MAX(UNORM,SQRT(9.81*(HO-H)*H/HO))
          FU_IMP%R(N) = - 0.5D0 * CF%R(N) * UNORM / H
          FV_IMP%R(N) = FU_IMP%R(N)
        ENDDO
      ELSE
        WRITE(LU,*) 'FRICTI : PARAMETRE HFROT INCONNU : ',HFROT
        WRITE(LU,*) 'FRICTI: UNKNOWN PARAMETER HFROT:',HFROT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(VERTIC) THEN
        CALL CPSTVC(UN,FUDRAG)
        CALL CPSTVC(VN,FVDRAG)
        CALL DRAGFO(FUDRAG,FVDRAG)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END