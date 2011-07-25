!                    *****************
                     SUBROUTINE DEBIMP
!                    *****************
!
     &(Q,UBOR,VBOR,U,V,H,NUMLIQ,IFRLIQ,WORK1,WORK2,NPTFR,MASK,MESH,
     & KP1BOR,EQUA)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IMPOSES FLUX BOUNDARY CONDITIONS,
!+                WITH AN ASSUMPTION OF AFFINITY WITH THE
!+                VELOCITY PROFILES AT THE ENTRANCE.
!
!history  J-M HERVOUET (LNH)
!+        24/04/1997
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
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| IFRLIQ         |-->| RANK OF LIQUID BOUNDARY
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| MASK           |-->| BLOCK OF MASKS FOR BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| Q              |-->| PRESCRIBED VALUE OF DISCHARGE
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| H              |-->| WATER DEPTH
!| WORK1          |<->| WORK BIEF_OBJ STRUCTURE
!| WORK2          |<->| WORK BIEF_OBJ STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),KP1BOR(NPTFR,2)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(*),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: H,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,WORK2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IELM
!
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
!     COMPUTES FLUX
!=======================================================================
!
!  IN THE FOLLOWING LOOP ONE RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
!  TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
!  DEFINED AT NODES, ONE RISKS AN ERROR FOR THE SEGMENT FOLLOWING
!  THE LAST NODE ON THE BOUNDARY. IN FACT THIS SEGMENT WILL BE SOLID
!  AND WILL HAVE A MASK ALREADY SET TO ZERO.
!
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          WORK1%R(K)=MASK(K)
        ELSE
          WORK1%R(K)=0.D0
        ENDIF
      ENDDO
!
      ELSE
!
!     FINITE VOLUMES COUNT THE SOLID SEGMENTS CLOSE TO
!     THE LIQUID BOUNDARIES
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          WORK1%R(K)          =MASK(K)
!         RA ON 07/12/2010
!         WORK1%R(KP1BOR(K,1))=MASK(K)
!         WORK1%R(KP1BOR(K,2))=MASK(K)
        ELSE
          WORK1%R(K)=0.D0
        ENDIF
      ENDDO
!
      ENDIF
!
      IELM=11
      CALL VECTOR(WORK2,'=','FLUBDF          ',IELBOR(IELM,1),
     &            1.D0,H,H,H,U,V,V,MESH,.TRUE.,WORK1)
!     SIGN CONVENTION REVERSED BETWEEN USER AND CODE
!     FOR THE USER: POSITIVE DISCHARGE = ENTERING
!     FOR THE CODE: U.N < 0 = ENTERING
      Q1 = - BIEF_SUM(WORK2)
      IF(NCSIZE.GT.1) Q1 = P_DSUM(Q1)
!
      IF(ABS(Q1).LT.1.D-10) THEN
!
! ZERO FLUX: WARNING MESSAGE
!
        IF(ABS(Q).GT.1.D-10) THEN
          IF(LNG.EQ.1) WRITE(LU,30) IFRLIQ
          IF(LNG.EQ.2) WRITE(LU,31) IFRLIQ
30        FORMAT(1X,'DEBIMP : PROBLEME SUR LA FRONTIERE ',1I6,/,1X,
     &     '         DONNER UN PROFIL DE VITESSES        ',/,1X,
     &     '         DANS LE FICHIER DES CONDITIONS AUX LIMITES',/,1X,
     &     '         OU VERIFIER LES HAUTEURS D''EAU')
31        FORMAT(1X,'DEBIMP : PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '         GIVE A VELOCITY PROFILE  ',/,1X,
     &     '         IN THE BOUNDARY CONDITIONS FILE',/,1X,
     &     '         OR CHECK THE WATER DEPTHS')
          CALL PLANTE(1)
          STOP
        ELSE
          Q1 = 1.D0
        ENDIF
!
      ENDIF
!
!=======================================================================
!   COMPUTES UBOR AND VBOR
!=======================================================================
!
      DO 40 K=1,NPTFR
!
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          UBOR(K) = UBOR(K) * Q / Q1
          VBOR(K) = VBOR(K) * Q / Q1
        ENDIF
!
40    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
