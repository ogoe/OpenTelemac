!                    *****************
                     SUBROUTINE DEBIMP
!                    *****************
!
     &(Q,UBOR,VBOR,U,V,H,NUMLIQ,IFRLIQ,WORK1,WORK2,NPTFR,MASK,MESH,
     & KP1BOR)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   19/03/2014
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
!history  J-M HERVOUET (LNH)
!+        11/10/2011
!+        V6P2
!+   Velocity updated with UBOR and VBOR eventually computed
!
!history  R. ATA (EDF R&D, LNHE)
!+        15/09/2013
!+        V6P3
!+   fixing bug with FV (imposed discharge)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      USE DECLARATIONS_SPECIAL
!##> JR @ RWTH: ALLOW COMPILERS TO CHECK PARALLEL INTERFACE
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!##< JR @ RWTH
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),KP1BOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(*),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: H
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,WORK2,U,V
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IELM,N,IELEB
!
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
!##> JR @ RWTH: INTERFACE CHECKED SO NO NEED FOR EXTERNALS
!      DOUBLE PRECISION P_DSUM
!      EXTERNAL         P_DSUM
!##< JR @ RWTH
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
      CALL OS( 'X=0     ' , X=WORK1 )
!
      DO IELEB=1,MESH%NELEB
        K=MESH%IKLBOR%I(IELEB)
        IF(NUMLIQ(K).EQ.IFRLIQ) WORK1%R(IELEB)=MASK(IELEB)
      ENDDO
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
!       ZERO FLUX: WARNING MESSAGE
!
        IF(ABS(Q).GT.1.D-10) THEN
          IF(LNG.EQ.1) WRITE(LU,30) IFRLIQ
          IF(LNG.EQ.2) WRITE(LU,31) IFRLIQ
30        FORMAT(1X,'DEBIMP : PROBLEME SUR LA FRONTIERE ',1I6     ,/,1X,
     &              '         DONNER UN PROFIL DE VITESSES       ',/,1X,
     &              '         DANS LE :                          ',/,1X,
     &              '         FICHIER DES CONDITIONS AUX LIMITES ',/,1X,
     &              '         OU VERIFIER LES HAUTEURS D''EAU.   ',/,1X,
     &              '         AUTRE CAUSE POSSIBLE :             ',/,1X,
     &              '         ENTREE TORRENTIELLE A HAUTEUR LIBRE',/,1X,
     &              '         METTRE UNE HAUTEUR NON NULLE       ',/,1X,
     &              '         DANS LES CONDITIONS INITIALES      ',/,1X,
     &              '         OU IMPOSER LA HAUTEUR D''EAU EN ENTREE.')
31        FORMAT(1X,'DEBIMP: PROBLEM ON BOUNDARY NUMBER ',1I6     ,/,1X,
     &              '        GIVE A VELOCITY PROFILE             ',/,1X,
     &              '        IN THE BOUNDARY CONDITIONS FILE     ',/,1X,
     &              '        OR CHECK THE WATER DEPTHS.          ',/,1X,
     &              '        OTHER POSSIBLE CAUSE:               ',/,1X,
     &              '        SUPERCRITICAL ENTRY WITH FREE DEPTH ',/,1X,
     &              '        IN THIS CASE GIVE A POSITIVE DEPTH  ',/,1X,
     &              '        IN THE INITIAL CONDITIONS           ',/,1X,
     &              '        OR PRESCRIBE THE DEPTH AT THE ENTRANCE.')
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
      DO K=1,NPTFR
!
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          N=MESH%NBOR%I(K)
          UBOR(K) = UBOR(K) * Q / Q1
          VBOR(K) = VBOR(K) * Q / Q1
!         WE DO NOT LET THE PREVIOUS UBOR WHICH IS ONLY A PROFILE
!         THIS HAS AN EFFECT AT LEAST IN PROPIN_TELEMAC2D
!         WHICH IS CALLED BEFORE THE TREATMENT OF DIRICHLET CONDITIONS
          U%R(N) = UBOR(K)
          V%R(N) = VBOR(K)
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
