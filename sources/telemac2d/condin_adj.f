!                    *********************
                     SUBROUTINE CONDIN_ADJ
!                    *********************
!
     &(ALIRE,NRES,RESFORMAT,TROUVE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS TO START
!+                AN ADJOINT COMPUTATION.
!
!history  J-M HERVOUET (LNHE)
!+        24/04/2009
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALIRE          |-->| ARRAY FOR ALL VARIABLES 0:VARIABLE DISCARDED
!|                |   |                         1:VARIABLE TO BE READ
!| NRES           |-->| LOGICAL UNIT OF RESULTS FILE
!| TROUVE         |<--| ARRAY FOR ALL VARIABLES 0:VARIABLE NOT FOUND
!|                |   |                         1:VARIABLE FOUND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: ALIRE(*),NRES
      CHARACTER(LEN=*), INTENT(IN) :: RESFORMAT
      INTEGER, INTENT(INOUT) :: TROUVE(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ITER, IERR, ITER1
      DOUBLE PRECISION HIST(1),AT1
!
!-----------------------------------------------------------------------
!
!     CONDIN FOR ADJOINT PROBLEM: T=T(N+1) : P=0,Q=0,R=0
!
      CALL OS( 'X=C     ',PP,PP,PP,0.D0)
      CALL OS( 'X=C     ',QQ,QQ,QQ,0.D0)
      CALL OS( 'X=C     ',RR,RR,RR,0.D0)
!
!     JUST IN CASE CV1,.. IS WRITTEN IN THE RESULT FILE
!
      CALL OS( 'X=C     ',CV1,CV1,CV1,0.D0)
      CALL OS( 'X=C     ',CV2,CV2,CV2,0.D0)
      CALL OS( 'X=C     ',CV3,CV3,CV3,0.D0)
!
!     READS THE LAST TIME IN THE TELEMAC RESULT FILE (NRES)
!     INITIALISES U,V AND H
!
      CALL GET_DATA_NTIMESTEP(RESFORMAT,NRES,ITER,IERR)
      CALL CHECK_CALL(IERR,'CONDIN_ADJ:GET_DATA_NTIMESTEP')
      ! Iteration are from 0 to ntimestep - 1
      ITER = ITER - 1
      CALL READ_DATASET(RESFORMAT,NRES,VARSOR,NPOIN,ITER,AT,TEXTE,
     &                  TROUVE,ALIRE,LISTIN,.TRUE.,MAXVAR)
!
!     GIVES MEASUREMENTS HD,UD AND VD AT THE LAST TIME STEP
!     (ITER AND AT GIVEN BY THE PREVIOUS CALL TO SUITE)
!
      CALL MESURES(ITER,AT)
!     INITIALISES HH, UU, VV
!
      CALL OS( 'X=Y     ' , HH   , H , H , 0.D0 )
      CALL OS( 'X=Y     ' , UU   , U , U , 0.D0 )
      CALL OS( 'X=Y     ' , VV   , V , V , 0.D0 )
      CALL OS( 'X=C     ' , HIT1 , HIT1 , HIT1 , 0.D0 )
      CALL OS( 'X=C     ' , UIT1 , UIT1 , UIT1 , 0.D0 )
      CALL OS( 'X=C     ' , VIT1 , VIT1 , VIT1 , 0.D0 )
!
!     READS TELEMAC2D RESULTS (RESULT FILE - UNIT NRES)
!     THIS IS TO HAVE UN AT THE LAST TIME STEP INTO U.
!
!     BEWARE : ASSUMES THAT NVARRES HAS ALREADY BEEN COMPUTED
!
      ITER1 = ITER - 1
      CALL READ_DATASET(RESFORMAT,NRES,VARSOR,NPOIN,ITER1,AT1,TEXTE,
     &                  TROUVE,ALIRE,LISTIN,.FALSE.,MAXVAR)
!
!-----------------------------------------------------------------------
!
      AT = AT + DT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
