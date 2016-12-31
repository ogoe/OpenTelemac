!                    *****************
                     SUBROUTINE CALCUE
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AN EFFECTIVE SPEED UE FOR THE ESTIMATION
!+                OF THE FRICTION DISSIPATION COEFFICIENT FW.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH) ; D. PAUGAM ( PLACEMENT)
!+        02/06/1999
!+        V5P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION BID
!
      INTRINSIC ABS,EXP
!
      CALL VECTOR(U0 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHIR , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK  , MASKEL )
!
      CALL VECTOR(V0 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHIR , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
!
!      THE OLD VARIABLE U1 IS STORED IN T1
!
      CALL VECTOR(T1, '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
!
!      THE OLD VARIABLE V1 IS STORED IN T2
!
      CALL VECTOR(T2, '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
      CALL VECTOR(T4 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , C , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
      CALL OS( 'X=Y/Z   ' , U0 , U0 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , V0 , V0 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , T1 , T1 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , T2 , T2 , T4 , BID )
!
!--------------------------------------------------------------
!             COMPUTES UE
!--------------------------------------------------------------
!
      CALL OS( 'X=C     ' , T4 , SBID , SBID , 0.D0 )
      CALL OS( 'X=YZ    ' , T4 , U0 , U0 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , V0 , V0 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , T1 , T1 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , T2 , T2 , 0.D0 )
!
      CALL OS( 'X=CX    ' , T4 , SBID , SBID  , 0.5D0 )
      CALL OS( 'X=SQR(Y)' , T1 , T4   , SBID  , BID   )
      CALL OS( 'X=CY    ' , T4 , T1   , SBID  , 1.2D0 )
!
      RETURN
      END
