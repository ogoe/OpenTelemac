!                    *****************
                     SUBROUTINE DIRI01
!                    *****************
!
     &(F, S, SM ,FBOR,LIMDIR,WORK1,WORK2,MESH,KDIR,MSK,MASKPT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT POINTS OF TYPE DIRICHLET IN A SYSTEM
!+                OF LINEAR EQUATIONS WITH SYMMETRICAL MATRIX.
!+
!+            IN THE EQUATIONS FOR POINTS NOT OF TYPE DIRICHLET :
!+                DIRICHLET VALUES ARE REMOVED.
!+
!+            IN THE EQUATIONS FOR POINTS OF TYPE DIRICHLET :
!+                DEFINES AN EQUATION FIXING THE IMPOSED VALUE.
!
!warning  THIS SUBROUTINE IS NOT PROTECTED AGAINST DIAGONAL EQUAL
!+            TO 0 ON DIRICHLET POINTS; IT WILL THEN SET AN EQUATION
!+            0 X = 0 ON SUCH POINTS
!
!history  J-M HERVOUET (LNHE)
!+        07/08/2009
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
!| F              |<->| VALEURS A L'ETAPE N+1 ET INITIALISATION
!| FBOR           |-->| CONDITIONS AUX LIMITES DES POINTS DIRICHLET.
!| KDIR           |-->| CONVENTION POUR LES CONDITIONS DE DIRICHLET
!| LIMDIR         |-->| TYPES DE CONDITIONS AUX LIMITES .
!|                |   | SI LIMDIR(K) = KDIR LE KIEME POINT DE BORD
!|                |   | EST DE TYPE DIRICHLET.
!| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
!|                |   | =1. : NORMAL   =0. : POINT MASQUE.
!| MESH           |---| 
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| S              |<->| MATRICE DU SYSTEME
!| SM             |-->| SECOND MEMBRE DU SYSTEME.
!| WORK2          |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIRI01 => DIRI01
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,S,SM,WORK1,WORK2
      TYPE(BIEF_OBJ), INTENT(IN)    :: FBOR,MASKPT
      INTEGER, INTENT(IN) :: LIMDIR(*), KDIR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN) :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C,Z(1)
!
      INTEGER IELMSM,IELMFB
!
      CHARACTER*1 OLDDIA
!
!----------------------------------------------------------------------
!
!  DEPLOYS THE MESH STRUCTURE
!
!----------------------------------------------------------------------
!
!  BUILDS AN ARRAY WITH 0S EVERYWHERE EXCEPT AT DIRICHLET POINTS
!  FOR WHICH THE VALUE IS TAKEN FROM FBOR
!  FBOR MUST BE 0 WHEN THE POINT IS NOT OF TYPE DIRICHLET
!
      CALL CPSTVC(SM,WORK1)
!
      IELMSM=SM%ELM
      IELMFB=FBOR%ELM
      IF(IELMSM.EQ.IELMFB) THEN
        CALL MATVEC( 'X=AY    ' ,WORK2,S,FBOR,C, MESH )
      ELSE
        CALL OS( 'X=0     ' , X=WORK1 )
        CALL OSDBIF( 'X=Y     ' ,WORK1,FBOR,LIMDIR,KDIR,MESH)
        CALL MATVEC( 'X=AY    ' ,WORK2,S,WORK1,C, MESH )
      ENDIF
!
!----------------------------------------------------------------------
!
!  THE PRODUCT S WORK1 IS DEDUCTED FROM THE SECOND MEMBER.
!  IT MEANS THAT THE VALUES AT DIRICHLET POINTS ARE NO LONGER
!  UNKNOWNS IN THE EQUATIONS FOR THE OTHER POINTS.
!
      CALL OS( 'X=X-Y   ' , X=SM , Y=WORK2 )
!
!----------------------------------------------------------------------
!
!  BUILDS AN ARRAY WITH 1S EVERYWHERE EXCEPT AT DIRICHLET POINTS
!  FOR WHICH IT'S 0
!
!  WHAT'S MORE, AN EQUATION OF THE FORM DS(N) * X = DS(N) * FBOR
!  (WILL GIVE X=FBOR) IS SET IN THE MATRIX FOR DIRICHLET POINTS;
!  AND F IS INITIALISED TO ITS KNOWN VALUE.
!  THIS ASSUMES THAT DS(N) IS NOT 0
!
      CALL DIRAUX(SM,S%D,FBOR,WORK2,F,LIMDIR,KDIR,MESH )
!
!  MASKING : FOR THE POINTS OF MASKED ELEMENTS THE EQUATION X=0
!            IS SET FOR THE DIAGONAL COEFFICIENT PRES
!
      IF(MSK) THEN
        CALL OV( 'X=XY    ', SM%R   ,MASKPT%R ,Z,C,   SM%DIM1)
        CALL OV( 'X=XY    ', F%R    ,MASKPT%R ,Z,C,    F%DIM1)
        CALL OV( 'X=XY    ', WORK2%R,MASKPT%R ,Z,C,WORK2%DIM1)
      ENDIF
!
!----------------------------------------------------------------------
!
!  WORK2 * S * WORK2 :
!  ERASES THE LINES AND COLUMNS IN S WHICH CORRESPOND TO DIRICHLET
!  POINTS
!  DOES NOT ALTER THE DIAGONAL BY DECLARING IT 0 HERE
!
      OLDDIA=S%TYPDIA
      S%TYPDIA='0'
      CALL OM( 'M=DMD   ' , S , S , WORK2 , C , MESH )
      S%TYPDIA=OLDDIA
!
!----------------------------------------------------------------------
!
      RETURN
      END