C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TREATS THE DIRICHLET POINTS FOR THE FOLLOWING
!>                SYSTEM (BLOCK OF 9 MATRICES):
!>  @code
!>         (     A11          A12         A13  )  ( X1 )   ( SM1 )
!>         (                                   )  (    )   (     )
!>         (    T                              )  (    )   (     )
!>         (     A21          A22         A23  )  ( X2 ) = ( SM2 )
!>         (                                   )  (    )   (     )
!>         (    T            T                 )  (    )   (     )
!>         (     A31          A32         A33  )  ( X3 )   ( SM3 )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  TRANSPOSING A21 A31 AND A32 MAKES IT POSSIBLE TO USE ONLY
!>         ONE CALL FOR A12 AND A21, A31 AND A13, A32 AND A23 WHEN
!>         THE BLOCK IS SYMMETRICAL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, KDIR, LIDIR1, LIDIR2, LIDIR3, MASKPT, MESH, MSK, SM1, SM2, SM3, T1, T2, T3, T4, T5, T6, X1, X2, X3, XBOR1, XBOR2, XBOR3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, STODIA, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIRI09
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DIRAUX(), MATVEC(), OM(), OS(), OSDBIF(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIRICH()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 30/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,A12 ETC
!></td><td><-></td><td>MATRICES
!>    </td></tr>
!>          <tr><td>A12
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A31
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A32
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE DIRICHLET
!>    </td></tr>
!>          <tr><td>LIDIR1,2,3
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES POUR X1,2,3
!>    </td></tr>
!>          <tr><td>LIDIR2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIDIR3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>S1,2,3
!></td><td>--></td><td>SIGNES DEVANT LES SECONDS MEMBRES.
!>    </td></tr>
!>          <tr><td>SM1,SM2,SM3
!></td><td>--></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>T1,
!></td><td>--></td><td>TABLEAUX DE TRAVAIL DU SYSTEME
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X1 ,X2 ,X3
!></td><td><--</td><td>VALEURS A L'ETAPE N+1 (INITIALISATION)
!>    </td></tr>
!>          <tr><td>XBOR1,2,3
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR X1,2,3.
!>    </td></tr>
!>          <tr><td>XBOR2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XBOR3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRI09
     &(X1,X2,X3,
     & A11,A12,A13,A21,A22,A23,A31,A32,A33,
     & SM1,SM2,SM3,T1,T2,T3,T4,T5,T6,
     & XBOR1,XBOR2,XBOR3,LIDIR1,LIDIR2,LIDIR3,
     & MESH,KDIR,MSK,MASKPT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12 ETC    |<->| MATRICES
C| A12            |---| 
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
C| LIDIR1,2,3     |-->| TYPES DE CONDITIONS AUX LIMITES POUR X1,2,3
C| LIDIR2         |---| 
C| LIDIR3         |---| 
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| S1,2,3         |-->| SIGNES DEVANT LES SECONDS MEMBRES.
C| SM1,SM2,SM3    |-->| SECONDS MEMBRES DU SYSTEME.
C| T1,            |-->| TABLEAUX DE TRAVAIL DU SYSTEME
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| X1 ,X2 ,X3     |<--| VALEURS A L'ETAPE N+1 (INITIALISATION)
C| XBOR1,2,3      |-->| CONDITIONS AUX LIMITES SUR X1,2,3.
C| XBOR2          |---| 
C| XBOR3          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DIRI09 => DIRI09
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,X3,SM1,SM2,SM3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A13,A21,A22
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A31,A32,A33
      TYPE(BIEF_OBJ), INTENT(IN)    :: XBOR1,XBOR2,XBOR3,MASKPT
      INTEGER, INTENT(IN)           :: LIDIR1(*),LIDIR2(*),LIDIR3(*)
      INTEGER, INTENT(IN)           :: KDIR
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      LOGICAL, INTENT(IN)           :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C,Z(1)
C
      CHARACTER*1 STODIA
C
C-----------------------------------------------------------------------
C
C 1) BUILDS ARRAYS T1,T2,T3 CONTAINING:
C    THE X1, X2 AND X3 IMPOSED VALUES IF THE POINT IS OF TYPE DIRICHLET
C    0 OTHERWISE
C
C    X1,X2,X3 TAKE THEIR DIRICHLET VALUE
C
C=======================================================================
C
C   BOUNDARY CONDITION FOR X1 : "XBOR1" IMPOSED
C
      CALL CPSTVC(X1,T1)
      CALL OS  ( 'X=C     ' , T1 , T1 , T1 , 0.D0 )
      CALL OSDBIF ( 'X=Y     ',T1,XBOR1,LIDIR1,KDIR,MESH)
C
C-----------------------------------------------------------------------
C
C   BOUNDARY CONDITION FOR X2 : "XBOR2" IMPOSED
C
      CALL CPSTVC(X2,T2)
      CALL OS  ( 'X=C     ' , T2 , T2 , T2 , 0.D0 )
      CALL OSDBIF ( 'X=Y     ',T2,XBOR2,LIDIR2,KDIR,MESH)
C
C-----------------------------------------------------------------------
C
C   BOUNDARY CONDITION FOR X3 : "XBOR3" IMPOSED
C
      CALL CPSTVC(X3,T3)
      CALL OS  ( 'X=C     ' , T3 , T3 , T3 , 0.D0 )
      CALL OSDBIF ( 'X=Y     ',T3,XBOR3,LIDIR3,KDIR,MESH)
C
C=======================================================================
C
C   2) COMPUTES THE PRODUCT OF THE MATRIX FOR THE SYSTEM TO SOLVE
C      AND T1,T2,T3
C      THE RESULT IS DEDUCTED FROM THE SECOND MEMBERS
C
      CALL MATVEC('X=AY    ',T4,A11,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T4,A12,T2,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T4,A13,T3,C,MESH,LEGO=.TRUE. )
      CALL MATVEC('X=AY    ',T5,A21,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T5,A22,T2,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T5,A23,T3,C,MESH,LEGO=.TRUE. )
      CALL MATVEC('X=AY    ',T6,A31,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T6,A32,T2,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T6,A33,T3,C,MESH,LEGO=.TRUE. )
C
      CALL CPSTVC(X1,SM1)
      CALL CPSTVC(X2,SM2)
      CALL CPSTVC(X3,SM3)
      CALL OS( 'X=X-Y   ' , SM1 , T4 , T4 , C )
      CALL OS( 'X=X-Y   ' , SM2 , T5 , T5 , C )
      CALL OS( 'X=X-Y   ' , SM3 , T6 , T6 , C )
C
C=======================================================================
C
C  SECOND MEMBERS OF THE EQUATIONS FOR DIRICHLET POINTS
C  PREPARES THE LINEAR SYSTEM
C
      CALL DIRAUX(SM1,A11%D,XBOR1,T1,X1,LIDIR1,KDIR,MESH)
      CALL DIRAUX(SM2,A22%D,XBOR2,T2,X2,LIDIR2,KDIR,MESH)
      CALL DIRAUX(SM3,A33%D,XBOR3,T3,X3,LIDIR3,KDIR,MESH)
C
C CALLS OV RATHER THAN OS BECAUSE SM1 AND MASKPT DON'T ALWAYS
C HAVE THE SAME LENGTH
C
      IF(MSK) THEN
        CALL OV( 'X=XY    ',SM1%R,MASKPT%R,Z,C,SM1%DIM1)
        CALL OV( 'X=XY    ', X1%R,MASKPT%R,Z,C,X1%DIM1)
        CALL OV( 'X=XY    ', T1%R,MASKPT%R,Z,C,T1%DIM1)
        CALL OV( 'X=XY    ',SM2%R,MASKPT%R,Z,C,SM2%DIM1)
        CALL OV( 'X=XY    ', X2%R,MASKPT%R,Z,C,X2%DIM1)
        CALL OV( 'X=XY    ', T2%R,MASKPT%R,Z,C,T2%DIM1)
        CALL OV( 'X=XY    ',SM3%R,MASKPT%R,Z,C,SM3%DIM1)
        CALL OV( 'X=XY    ', X3%R,MASKPT%R,Z,C,X3%DIM1)
        CALL OV( 'X=XY    ', T3%R,MASKPT%R,Z,C,T3%DIM1)
      ENDIF
C
C=======================================================================
C
C   ERASES THE LINES AND COLUMNS FOR DIRICHLET POINTS
C
C   IT'S EQUIVALENT TO A DIAGONAL PRECONDITIONING WITH ARRAYS
C   T1,T2,T3
C
C   DOES NOT ALTER A11,A22,A33 DIAGONALS
C   BY GIVING THEM A DUMMY TYPE : '0'
C
C
C=======================================================================
C A11 PRECONDITIONING :
C=======================================================================
C
      STODIA = A11%TYPDIA
      A11%TYPDIA='0'
      CALL OM( 'M=DMD   ' , A11,A11 ,T1,C,MESH)
      A11%TYPDIA=STODIA
C
C=======================================================================
C A12 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A12,A12 ,T1,C,MESH)
      CALL OM( 'M=MD    ' , A12,A12 ,T2,C,MESH)
C
C=======================================================================
C A13 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A13,A13 ,T1,C,MESH)
      CALL OM( 'M=MD    ' , A13,A13 ,T3,C,MESH)
C
C=======================================================================
C A21 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A21,A21 ,T2,C,MESH)
      CALL OM( 'M=MD    ' , A21,A21 ,T1,C,MESH)
C
C=======================================================================
C A22 PRECONDITIONING :
C=======================================================================
C
      STODIA = A22%TYPDIA
      A22%TYPDIA='0'
      CALL OM( 'M=DMD   ' , A22,A22 ,T2,C,MESH)
      A22%TYPDIA=STODIA
C
C=======================================================================
C A23 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A23,A23 ,T2,C,MESH)
      CALL OM( 'M=MD    ' , A23,A23 ,T3,C,MESH)
C
C=======================================================================
C A31 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A31,A31 ,T3,C,MESH)
      CALL OM( 'M=MD    ' , A31,A31 ,T1,C,MESH)
C
C=======================================================================
C A32 PRECONDITIONING :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A32,A32 ,T3,C,MESH)
      CALL OM( 'M=MD    ' , A32,A32 ,T2,C,MESH)
C
C=======================================================================
C A33 PRECONDITIONING :
C=======================================================================
C
      STODIA = A33%TYPDIA
      A33%TYPDIA='0'
      CALL OM( 'M=DMD   ' , A33,A33 ,T3,C,MESH)
      A33%TYPDIA=STODIA
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C