C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MATRICES SOLVING HELMHOLTZ EQUATIONS
!>               (STEPS 1 AND 3 OF BOUSSINESQ ALGORITHM).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     A11 IS THE MATRIX WHICH MULTIPLIES U IN THE EQUATION FOR U
!>  @note     A12 IS THE MATRIX WHICH MULTIPLIES V IN THE EQUATION FOR U
!>  @note     A21 IS THE MATRIX WHICH MULTIPLIES U IN THE EQUATION FOR V
!>  @note     A22 IS THE MATRIX WHICH MULTIPLIES V IN THE EQUATION FOR V
!>  @note     SMU IS THE SECOND MEMBER IN THE EQUATION FOR U
!>  @note     SMV IS THE SECOND MEMBER IN THE EQUATION FOR V

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A21, A22, DT, H0, M1, MASKEL, MESH, MSK, S, SMU, SMV, UN, VN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, FORMUL, IELMH, IELMU, SL11, SURDT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MATRIX(), MATVEC(), OM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PROPAG()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A12
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AM1,2,3
!></td><td><-></td><td>MATRICES
!>    </td></tr>
!>          <tr><td>AT,DT
!></td><td>--></td><td>TEMPS, PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>H0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>M1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMU,SMV
!></td><td><--</td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>TM1
!></td><td><-></td><td>MATRICE
!>    </td></tr>
!>          <tr><td>U ,V ,H
!></td><td><--</td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>          <tr><td>UN,VN,HN
!></td><td>--></td><td>VALEURS A L' ETAPE N.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FONT AU NOEUD DE MAILLAGE .
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ROTNE0
     &(MESH,M1,A11,A12,A21,A22,SMU,SMV,UN,VN,H0,MSK,MASKEL,S,DT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11            |---| 
C| A12            |---| 
C| A21            |---| 
C| A22            |---| 
C| AM1,2,3        |<->| MATRICES
C| AT,DT          |-->| TEMPS, PAS DE TEMPS
C| H0             |---| 
C| M1             |---| 
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| S             |---| 
C| SMU,SMV        |<--| SECONDS MEMBRES DU SYSTEME.
C| TM1            |<->| MATRICE
C| U ,V ,H        |<--| VALEURS A L' ETAPE N+1.
C| UN,VN,HN       |-->| VALEURS A L' ETAPE N.
C| ZF             |-->| COTE DU FONT AU NOEUD DE MAILLAGE .
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(IN)   :: DT
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKEL,H0,S,UN,VN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: SMU,SMV
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: A11,A12,A21,A22,M1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMU,IELMH
C
      DOUBLE PRECISION SL11,C,SURDT
C
      CHARACTER*16 FORMUL
C
C-----------------------------------------------------------------------
C
      IELMH=H0%ELM
      IELMU=UN%ELM
C
C------------------------------------------------------------------
C
      SURDT = 1.D0 / DT
C
C     MATRIX FOR U IN THE EQUATION FOR U (INITIALLY STORED IN M1)
C
      FORMUL='FFBT        0XX0'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        XX00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     FORMUL='FFBT        0X0X'
      FORMUL='FFBT        0XX0'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        00XX'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     ADDITION TO A11 MATRIX
C
      IF(A11%TYPEXT.EQ.'S') CALL OM( 'M=X(M)  ' ,A11,A11,S,C,MESH)
      CALL OM( 'M=M+N   ' , A11 , M1 , S , C , MESH )
C
C     SECOND MEMBER SMU
C
      CALL MATVEC( 'X=X+AY  ',SMU,M1,UN,C,MESH)
C
C------------------------------------------------------------------
C
C     MATRIX FOR V IN THE EQUATION FOR U (INITIALLY STORED IN M1)
C
      FORMUL='FFBT        0Y0X'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        XY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        00XY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        0X0Y'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      CALL OM( 'M=N     ' , A12 , M1 , S , C , MESH )
C
C     SECOND MEMBER SMU
C
      CALL MATVEC( 'X=X+AY  ',SMU,M1,VN,C,MESH)
C
C------------------------------------------------------------------
C
C     MATRIX FOR V IN THE EQUATION FOR V (INITIALLY STORED IN M1)
C
      FORMUL='FFBT        0YY0'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        YY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        0YY0'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        00YY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     ADDITION TO A22 MATRIX
C
      IF(A22%TYPEXT.EQ.'S') CALL OM( 'M=X(M)  ' ,A22,A22,S,C,MESH)
      CALL OM( 'M=M+N   ' , A22 , M1 , S , C , MESH )
C
C     SECOND MEMBER SMV
C
      CALL MATVEC( 'X=X+AY  ',SMV,M1,VN,C,MESH)
C
C------------------------------------------------------------------
C
C     MATRIX FOR U IN THE EQUATION FOR V (INITIALLY STORED IN M1)
C
      FORMUL='FFBT        0X0Y'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        00XY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        XY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      FORMUL='FFBT        0Y0X'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
C
      CALL OM( 'M=N     ' , A21 , M1 , S , C , MESH )
C
C     SECOND MEMBER SMV
C
      CALL MATVEC( 'X=X+AY  ',SMV,M1,UN,C,MESH)
C
C------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C