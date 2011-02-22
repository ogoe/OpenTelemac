C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES THE STRUCTURES FOR THE ADJOINT SYSTEM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::A23 A23@endlink, 
!> @link DECLARATIONS_TELEMAC2D::A32 A32@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ADJDIR ADJDIR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AK AK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA1 ALPHA1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA2 ALPHA2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA3 ALPHA3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV1 CV1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV2 CV2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV3 CV3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DESC DESC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EP EP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FU FU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FV FV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRADJ GRADJ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRADJN GRADJN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HD HD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HH HH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HIT1 HIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELM0 IELM0@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELM1 IELM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELME IELME@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMH IELMH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMK IELMK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMT IELMT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMU IELMU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MATADJ MATADJ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PATMOS PATMOS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PBOR PBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PP PP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QBOR QBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QQ QQ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RBOR RBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RR RR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SETSTR SETSTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SETSTR2 SETSTR2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T T@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T3 T3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T4 T4@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T5 T5@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T6 T6@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TAM1 TAM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TAM2 TAM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TAM3 TAM3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TBM1 TBM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TBM2 TBM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TCM1 TCM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TCM2 TCM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UD UD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UIT1 UIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UNKADJ UNKADJ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UU UU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARSORA VARSORA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VD VD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VISC VISC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VIT1 VIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VV VV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::WINDX WINDX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::WINDY WINDY@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CFG, CFGBOR, IELB1, IELBE, IELBH, IELBK, IELBT, IELBU, IELMX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), ALLBLO(), ALLMAT(), ALLVEC(), IELBOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D()

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
!> </td><td> 18/09/2000
!> </td><td> A LEOPARDI (UNINA)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 24/04/1997
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE POINT_ADJ_T2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELBU,IELBH,IELBT,IELBK,IELBE,IELB1,IELMX,CFG(2),CFGBOR(2)
C
C-----------------------------------------------------------------------
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,20)
         IF(LNG.EQ.2) WRITE(LU,21)
      ENDIF
20    FORMAT(1X,///,26X,'*************************************',/,
     &26X,              '* ALLOCATION DE LA MEMOIRE (ADJOINT)*',/,
     &26X,              '*************************************',/)
21    FORMAT(1X,///,26X,'*************************************',/,
     &26X,              '*    MEMORY ORGANIZATION  (ADJOINT) *',/,
     &26X,              '*************************************',/)
C
C-----------------------------------------------------------------------
C
C     TYPES OF DISCRETISATIONS
C
      IELM0 = 10*(IELMH/10)
      IELM1 = IELM0 + 1
C
      IELB1 = IELBOR(IELM1,1)
      IELBU = IELBOR(IELMU,1)
      IELBH = IELBOR(IELMH,1)
      IELBT = IELBOR(IELMT,1)
      IELBK = IELBOR(IELMK,1)
      IELBE = IELBOR(IELME,1)
C
      IELMX=MAX(IELMU,IELMH,IELMT,IELMK,IELME)
C
C TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
C
      CFG(1) = OPTASS
      CFG(2) = PRODUC
C     CFG FOR THE BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
C
C-----------------------------------------------------------------------
C
C                     ******************
C                     *  REAL ARRAYS   *
C                     ******************
C
C-----------------------------------------------------------------------
C
C  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
C
      CALL BIEF_ALLVEC(1,PP,'PP    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,QQ,'QQ    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,RR,'RR    ',IELMU,1,1,MESH)
C
      CALL BIEF_ALLVEC(1,UU  ,'UU    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VV  ,'VV    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HH  ,'HH    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UIT1,'UIT1  ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VIT1,'VIT1  ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HIT1,'HIT1  ',IELMH,1,1,MESH)
C
C  BLOCK OF THE UNKNOWNS IN PROPAG
C
      CALL ALLBLO(UNKADJ,'UNKADJ')
      CALL ADDBLO(UNKADJ, PP)
      CALL ADDBLO(UNKADJ, QQ)
      CALL ADDBLO(UNKADJ, RR)
C
C  MATRICES
C
      CALL BIEF_ALLMAT(TAM1,'TAM1  ',IELMH,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TAM2,'TAM2  ',IELMU,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TAM3,'TAM3  ',IELMU,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TBM1,'TBM1  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TBM2,'TBM2  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TCM1,'TCM1  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TCM2,'TCM2  ',IELMU,IELMH,CFG,'Q','Q',MESH)
C
C BLOCK OF THE MATRICES IN PROPAG
C
      CALL ALLBLO(MATADJ,'MATADJ')
      CALL ADDBLO(MATADJ,TAM1)
      CALL ADDBLO(MATADJ,TCM1)
      CALL ADDBLO(MATADJ,TCM2)
      CALL ADDBLO(MATADJ,TBM1)
      CALL ADDBLO(MATADJ,TAM2)
      CALL ADDBLO(MATADJ,A23)
      CALL ADDBLO(MATADJ,TBM2)
      CALL ADDBLO(MATADJ,A32)
      CALL ADDBLO(MATADJ,TAM3)
C
C DIRICHLET POINTS:
C
      CALL BIEF_ALLVEC(1,QBOR ,'QBOR  ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,RBOR ,'RBOR  ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,PBOR ,'PBOR  ',IELBH,1,1,MESH)
C
      CALL ALLBLO(ADJDIR,'ADJDIR')
      CALL ADDBLO(ADJDIR,PBOR)
      CALL ADDBLO(ADJDIR,QBOR)
      CALL ADDBLO(ADJDIR,RBOR)
C
C ARRAYS FOR THE DESCENT METHOD (UP TO NPOIN PARAMETERS)
C
      CALL BIEF_ALLVEC(1,GRADJ  ,'GRADJ ',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,GRADJN ,'GRADJN',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,DESC   ,'DESC  ',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,SETSTR ,'SETSTR',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,SETSTR2,'SETST2',IELM1,1,1,MESH)
C
C_______________________________________________________________________
C
C  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
C
C_______________________________________________________________________
C
C
      CALL BIEF_ALLVEC(1,ALPHA1  ,'ALPHA1',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,ALPHA2  ,'ALPHA2',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,ALPHA3  ,'ALPHA3',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HD      ,'HD    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UD      ,'UD    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VD      ,'VD    ',IELMU,1,1,MESH)
C
C  COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME TO ITS ARRAY
C
C
      CALL ALLBLO(VARSORA ,'VARSORA')
C 01
      CALL ADDBLO(VARSORA,U)
C 02
      CALL ADDBLO(VARSORA,V)
C 03
      CALL ADDBLO(VARSORA,FU)
C 04
      CALL ADDBLO(VARSORA,H)
C 05
      CALL ADDBLO(VARSORA,FV)
C 06
      CALL ADDBLO(VARSORA,ZF)
C 07
      CALL ADDBLO(VARSORA,T2)
C 08
      CALL ADDBLO(VARSORA,T3)
C 09
      CALL ADDBLO(VARSORA,T)
C 10
      CALL ADDBLO(VARSORA,AK)
C 11
      CALL ADDBLO(VARSORA,EP)
C 12
      CALL ADDBLO(VARSORA,VISC)
C 13
      CALL ADDBLO(VARSORA,T4)
C 14
      CALL ADDBLO(VARSORA,T5)
C 15
      CALL ADDBLO(VARSORA,T6)
C 16
      CALL ADDBLO(VARSORA,WINDX)
C 17
      CALL ADDBLO(VARSORA,WINDY)
C 18
      CALL ADDBLO(VARSORA,PATMOS)
C 19
      CALL ADDBLO(VARSORA,CHESTR)
C 20
      CALL ADDBLO(VARSORA,CV1)
C 21
      CALL ADDBLO(VARSORA,CV2)
C 22
      CALL ADDBLO(VARSORA,CV3)
C 23
      CALL ADDBLO(VARSORA,PP)
C 24
      CALL ADDBLO(VARSORA,QQ)
C 25
      CALL ADDBLO(VARSORA,RR)
C 26
      CALL ADDBLO(VARSORA,PRIVE%ADR(4)%P)
C
C***********************************************************************
C
C WRITES OUT TO LISTING :
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,22)
         IF(LNG.EQ.2) WRITE(LU,23)
      ENDIF
22    FORMAT(1X,///,21X,'***************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  *',/,
     &21X,              '***************************************',/)
23    FORMAT(1X,///,21X,'************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION ADJ*',/,
     &21X,              '************************************',/)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
