C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE BIMODAL DIRECTIONAL SPREADING FUNCTION
!>                FOR A RANGE OF DIRECTIONS.
!>  @code
!>              2S
!>           COS  ((T-T0)/2.)    (MITSUYASU)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEUPI, DIREC, FRA, NPLAN, SPRED1, SPRED2, TETA1, TETA2, XLAMDA
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ARGMI1, ARGMI2, ARGUM, DELT1, DELT2, FRA1, FRA2, FTH, JP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DELFRA()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SPEINI()

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
!>      <td><center> 1.2                                       </center>
!> </td><td> 07/11/96
!> </td><td> M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>DIREC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIREC(
!></td><td>--></td><td>DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>FRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRA(
!></td><td><--</td><td>VALEURS DE LA FONCTION DE REPARTITION ANG.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>SPRED1
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 1 POUR FRA
!>    </td></tr>
!>          <tr><td>SPRED2
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 2 POUR FRA
!>    </td></tr>
!>          <tr><td>TETA1
!></td><td>--></td><td>DIRECTION PRINCIPALE 1 POUR FRA
!>    </td></tr>
!>          <tr><td>TETA2
!></td><td>--></td><td>DIRECTION PRINCIPALE 2 POUR FRA
!>    </td></tr>
!>          <tr><td>XLAMDA
!></td><td>--></td><td>FACTEUR DE PONDERATION POUR LA FRA
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FSPRD3
     &( FRA   , DIREC , NPLAN , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA,
     &  DEUPI )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEUPI          |-->| 2.PI
C| DIREC          |---| 
C| DIREC(         |-->| DIRECTIONS DE DISCRETISATION
C| FRA            |---| 
C| FRA(           |<--| VALEURS DE LA FONCTION DE REPARTITION ANG.
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
C| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
C| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
C| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
C| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPLAN
      DOUBLE PRECISION SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA, DEUPI
      DOUBLE PRECISION FRA(NPLAN)    , DIREC(NPLAN)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELT1 , DELT2 , FTH   , FRA1  , FRA2  , ARGUM
      DOUBLE PRECISION ARGMI1, ARGMI2
C
C.....EXTERNAL FUNCTIONS
C     """""""""""""""""
      DOUBLE PRECISION DELFRA
      EXTERNAL         DELFRA
C
C
      DELT1 = 0.5D0/DELFRA(SPRED1,DEUPI)
      DELT2 = 0.5D0/DELFRA(SPRED2,DEUPI)
      IF (SPRED1.GT.1.D-1) THEN
        ARGMI1=10.D0**(-4.D0/SPRED1)
      ELSE
        ARGMI1=0.D0
      ENDIF
      IF (SPRED2.GT.1.D-1) THEN
        ARGMI2=10.D0**(-4.D0/SPRED2)
      ELSE
        ARGMI2=0.D0
      ENDIF
C
      DO 110 JP=1,NPLAN
        FTH = DIREC(JP)
C
        ARGUM = ABS(COS(0.5D0*(FTH-TETA1)))
        IF (ARGUM.GT.ARGMI1) THEN
          FRA1=DELT1*ARGUM**(2.D0*SPRED1)
        ELSE
          FRA1=0.D0
        ENDIF
C
        ARGUM = ABS(COS(0.5D0*(FTH-TETA2)))
        IF (ARGUM.GT.ARGMI2) THEN
          FRA2=DELT2*ARGUM**(2.D0*SPRED2)
        ELSE
          FRA2=0.D0
        ENDIF
C
        FRA(JP)=XLAMDA*FRA1+(1.D0-XLAMDA)*FRA2
        IF (FRA(JP).LT.1.D-10) FRA(JP)=0.D0
  110 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C