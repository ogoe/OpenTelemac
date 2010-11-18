C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE BOUNDARY ELEMENT TYPE CORRESPONDING TO
!>                A GIVEN ELEMENT TYPE IN THE DOMAIN.
!><br>            WHEN THERE ARE SEVERAL TYPES (AS IS THE CASE FOR THE
!>                PRISMS FOR EXAMPLE) USES INDEX I TO DISTINGUISH THEM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I, IELM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH(), BEDLOAD_SOLVS_FE(), BILANT1(), BILAN_SISYPHE(), CVDFTR(), DEBIMP(), POINT_ADJ_T2D(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POINT_TOMAWAC(), PROPAG(), PROPAG_ADJ(), SUSPENSION_COMPUTATION(), WAVE_EQUATION()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 06/02/08
!> </td><td> J-M HERVOUET 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>I
!></td><td>--></td><td>CAS DE PLUSIEURS TYPES D'ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT SUR LE DOMAINE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        INTEGER FUNCTION IELBOR
     &( IELM , I )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I             |-->| CAS DE PLUSIEURS TYPES D'ELEMENTS DE BORD
C| IELM           |-->| TYPE D'ELEMENT SUR LE DOMAINE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER IELM,I
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.10.OR.IELM.EQ.20) THEN
        IELBOR = 0
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.21) THEN
        IELBOR = 1
      ELSEIF(IELM.EQ.13) THEN
        IELBOR = 2
      ELSEIF(IELM.EQ.30) THEN
        IELBOR = 10
      ELSEIF(IELM.EQ.31) THEN
        IELBOR = 81
      ELSEIF(IELM.EQ.51.AND.I.EQ.1) THEN
        IELBOR = 11
      ELSEIF(IELM.EQ.51.AND.I.EQ.2) THEN
        IELBOR = 61
      ELSEIF(IELM.EQ.50.AND.I.EQ.1) THEN
        IELBOR = 10
      ELSEIF(IELM.EQ.50.AND.I.EQ.2) THEN
        IELBOR = 60
      ELSEIF(IELM.EQ.40.AND.I.EQ.1) THEN
        IELBOR = 10
      ELSEIF(IELM.EQ.40.AND.I.EQ.2) THEN
        IELBOR = 70
      ELSEIF(IELM.EQ.41.AND.I.EQ.1) THEN
        IELBOR = 11
      ELSEIF(IELM.EQ.41.AND.I.EQ.2) THEN
        IELBOR = 71
      ELSE
        IF(LNG.EQ.1) WRITE(LU,100) IELM
        IF(LNG.EQ.2) WRITE(LU,101) IELM
100     FORMAT(1X,'IELBOR (BIEF) : ',1I6,' ELEMENT NON PREVU')
101     FORMAT(1X,'IELBOR (BIEF) : ',1I6,' ELEMENT NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C