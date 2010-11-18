C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION STEP
!>                - SOURCE TERMS OF THE K-EPSILON MODEL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  LIMKEP IS BUILT FROM LIUBOR (LIKBOR AND LIEBOR DO NOT EXIST)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KADH, KDIR, KENT, KENTU, KINC, KLOG, KNEU, KSORT, LIMKEP, LIUBOR, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>INDICATEUR DE POINT DIRICHLET.
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>INDICATEUR DE POINT DE DIRICHLET
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>INDICATEUR DE POINT D'ENTREE FLUIDE .
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>INDICATEUR DE VITESSES IMPOSEES.
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>--></td><td>INDICATEUR POUR L'ONDE INCIDENTE.
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE .
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>INDICATEUR DE POINT DE NEUMANN
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>INDICATEUR DE POINT DE SORTIE FLUIDE .
!>    </td></tr>
!>          <tr><td>LIMKEP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMKEP(
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES POUR EP
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR U.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>DIMENSION DES TABLEAUX .
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KEPSIN
     &(LIMKEP,LIUBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KADH           |-->| INDICATEUR DE POINT DIRICHLET.
C| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
C| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE .
C| KENTU          |-->| INDICATEUR DE VITESSES IMPOSEES.
C| KINC           |-->| INDICATEUR POUR L'ONDE INCIDENTE.
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE .
C| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
C| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE .
C| LIMKEP         |---| 
C| LIMKEP(        |<--| TYPES DE CONDITIONS AUX LIMITES POUR EP
C| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR U.
C| NPTFR          |-->| DIMENSION DES TABLEAUX .
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPTFR,KENT,KSORT,KADH,KLOG
      INTEGER, INTENT(IN)    :: KINC,KNEU,KDIR,KENTU
      INTEGER, INTENT(INOUT) :: LIMKEP(NPTFR,2)
      INTEGER, INTENT(IN)    :: LIUBOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
C-----------------------------------------------------------------------
C
C  BUILDS THE ARRAY LIMKEP
C
      DO 1 K=1,NPTFR
C
        IF(LIUBOR(K).EQ.KENT) THEN
C
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
C
        ELSEIF(LIUBOR(K).EQ.KENTU) THEN
C
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
C
        ELSEIF(LIUBOR(K).EQ.KADH) THEN
C
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
C
        ELSEIF(LIUBOR(K).EQ.KSORT) THEN
C
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
C
        ELSEIF(LIUBOR(K).EQ.KINC) THEN
C
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
C
        ELSEIF(LIUBOR(K).EQ.KLOG ) THEN
C
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
C
        ELSE
C
          IF(LNG.EQ.1) WRITE(LU,100) K,LIUBOR(K)
          IF(LNG.EQ.2) WRITE(LU,101) K,LIUBOR(K)
100       FORMAT(1X,'KEPSIN: K=',1I6,' LIUBOR=',1I6,' CAS NON PREVU')
101       FORMAT(1X,'KEPSIN: K=',1I6,' LIUBOR=',1I6,' UNKNOWN CASE')
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C