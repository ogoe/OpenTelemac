C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES A SYSTEM OF IMAX EQUATIONS WITH UNKNOWN E
!>                AT TIME N+1.
!><br>            METHOD KNOWN AS DOUBLE SWEEPING METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IMAX, IVIDE, NDEB, NPFMAX, TRA01
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS, K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TASSEM()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 18/07/06
!> </td><td> NOEMIE DURAND AND SEBASTIEN BOURBAN; C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IMAX
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE DU FOND
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><--</td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>                  (MAILLAGE SELON UNE VERTICALE)
!>    </td></tr>
!>          <tr><td>NDEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE BISSEL
     &(IVIDE,TRA01,NPFMAX,IMAX,NDEB)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IMAX           |-->| NOMBRE DE POINTS DU MAILLAGE DU FOND
C| IVIDE          |<--| INDICE DES VIDES AUX POINTS DU MAILLAGE
C|                |   | (MAILLAGE SELON UNE VERTICALE)
C| NDEB           |---| 
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX
C| TRA01          |-->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPFMAX, IMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
!
      INTEGER K,NDEB
      DOUBLE PRECISION EPS
!
!======================================================================
!
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
C  THE FLOATING POINT EXCEPTIONS ARE NOW CHECKED BEFORE BEING USED
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####
      EPS=1.D-8
      DO K=1+NDEB,IMAX-1
        IF( ABS(TRA01(K-1,4)).LT.EPS ) THEN
          WRITE(LU,*) 'FLOATING EXCEPTION IN BISSEL (CALLED BY TASSEM)'
          CALL PLANTE(1)
          STOP
        ENDIF
        TRA01(K,4) = TRA01(K,4)-(TRA01(K,3)*TRA01(K-1,5))/TRA01(K-1,4)
        TRA01(K,6) = TRA01(K,6)-(TRA01(K,3)*TRA01(K-1,6))/TRA01(K-1,4)
      ENDDO
!
      IF(ABS(TRA01(IMAX,3)*TRA01(IMAX-1,5)
     &         -TRA01(IMAX-1,4)*TRA01(IMAX,4)).LT.EPS ) THEN
        WRITE(LU,*) 'DIVISION BY ZERO IN BISSEL (CALLED BY TASSEM)'
        CALL PLANTE(1)
        STOP
      ENDIF
      IVIDE(IMAX)=
     &  (TRA01(IMAX,6)*TRA01(IMAX-1,4)-TRA01(IMAX,3)*TRA01(IMAX-1,6))/
     &  (TRA01(IMAX,4)*TRA01(IMAX-1,4)-TRA01(IMAX,3)*TRA01(IMAX-1,5))
!
C#####> SEB-CHANGES
C SLIGHT MISTAKE IN THE LOOP INDEXING
C       DO I=1,IMAX-NDEB+1
C          K=IMAX+1-I
C          IVIDE(K)=0.D0
C          IVIDE(K)=(TRA01(K,6)-TRA01(K,5)*IVIDE(K+1))/TRA01(K,4)
       DO K = IMAX-1,NDEB,-1
         IF( ABS(TRA01(K,4)).LT.EPS ) THEN
           WRITE(LU,*) 'DIVISION BY ZERO IN BISSEL (CALLED BY TASSEM)'
           CALL PLANTE(1)
           STOP
         ENDIF
         IVIDE(K) = ( TRA01(K,6) - TRA01(K,5)*IVIDE(K+1) )/TRA01(K,4)
       ENDDO
C#####
!
!-----------------------------------------------------------------------
!
      RETURN
      END

C
C#######################################################################
C