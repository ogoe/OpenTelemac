C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ANGLES OF RESONANT VECTORS IN THE CASE
!>                OF THE STANDARD INTERACTION CONFIGURATION - DIA METHOD
!>                PROPOSED BY HASSELMANN AND HASSELMANN (1985).<br>
!><br>            PROCEDURE SPECIFIC TO THE CASE WHERE THE DIRECTIONS
!>                ARE EVENLY DISTRIBUTED OVER [0;2.PI].

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THE ANGLES DTPLUS AND DTMOIN ARE IN DEGREES AND ARE BOTH
!>          POSITIVE BY CONVENTION.

!>  @reference    HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!>                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!>                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!>                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!>                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DTMOIN, DTPLUS, XLAMD
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, CNVD
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRENL1()

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
!> </td><td> 26/06/96
!> </td><td> M. BENOIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DTMOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTPLUS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XLAMD
!></td><td>--></td><td>COEFFICIENT LAMBDA DE LA CONFIGUARTION STD
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ANGLES
     &( XLAMD , DTPLUS, DTMOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DTMOIN         |---| 
C| DTPLUS         |---| 
C| XLAMD          |-->| COEFFICIENT LAMBDA DE LA CONFIGUARTION STD
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C.....VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      DOUBLE PRECISION XLAMD , DTPLUS, DTMOIN
C
C.....LOCAL VARIABLES
C     """"""""""""""""""
      DOUBLE PRECISION CNVD  , AUX
C
C
C.....1. CHECKS THAT LAMBDA RANGES BETWEEN 0 AND 0.5.
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      IF ((XLAMD.LT.0.D0).OR.(XLAMD.GT.0.5D0)) THEN
        IF(LNG.EQ.1) THEN
           WRITE(LU,1001) XLAMD
        ELSE
           WRITE(LU,1002) XLAMD
        ENDIF
        STOP
      ENDIF
C
C.....2. COMPUTES TETA_PLUS (DTPLUS) AND TETA_MOINS (DTMOIN).
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      CNVD=180.D0/3.141592654D0
      AUX=2.D0*XLAMD*(1.D0+XLAMD*XLAMD)
      DTPLUS=ACOS( (1.D0+AUX)/(1.D0+XLAMD)**2 )*CNVD
      DTMOIN=ACOS( (1.D0-AUX)/(1.D0-XLAMD)**2 )*CNVD
C
C
 1001 FORMAT('/!/-----------------------------------------------/!/'/
     &       '/!/  ARRET DU PROGRAMME DANS SUBROUTINE ANGLES    /!/'/
     &       '/!/-----------------------------------------------/!/'/
     &       '/!/  LA VALEUR CHOISIE POUR LE PARAMETRE LAMBDA   /!/'/
     &       '/!/  INTERVENANT DANS LA METHODE DISCRETE INTER-  /!/'/
     &       '/!/  ACTION APPROXIMATION DOIT ETRE INCLUSE DANS  /!/'/
     &       '/!/  L INTERVALLE [ 0. ; 0.5 ].                   /!/'/
     &       '/!/  OR LA VALEUR UTILISEE EST : ', G10.4,'       /!/'/
     &       '/!/-----------------------------------------------/!/')
C
 1002 FORMAT('/!/-----------------------------------------------/!/'/
     &       '/!/        PROGRAM STOP IN SUBROUTINE ANGLES      /!/'/
     &       '/!/-----------------------------------------------/!/'/
     &       '/!/       THE VALUE OF THE LAMBDA PARAMETER       /!/'/
     &       '/!/             USED IN THE DIA METHOD            /!/'/
     &       '/!/  MUST BE INCLUDED IN THE INTERVAL [ 0. ; 0.5 ]/!/'/
     &       '/!/  THE VALUE HERE IMPOSED IS : ', G10.4,'       /!/'/
     &       '/!/-----------------------------------------------/!/')
C
      RETURN
      END
C
C#######################################################################
C