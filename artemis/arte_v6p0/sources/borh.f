C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief     TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!>              THEY ARE GIVEN BY SEGMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  MUST BE CODED BY THE USER
!>  @code
!> ---------------------------------------
!> INITIALISES THE VARIABLES (DEFAULT)
!> ---------------------------------------
!>      TETABT(:)=TETAH
!>      TETAPT(:)=0.0
!>      ALFAPT(:)=0.0
!>      RPT(:)=0.0
!>      HBT(:)=0.0
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ALFAPT ALFAPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::HBT HBT@endlink, 
!> @link DECLARATIONS_ARTEMIS::RPT RPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETABT TETABT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAH TETAH@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAPT TETAPT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AA, BID, HBCRIT, I, PI, RADDEG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>  <tr>
!>    <td><center> 6.0                                    </center></td>
!>    <td> 18/03/2010                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 21/08/2000                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALFAP
!></td><td><--</td><td>DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE
!>                  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST
!>                  POSITIF, L'ONDE REFLECHIE EST EN RETARD)
!>    </td></tr>
!>          <tr><td>C
!></td><td>--></td><td>CELERITE AU TEMPS N
!>    </td></tr>
!>          <tr><td>C,CG
!></td><td>--></td><td>VITESSES DE PHASE ET DE GROUPE
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>HB
!></td><td><--</td><td>HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES
!>    </td></tr>
!>          <tr><td>K
!></td><td>--></td><td>NOMBRE D'ONDE
!>    </td></tr>
!>          <tr><td>KENT,KLOG
!></td><td>--></td><td>CONVENTION POUR LES TYPES DE CONDITIONS AUX
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DU POINT FRONTIERE SUIVANT
!>    </td></tr>
!>          <tr><td>KSORT,KINC
!></td><td>---</td><td>LIMITES
!>                  KENT  : ENTREE (VALEUR IMPOSEE)
!>                  KLOG  : PAROI
!>                  KSORT : SORTIE
!>                  KINC  : ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE.
!>    </td></tr>
!>          <tr><td>OMEGA
!></td><td>--></td><td>PULSATION DE LA HOULE
!>    </td></tr>
!>          <tr><td>PER
!></td><td>--></td><td>PERIODE DE LA HOULE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)
!>    </td></tr>
!>          <tr><td>RP
!></td><td><--</td><td>COEFFICIENTS DE REFLEXION DES PAROIS
!>    </td></tr>
!>          <tr><td>TETAB
!></td><td><--</td><td>ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)
!>                  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE
!>                  SENS DIRECT)
!>    </td></tr>
!>          <tr><td>TETAH
!></td><td>--></td><td>ANGLE DE PROPAGATION DE LA HOULE
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td><--</td><td>ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES
!>                  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES
!>                  LES FRONTIERES LIQUIDES
!>                  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE
!>                  DANS LE SENS DIRECT)
!>    </td></tr>
!>          <tr><td>TRA01,
!></td><td><-></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XSGBOR,YSGBOR
!></td><td>--></td><td>NORMALES EXTERIEURES AUX SEGMENTS DE BORD
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BORH
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALFAP          |<--| DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE
C|                |   | REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST
C|                |   | POSITIF, L'ONDE REFLECHIE EST EN RETARD)
C| C             |-->| CELERITE AU TEMPS N
C| C,CG           |-->| VITESSES DE PHASE ET DE GROUPE
C| GRAV           |-->| GRAVITE
C| H             |-->| HAUTEUR D'EAU
C| HB             |<--| HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES
C| K             |-->| NOMBRE D'ONDE
C| KENT,KLOG      |-->| CONVENTION POUR LES TYPES DE CONDITIONS AUX
C| KP1BOR         |-->| NUMERO DU POINT FRONTIERE SUIVANT
C| KSORT,KINC     |---| LIMITES
C|                |   | KENT  : ENTREE (VALEUR IMPOSEE)
C|                |   | KLOG  : PAROI
C|                |   | KSORT : SORTIE
C|                |   | KINC  : ONDE INCIDENTE
C| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
C| OMEGA          |-->| PULSATION DE LA HOULE
C| PER            |-->| PERIODE DE LA HOULE
C| PRIVE          |-->| TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)
C| RP             |<--| COEFFICIENTS DE REFLEXION DES PAROIS
C| TETAB          |<--| ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)
C|                |   | (COMPTE PAR RAPPORT A L'AXE DES X DANS LE
C|                |   | SENS DIRECT)
C| TETAH          |-->| ANGLE DE PROPAGATION DE LA HOULE
C| TETAP          |<--| ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES
C|                |   | PAS SEULEMENT LES PAROIS, MAIS AUSSI LES
C|                |   | LES FRONTIERES LIQUIDES
C|                |   | (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE
C|                |   | DANS LE SENS DIRECT)
C| TRA01,         |<->| TABLEAUX DE TRAVAIL
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XSGBOR,YSGBOR  |-->| NORMALES EXTERIEURES AUX SEGMENTS DE BORD
C| ZF             |-->| FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
      DOUBLE PRECISION RADDEG,AA,HBCRIT
      DOUBLE PRECISION PI,BID
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C-----------------------------------------------------------------------
C
C BOUNDARY CONDITIONS
C KLOG : 'SOLID' SEGMENT.
C KINC : 'INCIDENT WAVE' SEGMENT.
C KENT : 'ENTRY' SEGMENT.
C KSORT : 'EXIT' SEGMENT.
C
C ALL THE ANGLES ARE IN  DEGREES
C                         ------
C ---------------------------------------
C INITIALISES THE VARIABLES (DEFAULT)
C ---------------------------------------
      TETABT(:)=TETAH
      TETAPT(:)=0.0
      ALFAPT(:)=0.0
      RPT(:)=0.0
      HBT(:)=0.0

      RETURN
      END
C
C#######################################################################
C