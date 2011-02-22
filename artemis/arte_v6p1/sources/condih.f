C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE PHYSICAL PARAMETER ARRAYS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CDTINI CDTINI@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::COTINI COTINI@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HAUTIN HAUTIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::OMEGA OMEGA@endlink, 
!> @link DECLARATIONS_ARTEMIS::PER PER@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, COTE, DHTEST, I, PI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLIP(), INCLUS(), MAJUS(), OS(), PLANTE()
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
!>    <td><center> 5.1                                    </center></td>
!>    <td> 02/06/1999                                              </td>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIH
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      DOUBLE PRECISION COTE
      DOUBLE PRECISION PI,BID,DHTEST
      PARAMETER( PI = 3.1415926535897932384626433D0 )
C
      INTRINSIC SINH, SQRT
C
C-----------------------------------------------------------------------
C
      CALL MAJUS(CDTINI)
C
C-----------------------------------------------------------------------
C
C   INITIALISES THE WATER DEPTH H
C
      IF(INCLUS(CDTINI,'COTE NULLE').OR.
     &   INCLUS(CDTINI,'ZERO ELEVATION') ) THEN
        COTE = 0.D0
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'COTE CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT ELEVATION') ) THEN
        COTE = COTINI
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR NULLE').OR.
     &       INCLUS(CDTINI,'ZERO DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , 0.D0 )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , HAUTIN )
      ELSEIF(INCLUS(CDTINI,'PARTICULIERES').OR.
     &       INCLUS(CDTINI,'SPECIAL')        ) THEN
C  TO BE MODIFIED BY USER
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIH : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIH')
11      FORMAT(1X,'CONDIH : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIH')
        CALL PLANTE(0)
        STOP
C  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) CDTINI
        IF(LNG.EQ.2) WRITE(LU,21) CDTINI
20      FORMAT(1X,'CONDIH : CONDITION INITIALE INCONNUE :',/,A72)
21      FORMAT(1X,'CONDIH : UNKNOWN INITIAL CONDITION :',/,A72)
        CALL PLANTE(0)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CLIPS H (VALUES LOWER THAN 1.D-2 NOT ALLOWED)
C
      CALL CLIP(H,1.D-2,.TRUE.,1.D6,.FALSE.,NPOIN)
C
C-----------------------------------------------------------------------
C
C   COMPUTES THE WAVE NUMBER: K
C   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
C   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
C


      OMEGA = 2.D0*PI/PER
      CALL OS('X=CY    ', T1 , H , SBID , OMEGA**2/GRAV )
C
C     INITIALISES DHTEST
C
      DHTEST = 1.D6
C
      DO 100 I=1,NPOIN
         T2%R(I) = 1.D0 + T1%R(I) *( 0.6522D0 +
     &                    T1%R(I) *( 0.4622D0 +
     &                    T1%R(I) *
     &                    T1%R(I) *( 0.0864D0 +
     &                    T1%R(I) *( 0.0675D0 ) )))
         T2%R(I) = SQRT( T1%R(I)*(T1%R(I) + 1.D0/T2%R(I)) )
         K%R(I)  = T2%R(I)/H%R(I)
         DHTEST  = MIN( DHTEST , H%R(I) )
100   CONTINUE
C
C   CHECKS WHETHER H HAS BEEN CLIPPED OR NOT
C
      IF (DHTEST.LE.1.01D-2) THEN
         IF(LNG.EQ.1) WRITE(LU,120)
         IF(LNG.EQ.2) WRITE(LU,121)
120      FORMAT(1X,'CONDIH : ATTENTION !! VOUS AVEZ ATTEINT LE SEUIL '
     &          ,/,'         MINI DE HAUTEUR D''EAU (1 CM).'
     &          ,/,'         VERIFIEZ BATHY OU CONDITIONS INITIALES')
121      FORMAT(1X,'CONDIH : WARNING !! YOU REACHED MINIMUM THRESHOLD'
     &          ,/,'         FOR WATER DEPTH (1 CM). CHECK THE'
     &          ,/,'         BATHYMETRY OR INITIAL CONDITIONS')
      ENDIF
C
C-----------------------------------------------------------------------
C
C   COMPUTES PHASE VELOCITY
C
      CALL OS('X=CY    ', T1    , K     , SBID , 1.D0/OMEGA )
      CALL OS('X=1/Y   ', C     , T1    , SBID , BID        )
C
C-----------------------------------------------------------------------
C
C   COMPUTES GROUP VELOCITY
C
      DO 200 I=1,NPOIN
         CG%R(I) = C%R(I)/2.D0 *
     &             (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
200   CONTINUE
C


C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C