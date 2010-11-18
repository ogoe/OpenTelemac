C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ADVECTION FIELD (3D WITHOUT CURRENT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!>            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!>            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CG, COSF, COSTET, CT, CX, CY, DEPTH, DZX, DZY, FREQ, JF, NF, NPLAN, NPOIN2, PROINF, PROMIN, SINTET, SPHE, TGF, TRA01, TRA02, XK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DDDN, DEUKD, DEUPI, DSDNSK, GRADEG, GSQP, IP, JP, R, SR, SRCF, TFSR
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PREPRO()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> 19/01/2004
!> </td><td> M. BENOIT (EDF LNHE) 01 30 87 83 51
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CG
!></td><td>--></td><td>VITESSE DE GROUPE DISCRETISEE
!>    </td></tr>
!>          <tr><td>COSF
!></td><td>--></td><td>COSINUS DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>--></td><td>COSINUS TETA
!>    </td></tr>
!>          <tr><td>CX,CY,CT
!></td><td><--</td><td>CHAMP CONVECTEUR SELON X(OU PHI),
!>                  Y(OU LAMBDA) ET TETA
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR
!>    </td></tr>
!>          <tr><td>DZX
!></td><td>--></td><td>GRADIENT DE FOND SELON X
!>    </td></tr>
!>          <tr><td>DZY
!></td><td>--></td><td>GRADIENT DE FOND SELON Y
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>JF
!></td><td>--></td><td>FREQUENCES COURANTE
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS OU DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
!>    </td></tr>
!>          <tr><td>PROMIN
!></td><td>--></td><td>VALEUR MINIMALE DE LA PROFONDEUR D'EAU
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>--></td><td>SINUS TETA
!>    </td></tr>
!>          <tr><td>SPHE
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
!>    </td></tr>
!>          <tr><td>TGF
!></td><td>--></td><td>TANGENTES DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>XK
!></td><td>--></td><td>NOMBRE D'ONDE DISCRETISE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONWAC
     &( CX    , CY    , CT    , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZX   , DZY   , FREQ  , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE  , PROMIN, TRA01 , TRA02 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CG             |-->| VITESSE DE GROUPE DISCRETISEE
C| COSF           |-->| COSINUS DES LATITUDES DES POINTS 2D
C| COSTET         |-->| COSINUS TETA
C| CX,CY,CT       |<--| CHAMP CONVECTEUR SELON X(OU PHI),
C|                |   | Y(OU LAMBDA) ET TETA
C| DEPTH          |-->| PROFONDEUR
C| DZX            |-->| GRADIENT DE FOND SELON X
C| DZY            |-->| GRADIENT DE FOND SELON Y
C| FREQ           |-->| FREQUENCES DISCRETISEES
C| JF             |-->| FREQUENCES COURANTE
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| PROINF         |-->| LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
C| PROMIN         |-->| VALEUR MINIMALE DE LA PROFONDEUR D'EAU
C| SINTET         |-->| SINUS TETA
C| SPHE           |-->| LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
C| TGF            |-->| TANGENTES DES LATITUDES DES POINTS 2D
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| TRA02          |<->| TABLEAU DE TRAVAIL
C| XK             |-->| NOMBRE D'ONDE DISCRETISE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER          NF    , NPLAN , NPOIN2, JF
      DOUBLE PRECISION PROMIN
      DOUBLE PRECISION DEPTH(NPOIN2) , DZX(NPOIN2)   , DZY(NPOIN2)
      DOUBLE PRECISION COSF(NPOIN2)  , TGF(NPOIN2)   , FREQ(NF)
      DOUBLE PRECISION COSTET(NPLAN) , SINTET(NPLAN)
      DOUBLE PRECISION TRA01(NPLAN)  , TRA02(NPLAN)
      DOUBLE PRECISION CG(NPOIN2,NF) , XK(NPOIN2,NF)
      DOUBLE PRECISION CX(NPOIN2,NPLAN),CY(NPOIN2,NPLAN)
      DOUBLE PRECISION CT(NPOIN2,NPLAN)
      LOGICAL          PROINF, SPHE
C
C.....LOCAL VARIABLES
C     """"""""""""""""""
      INTEGER          JP    , IP
      DOUBLE PRECISION GSQP  , SR    , R     , SRCF  , TFSR
      DOUBLE PRECISION DDDN  , DSDNSK, GRADEG, DEUKD , DEUPI
C
C
      GSQP=0.780654996D0
      R=6400.D3
      DEUPI=6.283185307D0
C
      IF (PROINF) THEN
C-----------------------------------------------------------------------
C     INFINITE WATER DEPTH ...
C-----------------------------------------------------------------------
C
        DO JP=1,NPLAN
          TRA01(JP)=GSQP/FREQ(JF)*COSTET(JP)
          TRA02(JP)=GSQP/FREQ(JF)*SINTET(JP)
        ENDDO
C
        IF (.NOT.SPHE) THEN
C       ----------------------------------------------------------------
C       ... AND IN CARTESIAN COORDINATE SYSTEM
C       ----------------------------------------------------------------
          DO IP=1,NPOIN2
            DO JP=1,NPLAN
              CX(IP,JP)=TRA01(JP)
              CY(IP,JP)=TRA02(JP)
              CT(IP,JP)=0.D0
            ENDDO
          ENDDO
C
        ELSE
C       ----------------------------------------------------------------
C       ... AND IN SPHERICAL COORDINATE SYSTEM
C       ----------------------------------------------------------------
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IP=1,NPOIN2
            SRCF=SR/COSF(IP)
            TFSR=TGF(IP)*SR
            DO JP=1,NPLAN
              CX(IP,JP)=TRA01(JP)*SR*GRADEG
              CY(IP,JP)=TRA02(JP)*SRCF*GRADEG
              CT(IP,JP)=TRA02(JP)*TFSR
            ENDDO
          ENDDO
C
        ENDIF
C
C
      ELSE
C-----------------------------------------------------------------------
C     FINITE WATER DEPTH ....
C-----------------------------------------------------------------------
C
        IF (.NOT.SPHE) THEN
C       ----------------------------------------------------------------
C       ... AND IN CARTESIAN COORDINATE SYSTEM
C       ----------------------------------------------------------------
          DO IP=1,NPOIN2
            IF (DEPTH(IP).GT.PROMIN) THEN
              DO JP=1,NPLAN
                DDDN=-SINTET(JP)*DZX(IP)+COSTET(JP)*DZY(IP)
                CX(IP,JP)=CG(IP,JF)*COSTET(JP)
                CY(IP,JP)=CG(IP,JF)*SINTET(JP)
                DEUKD=2.0D0*XK(IP,JF)*DEPTH(IP)
                IF (DEUKD.GT.7.0D2) THEN
                  DSDNSK=0.0D0
                ELSE
                  DSDNSK=DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                CT(IP,JP)=-DSDNSK*DDDN
              ENDDO
            ELSE
              DO JP=1,NPLAN
                CX(IP,JP)=0.0D0
                CY(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDDO
            ENDIF
          ENDDO
C
        ELSE
C       ----------------------------------------------------------------
C       ... AND IN SPHERICAL COORDINATE SYSTEM
C       ----------------------------------------------------------------
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IP=1,NPOIN2
            IF (DEPTH(IP).GT.PROMIN) THEN
              SRCF=SR/COSF(IP)
              TFSR=TGF(IP)*SR
              DO JP=1,NPLAN
                DDDN=-SINTET(JP)*DZX(IP)*SR+COSTET(JP)*DZY(IP)*SRCF
                CX(IP,JP)=(CG(IP,JF)*COSTET(JP))*SR*GRADEG
                CY(IP,JP)=(CG(IP,JF)*SINTET(JP))*SRCF*GRADEG
                DEUKD=2.D0*XK(IP,JF)*DEPTH(IP)
                IF (DEUKD.GT.7.D2) THEN
                  DSDNSK=0.D0
                ELSE
                  DSDNSK=DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                CT(IP,JP)=CG(IP,JF)*SINTET(JP)*TFSR-DSDNSK*DDDN*GRADEG
              ENDDO
            ELSE
              DO JP=1,NPLAN
                CX(IP,JP)=0.0D0
                CY(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDDO
            ENDIF
          ENDDO
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C