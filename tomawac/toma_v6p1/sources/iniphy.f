C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE PARAMETERS THAT ARE TIME-INDEPENDENT
!>               (WAVE NUMBER, GROUP VELOCITY,...).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   ALL THE DIRECTIONS ARE IN RADIAN AND IN THE RANGE [0 ; 2PI].

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, CG, COSPHI, DEPTH, FREQ, NF, NPOIN2, PROINF, SPHE, XK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, AUX2, AUX3, DEUKD, DEUPI, DEUPI2, DPDSUG, IP, JF, R2, XG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> WNSCOU()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 07/02/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B(
!></td><td><--</td><td>MATRICE DE PASSAGE DE (KX,KY) A (FR,TETA)
!>    </td></tr>
!>          <tr><td>CG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CG(
!></td><td><--</td><td>MATRICE DE VITESSES DE GROUPE EN FR
!>    </td></tr>
!>          <tr><td>COSPHI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSPHI(
!></td><td>--></td><td>VECTEUR DES COSINUS DES LATITUDES
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>VECTEUR DES PROFONDEURS
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>VECTEUR DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>INDICATEUR CALCUL EN PROFONDEUR INFINIE
!>    </td></tr>
!>          <tr><td>SPHE
!></td><td>--></td><td>INDICATEUR CALCUL EN SPHERIQUE
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td><--</td><td>MATRICE DES NOMBRES D'ONDE EN FR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIPHY
     &( XK    , CG    , B     , DEPTH , FREQ  , COSPHI, NPOIN2, NF    ,
     &  PROINF, SPHE  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |---| 
C| B(             |<--| MATRICE DE PASSAGE DE (KX,KY) A (FR,TETA)
C| CG             |---| 
C| CG(            |<--| MATRICE DE VITESSES DE GROUPE EN FR
C| COSPHI         |---| 
C| COSPHI(        |-->| VECTEUR DES COSINUS DES LATITUDES
C| DEPTH          |---| 
C| DEPTH(         |-->| VECTEUR DES PROFONDEURS
C| FREQ           |---| 
C| FREQ(          |-->| VECTEUR DES FREQUENCES DE DISCRETISATION
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| PROINF         |-->| INDICATEUR CALCUL EN PROFONDEUR INFINIE
C| SPHE           |-->| INDICATEUR CALCUL EN SPHERIQUE
C| XK             |---| 
C| XK(            |<--| MATRICE DES NOMBRES D'ONDE EN FR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NF    , NPOIN2
      DOUBLE PRECISION DEPTH(NPOIN2)    , COSPHI(NPOIN2), FREQ(NF)
      DOUBLE PRECISION B(NPOIN2,NF)  , XK(NPOIN2,NF) , CG(NPOIN2,NF)
      LOGICAL          PROINF, SPHE
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          IP    , JF
      DOUBLE PRECISION DEUPI2, DEUPI , XG    , DPDSUG, AUX2
      DOUBLE PRECISION AUX1  , AUX3  , DEUKD , R2
C
C
      XG=9.81D0
      DEUPI=2.D0*3.14159265D0
      DEUPI2=DEUPI*DEUPI
      DPDSUG=DEUPI2/XG
      R2=(6400.D3)**2
C
      IF (PROINF) THEN
C                               +----------------------+
C.............................. ! INFINITE WATER DEPTH !
C                               +----------------------+
        DO 310 JF=1,NF
          AUX1=DPDSUG*(FREQ(JF))**2
          AUX3=0.5D0*XG/(DEUPI*FREQ(JF))
          DO 320 IP=1,NPOIN2
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
  320     CONTINUE
  310   CONTINUE
      ELSE
C                               +--------------------+
C.............................. ! FINITE WATER DEPTH !
C                               +--------------------+
        DO 410 JF=1,NF
          AUX2=DEUPI*FREQ(JF)
          DO 430 IP=1,NPOIN2
            CALL WNSCOU(AUX1,FREQ(JF),DEPTH(IP))
            DEUKD=2.D0*AUX1*DEPTH(IP)
            IF (DEUKD.GT.7.D2) THEN
              AUX3=0.5D0*AUX2/AUX1
            ELSE
              AUX3=0.5D0*(1.D0+DEUKD/SINH(DEUKD))*AUX2/AUX1
            ENDIF
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
  430     CONTINUE
  410   CONTINUE
      ENDIF
C
C
C.....COMPUTES B TO GO FROM (KX, KY) TO (FR, TETA)
C     ===================================================
      IF (.NOT.SPHE) THEN
C                               +-----------------------------+
C.............................. ! CARTESIAN COORDINATE SYSTEM !
C                               +-----------------------------+
        DO 710 JF=1,NF
          AUX1=DEUPI2*FREQ(JF)
          DO 720 IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF))
  720     CONTINUE
  710   CONTINUE
C
      ELSE
C                               +-----------------------------+
C.............................. ! SPHERICAL COORDINATE SYSTEM !
C                               +-----------------------------+
        DO 810 JF=1,NF
          AUX1=DEUPI2*FREQ(JF)*R2
          DO 820 IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF)*COSPHI(IP))
  820     CONTINUE
  810   CONTINUE
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C