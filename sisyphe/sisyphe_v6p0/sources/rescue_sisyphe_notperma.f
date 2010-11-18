C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MISSING DATA/VARIABLES FOR HYDRODYNAMIC
!>                AND/OR SEDIMENTOLOGICAL CONTINUATION RUN.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  RESCUE_SISYPHE MUST BE MODIFIED FOR UNSTEADY SIMULATIONS
!>         TO TAKE THE BED EVOLUTIONS INTO ACCOUNT.

!>  @note  THE WATER DEPTH NEEDS TO BE COMPUTED FROM THE BED ELEVATION
!>        (SISYPHE) AND THE FREE SURFACE ELEVATION (HYDRO FILE).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALIRE, ENTET, H, HW, ICF, NPOIN, Q, QU, QV, S, THETAW, TROUVE, TW, U, V, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_RESCUE_SISYPHE_NOTPERMA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td>
!> </td><td> C.VILLARET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALIRE
!></td><td>---</td><td>TABLEAU DES VARIABLES A LIRE
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td><--</td><td>HAUTEUR.
!>    </td></tr>
!>          <tr><td>HW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KX,KY
!></td><td><--</td><td>COEFFICIENTS DE DISPERSION
!>    </td></tr>
!>          <tr><td>LISTI
!></td><td>--></td><td>LOGIQUE VRAI SI ON IMPRIME DES MESSAGES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NOMBRE DE TABLEAUX UTILISATEURS DU CAS
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS DU CAS A TRAITER
!>    </td></tr>
!>          <tr><td>Q
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td><--</td><td>SURFACE LIBRE.
!>    </td></tr>
!>          <tr><td>THETAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td>--></td><td>LOGIQUE INDIQUANT LES VARIABLES TROUVEES
!>                  DANS LE SOUS-PROGRAMME SUITE
!>    </td></tr>
!>          <tr><td>TW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><--</td><td>COTE DES POINTS DU FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                     SUBROUTINE RESCUE_SISYPHE_NOTPERMA
     &(QU,QV,Q,U,V,H,S,ZF,HW,TW,THETAW,NPOIN,TROUVE,ALIRE,ICF,ENTET)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALIRE          |---| TABLEAU DES VARIABLES A LIRE
C| ENTET          |---| 
C| H             |<--| HAUTEUR.
C| HW             |---| 
C| ICF            |---| 
C| KX,KY          |<--| COEFFICIENTS DE DISPERSION
C| LISTI          |-->| LOGIQUE VRAI SI ON IMPRIME DES MESSAGES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPRIV          |-->| NOMBRE DE TABLEAUX UTILISATEURS DU CAS
C| NTRAC          |-->| NOMBRE DE TRACEURS DU CAS A TRAITER
C| Q             |---| 
C| QU             |---| 
C| QV             |---| 
C| S             |<--| SURFACE LIBRE.
C| THETAW         |---| 
C| TROUVE         |-->| LOGIQUE INDIQUANT LES VARIABLES TROUVEES
C|                |   | DANS LE SOUS-PROGRAMME SUITE
C| TW             |---| 
C| U             |---| 
C| V             |---| 
C| ZF             |<--| COTE DES POINTS DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_RESCUE_SISYPHE_NOTPERMA
     &           => RESCUE_SISYPHE_NOTPERMA
C
      USE DECLARATIONS_SISYPHE, ONLY : MAXVAR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: TROUVE(MAXVAR),ALIRE(MAXVAR),NPOIN,ICF
C
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
      LOGICAL, INTENT(IN)             :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     THE ESSENTIAL VARIABLES NOT MODIFIED BY THE BED EVOLUTIONS
C     ARE THE FREE SURFACE AND THE FLOW
C
C     COMPUTES THE FREE SURFACE(4) FROM THE NON-MODIFIED WATER DEPTH
C     AND THE NON-MODIFIED BED ELEVATION
C
      IF(TROUVE(4).EQ.0.AND.ALIRE(4).EQ.1) THEN
        IF(TROUVE(3).EQ.1.AND.TROUVE(5).EQ.1) THEN
          CALL OV( 'X=Y+Z   ' , S , H , ZF , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,*) 'UNABLE TO BUILD FREE SURFACE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
C COMPUTES THE TELEMAC WATER DEPTH(3) FROM THE NON-MODIFIED
C BED ELEVATION AND THE FREE SURFACE
C
      IF(TROUVE(3).EQ.0.AND.ALIRE(3).EQ.1) THEN
        IF(TROUVE(5).EQ.1) THEN
          CALL OV( 'X=Y-Z   ' , H , S , ZF , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,*) 'MISSING BOTTOM OR WATER DEPTH'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
C  FLOWRATE ALONG X:  QU
C
      IF (ALIRE(7).EQ.1.AND.TROUVE(7).EQ.0)  THEN
        IF (TROUVE(1).EQ.1) THEN
          IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,150)
          IF (LNG.EQ.2) WRITE(LU,151)
150       FORMAT(1X,'DEBIT /X CALCULE AVEC LA VITESSE ET LA HAUTEUR')
151       FORMAT(1X,'DISCHARGE /X COMPUTED WITH DEPTH AND VELOCITY')
          ENDIF
          CALL OV( 'X=YZ    ' , QU , U , H  , 0.D0 , NPOIN )
        ELSE
          IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,190)
          IF (LNG.EQ.2) WRITE(LU,191)
190       FORMAT(1X,'CALCUL PRECEDENT SANS LA VITESSE U: ON PREND ZERO')
191       FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT VELOCITY U: U IS
     &               EQUAL TO ZERO')
          ENDIF
          CALL OV( 'X=C     ' , QU , U , H, 0.D0 , NPOIN )
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FLOWRATE ALONG Y :  QV
C
      IF (ALIRE(8).EQ.1.AND.TROUVE(8).EQ.0)  THEN
        IF (TROUVE(2).EQ.1) THEN
          IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,160)
          IF (LNG.EQ.2) WRITE(LU,161)
160       FORMAT(1X,'DEBIT /Y CALCULE AVEC LA VITESSE ET LA HAUTEUR')
161       FORMAT(1X,'DISCHARGE /Y COMPUTED WITH DEPTH AND VELOCITY')
          ENDIF
          CALL OV( 'X=YZ    ' , QV , V , H , 0.D0 , NPOIN )
        ELSE
          IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,210)
          IF (LNG.EQ.2) WRITE(LU,211)
210       FORMAT(1X,'CALCUL PRECEDENT SANS LA VITESSE V: ON PREND ZERO')
211       FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT VELOCITY V:
     &               FIXED TO ZERO')
          ENDIF
          CALL OV( 'X=C     ' , QV , V , H , 0.D0, NPOIN )
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FLOWRATE (M2/S)
C
      IF ((ALIRE(6).EQ.1).AND.(TROUVE(6).EQ.0))  THEN
       CALL OV( 'X=N(Y,Z)' , Q  , QU  , QV  , 0.D0 , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C  WAVE HEIGHT AND PERIOD
C
      IF ((ICF==4).OR.(ICF==5).OR.
     &    (ICF==8).OR.(ICF==9)    ) THEN
C
         IF ((ALIRE(12).EQ.1).AND.(TROUVE(12).EQ.0)) THEN
            IF(LNG.EQ.1) WRITE(LU,900)
            IF(LNG.EQ.2) WRITE(LU,901)
           CALL OV( 'X=C     ' , HW , U , V , 0.D0 , NPOIN )
         ENDIF
C
900     FORMAT(1X,'CALCUL PRECEDENT SANS LA HAUTEUR DE HOULE : ON',
     &          ' PREND ZERO')
901     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE HEIGHT : IT IS',
     &          ' FIXED TO ZERO')
C
         IF ((ALIRE(13).EQ.1).AND.(TROUVE(13).EQ.0)) THEN
            IF(LNG.EQ.1) WRITE(LU,902)
            IF(LNG.EQ.2) WRITE(LU,903)
            CALL OV( 'X=C     ' , TW , U , V , 0.D0 , NPOIN )
         ENDIF
902     FORMAT(1X,'CALCUL PRECEDENT SANS LA PERIODE DE HOULE : ON',
     &          ' PREND ZERO')
903     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE PERIOD : IT IS',
     &          ' FIXED TO ZERO')
C
         IF ((ALIRE(14).EQ.1).AND.(TROUVE(14).EQ.0)) THEN
            IF(LNG.EQ.1) WRITE(LU,902)
            IF(LNG.EQ.2) WRITE(LU,903)
            CALL OV( 'X=C     ' , THETAW , U , V , 90.D0  , NPOIN )
         ENDIF
      ENDIF
909     FORMAT(1X,'CALCUL PRECEDENT SANS ANGLE DE HOULE : ON',
     &          ' PREND ZERO')
910     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE ANGLE : IT IS',
     &          ' FIXED TO ZERO')
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE RESCUE_SISYPHE_NOTPERMA
C
C#######################################################################
C