C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MISSING DATA/VARIABLES FOR HYDRODYNAMIC
!>                AND/OR SEDIMENTOLOGICAL CONTINUATION RUN.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALIRE, H, HW, ICF, LISTI, NPOIN, PASS, Q, QU, QV, S, THETAW, TROUVE, TW, U, V, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_RESCUE_SISYPHE
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
!>      <td><center> 6.0                                       </center>
!> </td><td>
!> </td><td> C. LENORMANT
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
!>          <tr><td>PASS
!></td><td>--></td><td>LOGIQUE VRAI SI ON EST EN DEBUT DE CALCUL
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
                        SUBROUTINE RESCUE_SISYPHE
     &(QU,QV,Q,U,V,H,S,ZF,HW,TW,THETAW,NPOIN,TROUVE,ALIRE,PASS,
     & ICF,LISTI,MAXVAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALIRE          |---| TABLEAU DES VARIABLES A LIRE
C| H              |<--| HAUTEUR.
C| HW             |---| 
C| ICF            |---| 
C| KX,KY          |<--| COEFFICIENTS DE DISPERSION
C| LISTI          |-->| LOGIQUE VRAI SI ON IMPRIME DES MESSAGES
C| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPRIV          |-->| NOMBRE DE TABLEAUX UTILISATEURS DU CAS
C| NTRAC          |-->| NOMBRE DE TRACEURS DU CAS A TRAITER
C| PASS           |-->| LOGIQUE VRAI SI ON EST EN DEBUT DE CALCUL
C| Q              |---| 
C| QU             |---| 
C| QV             |---| 
C| S              |<--| SURFACE LIBRE.
C| THETAW         |---| 
C| TROUVE         |-->| LOGIQUE INDIQUANT LES VARIABLES TROUVEES
C|                |   | DANS LE SOUS-PROGRAMME SUITE
C| TW             |---| 
C| U              |---| 
C| V              |---| 
C| ZF             |<--| COTE DES POINTS DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      USE INTERFACE_SISYPHE, EX_RESCUE_SISYPHE
     &           => RESCUE_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: MAXVAR
      INTEGER, INTENT(IN) :: TROUVE(MAXVAR),ALIRE(MAXVAR),NPOIN,ICF
      LOGICAL, INTENT(IN) :: PASS,LISTI
C
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
C-----------------------------------------------------------------------
C
C PRINTOUTS :
C -----------
      IF(PASS.AND.LISTI) THEN
        WRITE(LU,200)
200     FORMAT(80('-'))
        IF(ALIRE(8).EQ.1) THEN
          IF(LNG.EQ.1) WRITE(LU,300)
          IF(LNG.EQ.2) WRITE(LU,301)
300       FORMAT(1X,'RESCUE : FICHIER HYDRODYNAMIQUE')
301       FORMAT(1X,'RESCUE : HYDRODYNAMIC FILE')
        ELSE
          IF(LNG.EQ.1) WRITE(LU,310)
          IF(LNG.EQ.2) WRITE(LU,311)
310       FORMAT(1X,'RESCUE : FICHIER SEDIMENTOLOGIQUE')
311       FORMAT(1X,'RESCUE : SEDIMENTOLOGICAL FILE')
        ENDIF
      ENDIF
C
C ------------------------------------------------------------------
C  WATER DEPTH :
C  -------------
      IF((ALIRE(3).EQ.1).AND.(TROUVE(3).NE.1)) THEN
        IF(TROUVE(4).EQ.1.AND.TROUVE(5).EQ.1) THEN
          IF (LISTI) THEN
            IF(LNG.EQ.1) WRITE(LU,400)
            IF(LNG.EQ.2) WRITE(LU,401)
          ENDIF
          CALL OV( 'X=Y-Z   ' , H , S , ZF , 0.D0 , NPOIN )
        ELSE
          IF (LISTI) THEN
            IF(LNG.EQ.1) WRITE(LU,420)
            IF(LNG.EQ.2) WRITE(LU,421)
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
400       FORMAT(1X,'HAUTEUR D''EAU CALCULEE AVEC LE FOND',
     &         /,1X,'ET LA SURFACE LIBRE')
401       FORMAT(1X,'WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,' AND SURFACE ELEVATION')
420       FORMAT(1X,'IMPOSSIBLE DE CALCULER LA HAUTEUR D''EAU')
421       FORMAT(1X,'WATER DEPTH UNABLE TO BE COMPUTED')
C
C ----------------------------------------------------------------------
C
C CLIPS NEGATIVE WATER DEPTHS :
C -------------------------------------
C
      DO K = 1,NPOIN
        H(K) = MAX(H(K),0.D0)
      ENDDO
C
C------------------------------------------------------------------------
C
C  WAVE HEIGHT AND PERIOD
C
      IF(ICF==4.OR.ICF==5.OR.ICF==8.OR.ICF==9) THEN
C
        IF(ALIRE(12).EQ.1.AND.TROUVE(12).EQ.0) THEN
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
         IF(ALIRE(13).EQ.1.AND.TROUVE(13).EQ.0) THEN
           IF(LNG.EQ.1) WRITE(LU,902)
           IF(LNG.EQ.2) WRITE(LU,903)
           CALL OV( 'X=C     ' , TW , U , V , 0.D0 , NPOIN )
         ENDIF
902     FORMAT(1X,'CALCUL PRECEDENT SANS LA PERIODE DE HOULE : ON',
     &          ' PREND ZERO')
903     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE PERIOD : IT IS',
     &          ' FIXED TO ZERO')
C
         IF(ALIRE(14).EQ.1.AND.TROUVE(14).EQ.0) THEN
           IF(LNG.EQ.1) WRITE(LU,902)
           IF(LNG.EQ.2) WRITE(LU,903)
           CALL OV( 'X=C     ' , THETAW , U , V , 90.D0  , NPOIN )
         ENDIF
      ENDIF
909   FORMAT(1X,'CALCUL PRECEDENT SANS ANGLE DE HOULE : ON',
     &          ' PREND ZERO')
910   FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE ANGLE : IT IS',
     &          ' FIXED TO ZERO')
C
C-----------------------------------------------------------------------
C  NON-ERODABLE BED
C
      IF(ALIRE(9).EQ.1.AND.TROUVE(9).EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,907)
        IF(LNG.EQ.2) WRITE(LU,908)
      ENDIF
907   FORMAT(1X,'CALCUL PRECEDENT SANS FOND NON ERODABLE')
908   FORMAT(1X,'PREVIOUS CALCULATION WITHOUT NON ERODABLE',
     &         /,1X,'BOTTOM')
C
C-----------------------------------------------------------------------
C  BED ELEVATION
C
      IF(ALIRE(5).EQ.1.AND.TROUVE(5).EQ.0) THEN
C
        IF(TROUVE(4).EQ.1.AND.TROUVE(3).EQ.1) THEN
          IF (LISTI) THEN
          IF(LNG.EQ.1) WRITE(LU,410)
          IF(LNG.EQ.2) WRITE(LU,411)
410       FORMAT(1X,'FOND CALCULE AVEC LA HAUTEUR D''EAU',
     &         /,1X,'ET LA SURFACE LIBRE')
411       FORMAT(1X,'BATHYMETRY COMPUTED FROM WATER DEPTH',
     &         /,1X,'AND SURFACE ELEVATION')
          ENDIF
          CALL OV( 'X=Y-Z   ' , ZF , S , H , 0.D0 , NPOIN )
        ELSE
          CALL  OV( 'X=C     ' , ZF , ZF, ZF, 0.D0 , NPOIN )
          IF(LNG.EQ.1) WRITE(LU,960)
          IF(LNG.EQ.2) WRITE(LU,961)
        ENDIF
960     FORMAT(1X,'COTE DU FOND NON TROUVEE',/,
     &            'LA COTE EST INITIALISEE A ZERO')
961     FORMAT(1X,'BOTTOM TOPOGRAPHY NOT FOUND',/,
     &            'IT IS SET TO ZERO')
C
      ENDIF
C
      IF (PASS.AND.LISTI) THEN
        WRITE(LU,970)
970     FORMAT(80('-'))
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE RESCUE_SISYPHE
C
C#######################################################################
C
