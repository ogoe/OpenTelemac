C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE ARRAY OF THE ELEVATIONS OF THE MESH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HH, ZZ
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRANSF TRANSF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRANSF_PLANE TRANSF_PLANE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZPLANE ZPLANE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZSTAR ZSTAR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISBOT, DISMIN_BOT, DISMIN_SUR, DISSUR, I1, I2, IPLAN, IPOIN, RPLI, RPLS, ZFP, ZSP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIM(), MESH_PROP(), TELEMAC3D()

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
!> </td><td> 11/03/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54; J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COTINT
!></td><td>--></td><td>COTE DU PLAN INTERMEDIAIRE DE REFERENCE
!>    </td></tr>
!>          <tr><td>HH
!></td><td>--></td><td>HAUTEURS D'EAU
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>TRANSF
!></td><td>--></td><td>CHOICE OF MESH TRANSFORMATION
!>    </td></tr>
!>          <tr><td>ZZ
!></td><td><--</td><td>COTES DES POINTS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCOT
     &(ZZ,HH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COTINT         |-->| COTE DU PLAN INTERMEDIAIRE DE REFERENCE
C| HH             |-->| HAUTEURS D'EAU
C| HMIN           |-->| 
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| TRANSF         |-->| CHOICE OF MESH TRANSFORMATION
C| ZZ             |<--| COTES DES POINTS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, INTENT(IN)    :: HH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZZ(NPOIN2,NPLAN)
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION RPLS,RPLI,ZFP,ZSP,DISBOT,DISSUR
      DOUBLE PRECISION DISMIN_BOT,DISMIN_SUR
      INTEGER IPOIN,IPLAN,I1,I2
!
!***********************************************************************
!
      DISMIN_SUR = 0.2D0
      DISMIN_BOT = 0.2D0
!
C     1) IN ALL CASES: FREE SURFACE = BOTTOM+DEPTH
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + MAX(HH(IPOIN),0.D0)
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + HH(IPOIN)
        ENDDO
      ENDIF
!
C     HERE IMPLEMENTATION BY USER
!
      IF(TRANSF.EQ.0) THEN
!
        IF(LNG.EQ.1) WRITE(LU,81)
        IF(LNG.EQ.2) WRITE(LU,82)
81      FORMAT('CALCOT: TRANSFORMATION A PROGRAMMER PAR L''UTILISATEUR')
82      FORMAT('CALCOT: TRANSFORMATION TO BE PROGRAMMED BY USER')
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
C     NOW ALL OTHER CASES: SEQUENCES OF SIGMA TRANSFORMATIONS
C                          AND PLANES WITH PRESCRIBED ELEVATION
!
      ELSEIF(NPLAN.GT.2) THEN
!
!-----------------------------------------------------------------------
!
C       2) SETS THE PLANES WITH PRESCRIBED ELEVATION
!
        DO IPLAN=2,NPLAN-1
          IF(TRANSF_PLANE%I(IPLAN).EQ.3) THEN
C           IF NOT POSSIBLE BECAUSE OF FREE SURFACE OR BOTTOM, A SECURITY
C           DISTANCE, DISMIN, IS USED. ALL PLANES THAT WOULD CROSS E.G.
C           THE BOTTOM AVOID IT AT A DISTANCE DISMIN*RPLI, SEE RPLI BELOW
            RPLS = DBLE(NPLAN-IPLAN) / DBLE(NPLAN)
            RPLI = DBLE(IPLAN-    1) / DBLE(NPLAN)
            DO IPOIN = 1,NPOIN2
              ZFP = ZZ(IPOIN,1)
              ZSP = ZZ(IPOIN,NPLAN)
              DISBOT = MIN(ZSP-ZFP,DISMIN_BOT)
              DISSUR = MIN(ZSP-ZFP,DISMIN_SUR)
              ZZ(IPOIN,IPLAN)=MIN(                    ZSP-DISSUR*RPLS,
     &                            MAX(ZPLANE%R(IPLAN),ZFP+DISBOT*RPLI))
            ENDDO
          ENDIF
        ENDDO
!
C       3) SETS THE PLANES WITH SIGMA TRANSFORMATION
!
        I1=2
        DO WHILE(I1.NE.NPLAN)
          IF(TRANSF_PLANE%I(I1).EQ.3) THEN
            I1=I1+1
          ELSE
C           LOOKS FOR SEQUENCES OF SIGMA TRANSFORMATION PLANES
            I2=I1
            DO WHILE(TRANSF_PLANE%I(I2+1).NE.3.AND.I2+1.NE.NPLAN)
              I2=I2+1
            ENDDO
C           SIGMA TRANSFORMATION FOR PLANES I1 TO I2
C           BETWEEN ALREADY TREATED PLANES I1-1 AND I2+1
            DO IPLAN=I1,I2
              IF(TRANSF_PLANE%I(IPLAN).EQ.1) THEN
                ZSTAR%R(IPLAN)=FLOAT(IPLAN-I1+1)/FLOAT(I2-I1+2)
C             ELSE
C               ZSTAR%R(IPLAN) HAS BEEN GIVEN BY USER IN CONDIM
              ENDIF
              DO IPOIN = 1,NPOIN2
                ZZ(IPOIN,IPLAN) = ZZ(IPOIN,I1-1)
     &                          + ZSTAR%R(IPLAN)*(  ZZ(IPOIN,I2+1)
     &                                             -ZZ(IPOIN,I1-1) )
              ENDDO
            ENDDO
            I1=I2+1
          ENDIF
        ENDDO
!
C       4) CHECKS
!
        IF(NPLAN.GT.2) THEN
          DO IPLAN=2,NPLAN-1
            DO IPOIN = 1,NPOIN2
              IF(ZZ(IPOIN,IPLAN).LT.ZZ(IPOIN,IPLAN-1)) THEN
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'CALCOT : LES PLANS ',IPLAN-1,' ET ',IPLAN
                  WRITE(LU,*) '         SE CROISENT AU POINT ',IPOIN
                  WRITE(LU,*) '         COTE BASSE : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         COTE HAUTE : ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '         DIFFERENCE : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         HAUTEUR    : ',HH(IPOIN)
                ENDIF
                 IF(LNG.EQ.2) THEN
                  WRITE(LU,*) 'CALCOT: PLANES ',IPLAN-1,' AND ',IPLAN
                  WRITE(LU,*) '        INTERCROSS AT POINT ',IPOIN
                  WRITE(LU,*) '        LOWER POINT : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        HIGHER POINT: ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '        DIFFERENCE  : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        DEPTH       : ',HH(IPOIN)
                ENDIF
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C