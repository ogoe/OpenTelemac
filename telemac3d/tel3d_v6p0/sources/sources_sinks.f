C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE SOURCE TERMS TO ADD IN 2D AND 3D
!>                CONTINUITY EQUATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::IELM3 IELM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ISCE ISCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KSCE KSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSCE NSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PLUIE PLUIE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::QSCE2 QSCE2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAIN RAIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAIN_MMPD RAIN_MMPD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SMH SMH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SOURCES SOURCES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_02 T3_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_03 T3_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLU2D VOLU2D@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IP, IS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 07/04/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
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
                        SUBROUTINE SOURCES_SINKS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,IP
!
!-----------------------------------------------------------------------
!
C     SOURCES AND SINKS
!
      IF(NSCE.GT.0) THEN
!
C       HERE T3_02 LIKE VOLU, BUT CALL PARCOM (AND ZPROP INSTEAD OF Z)
        CALL VECTOR(T3_02,'=','MASBAS          ',IELM3,1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &              MESH3D,.FALSE.,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T3_02,2,MESH3D)
        CALL CPSTVC(T3_02,T3_03)
        DO IS=1,NSCE
          CALL OS('X=0     ',X=T3_03)
C         IN PARALLEL IF ISCE(IS)=0, THE POINT IS OUTSIDE THE SUBDOMAIN
          IF(ISCE(IS).GT.0) THEN
            I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
            T3_03%R(I)=QSCE2(IS)/MAX(1.D-8,T3_02%R(I))
          ENDIF
          CALL VECTOR(SOURCES%ADR(IS)%P,'=','MASVEC          ',
     &                IELM3,1.D0,T3_03,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH3D,MSK,MASKEL)
        ENDDO
C       SUMS ON THE VERTICAL TO GET THE 2D SOURCES
        DO IS=1,NSCE
          DO IP=1,NPLAN
            DO I=1,NPOIN2
              SMH%R(I)=SMH%R(I)+SOURCES%ADR(IS)%P%R(I+NPOIN2*(IP-1))
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     RAIN AND EVAPORATION (NEGATIVE RAIN)
!
      IF(RAIN) THEN
C       PLUIE IS NON ASSEMBLED IN PARALLEL
        CALL OS('X=CY    ',X=PLUIE,Y=VOLU2D,C=RAIN_MMPD/86400000.D0)
        CALL OS('X=X+Y   ',X=SMH,Y=PLUIE)
      ENDIF
!
!-----------------------------------------------------------------------
!
C     PARALLELISM, REAL VALUES REQUIRED IN SOURCES FOR MURD3D
C     BUT BEWARE IN TRIDW2, PARCOM MUST NOT BE DONE TWICE ON SOURCES
!
C     12/06/2007 : VALUES WITHOUT PARCOM STORED IN ADDRESS IS+NSCE
C                  SIZE CHANGED ACCORDINGLY IN POINT_TELEMAC3D
!
      IF(NCSIZE.GT.1) THEN
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            CALL OS('X=Y     ',X=SOURCES%ADR(IS+NSCE)%P,
     &                         Y=SOURCES%ADR(IS     )%P)
            CALL PARCOM(SOURCES%ADR(IS)%P,2,MESH3D)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C