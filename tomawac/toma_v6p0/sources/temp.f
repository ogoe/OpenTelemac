C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TIME IN SECOND BETWEEN THE DATES
!>                DAT AND DDC.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DAT, DDC, TV
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADC, ADT, HDC, HDT, JDC, JDT, MDC, MDT, MNDC, MNDT, NJDM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIVEN(), LECDOI(), LECHAM(), NOUDON(), NOUMAR()

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
!> </td><td> 01/02/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DAT
!></td><td>--></td><td>DATE D'UN ENREGISTREMENT DES VENTS
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DU DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>TV
!></td><td><--</td><td>ECART DE TEMPS EN SECONDES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TEMP
     &(TV ,DAT,DDC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DAT            |-->| DATE D'UN ENREGISTREMENT DES VENTS
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| TV             |<--| ECART DE TEMPS EN SECONDES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER ADC,MDC,JDC,HDC,MNDC,ADT,MDT,JDT,HDT,MNDT
      INTEGER NJDM(12)
      DOUBLE PRECISION TV,DDC,DAT
C
C-----------------------------------------------------------------------
C
      DATA NJDM /0,31,59,90,120,151,181,212,243,273,304,334/
C       LEAP YEARS ARE NOT TREATED !!
C
C  DECODES THE DATE OF THE BEGINNING OF COMPUTATION
C
      ADC=INT(DDC*1.D-8)
      MDC=INT(DDC*1.D-6)
      JDC=INT(DDC*1.D-4)
      HDC=INT(DDC*1.D-2)
      MNDC=INT(DDC-100.D0*HDC)
      HDC =HDC-100*JDC
      JDC =JDC-100*MDC
      MDC =MDC-100*ADC
C
C  DECODES THE DATE OF THE WIND RECORD
C
      ADT=INT(DAT*1.D-8)
      MDT=INT(DAT*1.D-6)
      JDT=INT(DAT*1.D-4)
      HDT=INT(DAT*1.D-2)
      MNDT=INT(DAT-100.D0*HDT)
      HDT =HDT-100*JDT
      JDT =JDT-100*MDT
      MDT =MDT-100*ADT
C
      TV=((((ADT-ADC)*365+(JDT+NJDM(MDT)-JDC-NJDM(MDC)))*24 +
     &     HDT-HDC)*60 + MNDT-MNDC)*60
C
      RETURN
      END
C
C#######################################################################
C