! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Konstantwerte f&uuml;r die base library</h2>
!! @author Peter Schade
!! @version 1.4 vom 02/23/05, Quellcode: mod_b_constants.f90
!! <HR>
!! constants for the base library<BR>
!! <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
! 01.01 : 2002-01-08 : Peter Schade  : Kind-Parameter
! 01.02 : 2002-01-23 : Peter Schade  : Kommentarzeilen verbessert
! 01.03 : 2002-08-09 : Peter Schade  : Pi und Eulersche Zahl
! 01.04 : 2003-03-04 : Guenther Lang : Anpassungen TV12 vom Dez. 2002
!
!! <HR>
!!                                                                  <BR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! folgende Kind-Parameter sind enthalten:                         <BR>
!!  - f&uuml;r INTEGER: "Byte", "Short" und "Long";
!!  - f&uuml;r REAL   : "Single" und "Double".
!! mathematische Konstanten:
!!  - PI und Teile von PI
!!  - Eulersche Zahl
!!
!! <HR>
!!                                                                  <BR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt keinen Datentyp zur Verf&uuml;gung.
!! <HR>
!!                                                                  <BR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit <BR>
!!         USE b_constants, ONLY : & <BR>
!!                         Byte,  & <BR>
!!                         Long,  & <BR>
!!                         Single   <P>
!!    <LI> Kind-Parameter setzen: <BR>
!!         INTEGER (KIND=Byte)  :: i <P>
!!         Es wird empfohlen, Variablen verschiedener numerischer Pr&auml;zision <BR>
!!         als Komponenten eines komplexen Datentypen zu entwerfen. Wenn einmal <BR>
!!         die Pr&auml;zision ge&auml;ndert werden muss, ist nur die Definition des <BR>
!!         komplexen Datentypen zu modifizieren. <BR>
!!              TYPE, PUBLIC :: <datatype>    <BR>
!!                INTEGER (KIND=Byte)   :: n  <BR>
!!                REAL    (KIND=Single) :: r  <BR>
!!              END TYPE <datatype>           <P>
!! </OL>
!! <HR>
!
MODULE b_constants
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  IMPLICIT NONE
  PRIVATE
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  ! [C.2] Konstantwerte (Parameter)
  ! [C.2.1] Kind-Parameter
  !! Kind-Parameter fuer INTEGER (Byte)
  INTEGER, PUBLIC, PARAMETER :: Byte   = SELECTED_INT_KIND(2)
  !! Kind-Parameter fuer INTEGER (Short)
  INTEGER, PUBLIC, PARAMETER :: Short  = SELECTED_INT_KIND(4)
  !! Kind-Parameter fuer INTEGER (Long)
  INTEGER, PUBLIC, PARAMETER :: Long   = SELECTED_INT_KIND(9)
  !! Kind-Parameter fuer REAL (Single)
  INTEGER, PUBLIC, PARAMETER :: Single = SELECTED_REAL_KIND( 6, 30) 
  !! Kind-Parameter fuer REAL (Double)
  INTEGER, PUBLIC, PARAMETER :: Double = SELECTED_REAL_KIND(12,150) 
  ! 
  ! [C.2.2] PI und Teile von PI
  !! PI als REAL (Single) <BR>
  !! berechnet auf HP-Workstation: 4.0_Double * ATAN(1.0_Double)
  REAL (KIND=Single), PARAMETER, PUBLIC :: pi_single        = 3.141592653589793_Single
  !! PI als REAL (Double) <BR>
  !! berechnet auf HP-Workstation: 4.0_Double * ATAN(1.0_Double)
  REAL (KIND=Double), PARAMETER, PUBLIC :: pi               = 3.141592653589793_Double
  !! PI/2 als REAL (Single)
  REAL (KIND=Single), PARAMETER, PUBLIC :: pi_half_single   = 1.570796326794896_Single
  !! PI/2 als REAL (Double)
  REAL (KIND=Double), PARAMETER, PUBLIC :: pi_half          = 1.570796326794896_Double
  !! PI/4 als REAL (Single)
  REAL (KIND=Single), PARAMETER, PUBLIC :: pi_quarter_single= 0.785398163397448_Single
  !! PI/4 als REAL (Double)
  REAL (KIND=Double), PARAMETER, PUBLIC :: pi_quarter       = 0.785398163397448_Double
  !
  ! [C.2.3] Eulersche Zahl
  !! Eulersche Zahl als REAL (Single) <BR>
  !! Zahl wurde aus zwei voneinander unabhaengigen Internet-Quellen entnommen
  REAL (KIND=Single), PARAMETER, PUBLIC :: e_single  = 2.7182818284590452353_Single
  !! Eulersche Zahl als REAL (Double)
  !! Zahl wurde aus zwei voneinander unabhaengigen Internet-Quellen entnommen
  REAL (KIND=Double), PARAMETER, PUBLIC :: e         = 2.7182818284590452353_Double
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
END MODULE b_constants
! TailOfBaseModule --------------------------------------------------------
